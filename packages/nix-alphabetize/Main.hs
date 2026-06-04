{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Data.Fix (Fix (Fix))
import Data.Function (on)
import Data.Functor.Compose (Compose (Compose))
import Data.List (groupBy, sort, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (comparing)
import Data.Text
  ( Text,
    empty,
    isPrefixOf,
    pack,
    stripStart,
    unpack,
  )
import Data.Text.IO qualified as TIO
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (NamedVar),
    NExprF (NAbs, NLet, NList, NSet),
    NKeyName (DynamicKey, StaticKey),
    NString (DoubleQuoted),
    Params (ParamSet),
    Recursivity (NonRecursive),
    VarName (VarName),
  )
import Nix.Expr.Types.Annotated
  ( AnnUnit (AnnUnit),
    NExprLoc,
    SrcSpan (SrcSpan),
    stripAnnotation,
  )
import Nix.Parser (parseNixFileLoc)
import Nix.Pretty (prettyNix)
import Nix.Utils (Path (Path))
import Prettyprinter
  ( LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine),
    layoutPretty,
  )
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase, TestList),
    assertEqual,
    assertFailure,
    runTestTT,
  )
import Test.QuickCheck qualified as QC
import Prelude
  ( Bool (False, True),
    Either (Left, Right),
    Eq ((==)),
    FilePath,
    IO,
    Int,
    Show (show),
    String,
    all,
    any,
    concatMap,
    elem,
    fmap,
    fst,
    map,
    mapM_,
    notElem,
    otherwise,
    pure,
    putStrLn,
    reverse,
    sequence,
    unwords,
    zip,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )
main :: IO ()
main = do
  args <- getArgs
  mapM_
    ( \filePath -> do
        parseResult <- parseNixFileLoc (Path filePath)
        case parseResult of
          Left parseError -> putStrLn ("Error parsing " ++ filePath ++ ": " ++ show parseError)
          Right expr -> writeFormattedFile filePath expr
    )
    args
writeFormattedFile :: FilePath -> NExprLoc -> IO ()
writeFormattedFile filePath expr = do
  let finalText =
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 1 1.0)) $
            prettyNix $
              stripAnnotation (sortExpression expr)
  TIO.writeFile filePath finalText
renderExpressionText :: NExprLoc -> Text
renderExpressionText =
  renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 1 1.0)) . prettyNix . stripAnnotation
isStringExpr :: NExprLoc -> Bool
isStringExpr expr =
  let rendered = stripStart (renderExpressionText expr)
   in pack "\"" `isPrefixOf` rendered || pack "''" `isPrefixOf` rendered
sortExpression :: NExprLoc -> NExprLoc
sortExpression (Fix (Compose (AnnUnit exprSpan exprF))) =
  Fix . Compose . AnnUnit exprSpan $ case exprF of
    NAbs (ParamSet atPattern variadic paramList) body ->
      NAbs (ParamSet atPattern variadic (sortBy (comparing fst) paramList)) (sortExpression body)
    NAbs params body ->
      NAbs params (sortExpression body)
    NList items ->
      let sortedItems = map sortExpression items
       in NList $
            if any isStringExpr sortedItems
              then sortedItems
              else sortBy (comparing renderExpressionText) sortedItems
    NSet rec bindings ->
      NSet rec (sortAndCollapseBindings bindings)
    NLet bindings body ->
      NLet (sortAndCollapseBindings bindings) (sortExpression body)
    otherExpr -> fmap sortExpression otherExpr
getBindingName :: Binding r -> Text
getBindingName (NamedVar (StaticKey (VarName keyText) :| _) _ _) = keyText
getBindingName (NamedVar (DynamicKey (Plain (DoubleQuoted [Plain keyText])) :| _) _ _) = keyText
getBindingName _ = empty
sortAndCollapseBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
sortAndCollapseBindings =
  concatMap collapseNestedBindings
    . groupBy ((==) `on` getBindingName)
    . sortBy (comparing getBindingName)
collapseNestedBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
collapseNestedBindings [] = []
collapseNestedBindings bindings@(firstBinding : _) =
  case firstBinding of
    NamedVar (bindingKey :| _) _ bindingPos ->
      let nestedBindings = concatMap nextLevelBindings bindings
          sortedNested = sortAndCollapseBindings nestedBindings
          sortedBindings = map (fmap sortExpression) bindings
       in case sortedNested of
            [] -> sortedBindings
            [NamedVar (subKey :| restKeys) valExpr _] ->
              [NamedVar (bindingKey :| subKey : restKeys) valExpr bindingPos]
            newNested ->
              [ NamedVar
                  (bindingKey :| [])
                  (Fix (Compose (AnnUnit (SrcSpan bindingPos bindingPos) (NSet NonRecursive newNested))))
                  bindingPos
              ]
    _ -> map (fmap sortExpression) bindings
nextLevelBindings :: Binding NExprLoc -> [Binding NExprLoc]
nextLevelBindings (NamedVar (_ :| bindingKey : restKeys) valExpr bindingPos) =
  [NamedVar (bindingKey :| restKeys) valExpr bindingPos]
nextLevelBindings (NamedVar (_ :| []) (Fix (Compose (AnnUnit _ (NSet _ nested)))) _) = nested
nextLevelBindings _ = []
makeFormattingTest :: String -> Text -> Text -> Test
makeFormattingTest testName input expectedOutput = TestCase $ do
  withSystemTempFile "test.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    TIO.writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expr -> do
        writeFormattedFile tmpFile expr
        formatted <- TIO.readFile tmpFile
        assertEqual testName expectedOutput formatted
      Left parseError ->
        assertFailure $ "Parse error in test '" ++ testName ++ "': " ++ show parseError
formatText :: Text -> IO Text
formatText input =
  withSystemTempFile "property.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    TIO.writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expr -> do
        writeFormattedFile tmpFile expr
        TIO.readFile tmpFile
      Left parseError ->
        assertFailure ("Property fixture failed to parse: " ++ show parseError)
runPackageTests :: IO ()
runPackageTests = do
  counts <- runTestTT getAllFormattingTests
  propertySuccess <- quickCheckFormattingProperties
  if errors counts == 0 && failures counts == 0 && propertySuccess
    then putStrLn "test ... ok"
    else exitFailure
quickCheckFormattingProperties :: IO Bool
quickCheckFormattingProperties = do
  sortedListResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_nonStringListsAreSorted)
  stringOrderResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_stringListsPreserveOrder)
  attrSetResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_attrSetsCanonicalizeByKeyOrder)
  dottedCollapseResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_dottedAssignmentsCollapseToCanonicalNestedSets)
  pure (all isQuickCheckSuccess [sortedListResult, stringOrderResult, attrSetResult, dottedCollapseResult])
isQuickCheckSuccess :: QC.Result -> Bool
isQuickCheckSuccess QC.Success {} = True
isQuickCheckSuccess _ = False
prop_nonStringListsAreSorted :: QC.Property
prop_nonStringListsAreSorted =
  QC.forAll digitListGen $ \values ->
    QC.ioProperty $ do
      formatted <- formatText (renderNumberListInput values)
      sortedFormatted <- formatText (renderNumberListInput (sort values))
      pure (formatted == sortedFormatted)
prop_stringListsPreserveOrder :: QC.Property
prop_stringListsPreserveOrder =
  QC.forAll simpleStringListGen $ \values ->
    QC.ioProperty $ do
      formatted <- formatText (renderStringListInput values)
      pure (extractQuotedStrings formatted == values)
prop_attrSetsCanonicalizeByKeyOrder :: QC.Property
prop_attrSetsCanonicalizeByKeyOrder =
  QC.forAll attrBindingListGen $ \bindings ->
    QC.ioProperty $ do
      formatted <- formatText (renderAttrSetInput bindings)
      sortedFormatted <- formatText (renderAttrSetInput (sortBindings bindings))
      pure (formatted == sortedFormatted)
prop_dottedAssignmentsCollapseToCanonicalNestedSets :: QC.Property
prop_dottedAssignmentsCollapseToCanonicalNestedSets =
  QC.forAll nestedBindingSetGen $ \(rootKey, bindings) ->
    QC.ioProperty $ do
      dottedFormatted <- formatText (renderDottedAttrSetInput rootKey bindings)
      nestedFormatted <- formatText (renderNestedAttrSetInput rootKey bindings)
      pure (dottedFormatted == nestedFormatted)
digitListGen :: QC.Gen [Int]
digitListGen = QC.listOf1 (QC.chooseInt (0, 9))
simpleStringGen :: QC.Gen String
simpleStringGen =
  QC.listOf1 (QC.elements (['a' .. 'z'] ++ ['0' .. '9']))
simpleStringListGen :: QC.Gen [String]
simpleStringListGen =
  fmap dedupePreservingOrder (QC.listOf1 simpleStringGen)
attrBindingListGen :: QC.Gen [(String, Int)]
attrBindingListGen = do
  keys <- fmap dedupePreservingOrder (QC.listOf1 attrKeyGen)
  values <- sequence [QC.chooseInt (0, 9) | _ <- keys]
  pure (zip keys values)
nestedBindingSetGen :: QC.Gen (String, [(String, Int)])
nestedBindingSetGen = do
  rootKey <- attrKeyGen
  bindings <- attrBindingListGen
  pure (rootKey, bindings)
attrKeyGen :: QC.Gen String
attrKeyGen =
  QC.suchThat
    ( do
        firstCharacter <- QC.elements ['a' .. 'z']
        restCharacters <- QC.listOf (QC.elements (['a' .. 'z'] ++ ['0' .. '9']))
        pure (firstCharacter : restCharacters)
    )
    (`notElem` nixReservedKeywords)
nixReservedKeywords :: [String]
nixReservedKeywords =
  [ "assert",
    "else",
    "if",
    "in",
    "inherit",
    "let",
    "or",
    "rec",
    "then",
    "with"
  ]
dedupePreservingOrder :: [String] -> [String]
dedupePreservingOrder = go []
  where
    go :: [String] -> [String] -> [String]
    go _seen [] = []
    go seen (value : rest)
      | value `elem` seen = go seen rest
      | otherwise = value : go (seen ++ [value]) rest
renderNumberListInput :: [Int] -> Text
renderNumberListInput values =
  pack ("[ " ++ unwords (map show values) ++ " ]")
renderStringListInput :: [String] -> Text
renderStringListInput values =
  pack ("[ " ++ unwords (map show values) ++ " ]")
renderAttrSetInput :: [(String, Int)] -> Text
renderAttrSetInput bindings =
  pack ("{ " ++ unwords (map renderBinding bindings) ++ " }")
  where
    renderBinding :: (String, Int) -> String
    renderBinding (name, value) = name ++ " = " ++ show value ++ ";"
renderDottedAttrSetInput :: String -> [(String, Int)] -> Text
renderDottedAttrSetInput rootKey bindings =
  pack ("{ " ++ unwords (map renderBinding bindings) ++ " }")
  where
    renderBinding :: (String, Int) -> String
    renderBinding (name, value) = rootKey ++ "." ++ name ++ " = " ++ show value ++ ";"
renderNestedAttrSetInput :: String -> [(String, Int)] -> Text
renderNestedAttrSetInput rootKey bindings =
  pack ("{ " ++ rootKey ++ " = { " ++ unwords (map renderBinding bindings) ++ " }; }")
  where
    renderBinding :: (String, Int) -> String
    renderBinding (name, value) = name ++ " = " ++ show value ++ ";"
sortBindings :: [(String, Int)] -> [(String, Int)]
sortBindings = sortBy (comparing fst)
extractQuotedStrings :: Text -> [String]
extractQuotedStrings = go [] [] False . unpack
  where
    go :: [String] -> String -> Bool -> String -> [String]
    go collected _currentToken _insideQuotes [] = reverse collected
    go collected currentToken insideQuotes (character : rest)
      | character == '"' =
          if insideQuotes
            then go (reverse currentToken : collected) [] False rest
            else go collected [] True rest
      | insideQuotes = go collected (character : currentToken) True rest
      | otherwise = go collected currentToken False rest
getAllFormattingTests :: Test
getAllFormattingTests =
  TestList
    [ makeFormattingTest
        "Sorts non-string lists."
        (pack "[ 3 1 2 ]")
        (pack "[\n  1\n  2\n  3\n]"),
      makeFormattingTest
        "Preserves string-list order."
        (pack "[ \"c\" \"a\" \"b\" ]")
        (pack "[\n  \"c\"\n  \"a\"\n  \"b\"\n]"),
      makeFormattingTest
        "Sorts function parameters."
        (pack "{ x = { z, x, y }: x + y + z; }")
        (pack "{\n  x = { x\n    , y\n    , z }:\n    x + y + z;\n}"),
      makeFormattingTest
        "Sorts attribute sets."
        (pack "{ c = 1; a = 2; b = 3; }")
        (pack "{\n  a = 2;\n  b = 3;\n  c = 1;\n}"),
      makeFormattingTest
        "Sorts nested attribute sets."
        (pack "{ b = { z = 1; x = 2; }; a = 1; }")
        (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      makeFormattingTest
        "Collapses dotted list assignments."
        (pack "{ a = { b = [ \"c\" ]; }; }")
        (pack "{\n  a.b = [\n    \"c\"\n  ];\n}"),
      makeFormattingTest
        "Collapses dotted nested assignments."
        (pack "{ b = { z = 1; }; a = 1; }")
        (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      makeFormattingTest
        "Preserves dotted attribute assignments."
        (pack "{ b.z = 1; a = 1; }")
        (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      makeFormattingTest
        "Converts dotted assignments to nested sets."
        (pack "{ b.z = 1; b.x = 2; a = 1; }")
        (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      makeFormattingTest
        "Converts multi-dotted assignments to nested sets."
        (pack "{ b.z.b = 1; b.z.a = 2; }")
        (pack "{\n  b.z = {\n    a = 2;\n    b = 1;\n  };\n}"),
      makeFormattingTest
        "Sorts let expressions."
        (pack "let c = 1; a = 2; b = 3; in a + b + c")
        (pack "let\n  a = 2;\n  b = 3;\n  c = 1;\nin a + b + c"),
      makeFormattingTest
        "Sorts let expressions with nested sets."
        (pack "let c = { z = 1; x = 2; }; a = 1; in a + c.x + c.z")
        (pack "let\n  a = 1;\n  c = {\n    x = 2;\n    z = 1;\n  };\nin a + c.x + c.z"),
      makeFormattingTest
        "Collapses deep nested assignments."
        (pack "{ c = { z = { x = 2; }; }; }")
        (pack "{\n  c.z.x = 2;\n}"),
      makeFormattingTest
        "Sorts string-key assignments."
        (pack "{ \"b\".val1 = 1; \"a\".val2 = 2; }")
        (pack "{\n  \"a\".val2 = 2;\n  \"b\".val1 = 1;\n}"),
      makeFormattingTest
        "Preserves multiline string formatting."
        (pack "{ a = ''\n  line1\n  line2\n''; }")
        (pack "{\n  a = ''\n    line1\n    line2\n    '';\n}"),
      makeFormattingTest
        "Preserves python template installPhase formatting."
        (pack "{ installPhase = ''\nmkdir -p $out/bin\ncp ./main.py $out/bin/${pname}\n''; }")
        (pack "{\n  installPhase = ''\n    mkdir -p $out/bin\n    cp ./main.py $out/bin/${pname}\n    '';\n}")
    ]
