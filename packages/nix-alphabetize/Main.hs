{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Main (main, runPackageTests, runPackageTestsWithTimings) where
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Fix (Fix (Fix))
import Data.Function (on)
import Data.Functor.Compose (Compose (Compose))
import Data.List (groupBy, isPrefixOf, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (NamedVar),
    NExprF (NAbs, NLet, NList, NSet, NStr),
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
import Numeric (showFFloat)
import Prettyprinter
  ( LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine),
    layoutPretty,
  )
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    assertFailure,
    runTestTT,
  )
import Test.QuickCheck qualified as QC
import Prelude
  ( Bool (False, True),
    Double,
    Either (Left, Right),
    Eq ((==)),
    FilePath,
    IO,
    Int,
    Maybe (Just, Nothing),
    Show (show),
    String,
    all,
    and,
    any,
    appendFile,
    concatMap,
    drop,
    fmap,
    fromIntegral,
    fst,
    map,
    mapM,
    maybe,
    not,
    notElem,
    null,
    otherwise,
    pure,
    putStrLn,
    reverse,
    sequence,
    unwords,
    writeFile,
    zip,
    zipWith,
    ($),
    (&&),
    (++),
    (-),
    (.),
    (/),
  )
main :: IO ()
main = do
  arguments <- getArgs
  successes <- mapM formatNixFile arguments
  unless (and successes) exitFailure
formatNixFile :: FilePath -> IO Bool
formatNixFile filePath = do
  parseResult <- parseNixFileLoc (Path filePath)
  case parseResult of
    Left parseError -> do
      putStrLn ("Error parsing " ++ filePath ++ ": " ++ show parseError)
      pure False
    Right expression -> do
      TIO.writeFile filePath (formattedExpression expression)
      pure True
renderExpressionText :: NExprLoc -> Text
renderExpressionText =
  renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 1 1.0)) . prettyNix . stripAnnotation
isStringExpr :: NExprLoc -> Bool
isStringExpr (Fix (Compose (AnnUnit _ (NStr _)))) = True
isStringExpr _ = False
sortExpression :: NExprLoc -> NExprLoc
sortExpression (Fix (Compose (AnnUnit expressionSpan expressionF))) =
  Fix . Compose . AnnUnit expressionSpan $ case expressionF of
    NAbs (ParamSet atPattern variadic paramList) body ->
      NAbs (ParamSet atPattern variadic (sortOn fst paramList)) (sortExpression body)
    NAbs params body ->
      NAbs params (sortExpression body)
    NList items ->
      let sortedItems = map sortExpression items
       in NList $
            if any isStringExpr sortedItems
              then sortedItems
              else sortOn renderExpressionText sortedItems
    NSet recursivity bindings ->
      NSet recursivity (sortAndCollapseBindings bindings)
    NLet bindings body ->
      NLet (sortAndCollapseBindings bindings) (sortExpression body)
    otherExpr -> fmap sortExpression otherExpr
bindingName :: Binding r -> Maybe Text
bindingName = fmap NE.head . bindingPath
bindingPath :: Binding r -> Maybe (NonEmpty Text)
bindingPath (NamedVar bindingKeys _ _) = mapM keyNameText bindingKeys
bindingPath _ = Nothing
keyNameText :: NKeyName r -> Maybe Text
keyNameText (StaticKey (VarName keyText)) = Just keyText
keyNameText (DynamicKey (Plain (DoubleQuoted [Plain keyText]))) = Just keyText
keyNameText _ = Nothing
sortAndCollapseBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
sortAndCollapseBindings =
  concatMap collapseNestedBindings
    . groupBy ((==) `on` bindingName)
    . sortOn bindingName
collapseNestedBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
collapseNestedBindings [] = []
collapseNestedBindings bindings@(firstBinding : _) =
  case firstBinding of
    NamedVar (bindingKey :| _) _ bindingPos
      | bindingsAreStructurallyCompatible bindings ->
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
bindingsAreStructurallyCompatible :: [Binding NExprLoc] -> Bool
bindingsAreStructurallyCompatible [] = False
bindingsAreStructurallyCompatible [binding] = not (null (nextLevelBindings binding))
bindingsAreStructurallyCompatible bindings =
  all isDottedBinding bindings
    && maybe False (pathsAreUnambiguous . map NE.toList) (mapM bindingPath bindings)
isDottedBinding :: Binding r -> Bool
isDottedBinding (NamedVar (_ :| _ : _) _ _) = True
isDottedBinding _ = False
pathsAreUnambiguous :: [[Text]] -> Bool
pathsAreUnambiguous paths =
  let sortedPaths = sort paths
   in and (zipWith (\path nextPath -> not (path `isPrefixOf` nextPath)) sortedPaths (drop 1 sortedPaths))
nextLevelBindings :: Binding NExprLoc -> [Binding NExprLoc]
nextLevelBindings (NamedVar (_ :| bindingKey : restKeys) valExpr bindingPos) =
  [NamedVar (bindingKey :| restKeys) valExpr bindingPos]
nextLevelBindings (NamedVar (_ :| []) (Fix (Compose (AnnUnit _ (NSet NonRecursive nested)))) _) = nested
nextLevelBindings _ = []
makeFormattingTest :: Text -> Text -> Test
makeFormattingTest input expectedOutput = TestCase $ do
  actualOutput <- formatText input
  assertEqual "formatted output" expectedOutput actualOutput
formatText :: Text -> IO Text
formatText input =
  withSystemTempFile "test.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    TIO.writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expression ->
        pure (formattedExpression expression)
      Left parseError ->
        assertFailure ("Formatting fixture failed to parse: " ++ show parseError)
formattedExpression :: NExprLoc -> Text
formattedExpression = renderExpressionText . sortExpression
runPackageTests :: IO ()
runPackageTests = runPackageTestsWith Nothing
runPackageTestsWithTimings :: FilePath -> IO ()
runPackageTestsWithTimings timingsPath = do
  writeFile timingsPath ""
  runPackageTestsWith (Just timingsPath)
runPackageTestsWith :: Maybe FilePath -> IO ()
runPackageTestsWith maybeTimingsPath = do
  let packageTests = maybe hUnitPackageTests (`timeHUnitTests` hUnitPackageTests) maybeTimingsPath
  counts <- runTestTT packageTests
  propertySuccess <- quickCheckFormattingProperties maybeTimingsPath
  if errors counts == 0 && failures counts == 0 && propertySuccess
    then putStrLn "test ... ok"
    else exitFailure
quickCheckFormattingProperties :: Maybe FilePath -> IO Bool
quickCheckFormattingProperties maybeTimingsPath = do
  sortedListResult <- timedQuickCheck "Non-string lists are sorted." (QC.quickCheckResult (QC.withMaxSuccess 100 prop_nonStringListsAreSorted))
  stringOrderResult <- timedQuickCheck "String lists preserve order." (QC.quickCheckResult (QC.withMaxSuccess 100 prop_stringListsPreserveOrder))
  attrSetResult <- timedQuickCheck "Attribute sets canonicalize by key order." (QC.quickCheckResult (QC.withMaxSuccess 100 prop_attributeSetsCanonicalizeByKeyOrder))
  dottedCollapseResult <- timedQuickCheck "Dotted assignments collapse to canonical nested sets." (QC.quickCheckResult (QC.withMaxSuccess 100 prop_dottedAssignmentsCollapseToCanonicalNestedSets))
  pure (all isQuickCheckSuccess [sortedListResult, stringOrderResult, attrSetResult, dottedCollapseResult])
  where
    timedQuickCheck :: String -> IO QC.Result -> IO QC.Result
    timedQuickCheck testName testAction = maybe testAction (\timingsPath -> timeTestAction timingsPath testName testAction) maybeTimingsPath
timeHUnitTests :: FilePath -> Test -> Test
timeHUnitTests timingsPath (TestLabel testName (TestCase testAction)) = TestLabel testName (TestCase (timeTestAction timingsPath testName testAction))
timeHUnitTests timingsPath (TestLabel testName nestedTest) = TestLabel testName (timeHUnitTests timingsPath nestedTest)
timeHUnitTests timingsPath (TestList nestedTests) = TestList (map (timeHUnitTests timingsPath) nestedTests)
timeHUnitTests _ testCase = testCase
timeTestAction :: FilePath -> String -> IO a -> IO a
timeTestAction timingsPath testName testAction = do
  startedAt <- getMonotonicTimeNSec
  testAction
    `finally` do
      finishedAt <- getMonotonicTimeNSec
      let elapsedSeconds = fromIntegral (finishedAt - startedAt) / 1000000000 :: Double
      appendFile timingsPath ("test\t" ++ showFFloat (Just 6) elapsedSeconds "" ++ "\t" ++ testName ++ "\n")
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
prop_attributeSetsCanonicalizeByKeyOrder :: QC.Property
prop_attributeSetsCanonicalizeByKeyOrder =
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
  fmap nub (QC.listOf1 simpleStringGen)
attrBindingListGen :: QC.Gen [(String, Int)]
attrBindingListGen = do
  keys <- fmap nub (QC.listOf1 attrKeyGen)
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
sortBindings = sortOn fst
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
hUnitPackageTests :: Test
hUnitPackageTests =
  TestList
    [ TestLabel "Preserves string-list order." $
        makeFormattingTest
          (pack "[ \"c\" \"a\" \"b\" ]")
          (pack "[\n  \"c\"\n  \"a\"\n  \"b\"\n]"),
      TestLabel "Sorts function parameters." $
        makeFormattingTest
          (pack "{ x = { z, x, y }: x + y + z; }")
          (pack "{\n  x = { x\n    , y\n    , z }:\n    x + y + z;\n}"),
      TestLabel "Sorts nested attribute sets." $
        makeFormattingTest
          (pack "{ b = { z = 1; x = 2; }; a = 1; }")
          (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      TestLabel "Preserves recursive nested attribute sets." $
        makeFormattingTest
          (pack "{ a = rec { y = x; x = 1; }; }")
          (pack "{\n  a = rec {\n    x = 1;\n    y = x;\n  };\n}"),
      TestLabel "Collapses dotted list assignments." $
        makeFormattingTest
          (pack "{ a = { b = [ \"c\" ]; }; }")
          (pack "{\n  a.b = [\n    \"c\"\n  ];\n}"),
      TestLabel "Collapses dotted nested assignments." $
        makeFormattingTest
          (pack "{ b = { z = 1; }; a = 1; }")
          (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      TestLabel "Preserves dotted attribute assignments." $
        makeFormattingTest
          (pack "{ b.z = 1; a = 1; }")
          (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      TestLabel "Converts dotted assignments to nested sets." $
        makeFormattingTest
          (pack "{ b.z = 1; b.x = 2; a = 1; }")
          (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      TestLabel "Converts multi-dotted assignments to nested sets." $
        makeFormattingTest
          (pack "{ b.z.b = 1; b.z.a = 2; }")
          (pack "{\n  b.z = {\n    a = 2;\n    b = 1;\n  };\n}"),
      TestLabel "Sorts let expressions." $
        makeFormattingTest
          (pack "let c = 1; a = 2; b = 3; in a + b + c")
          (pack "let\n  a = 2;\n  b = 3;\n  c = 1;\nin a + b + c"),
      TestLabel "Sorts let expressions with nested sets." $
        makeFormattingTest
          (pack "let c = { z = 1; x = 2; }; a = 1; in a + c.x + c.z")
          (pack "let\n  a = 1;\n  c = {\n    x = 2;\n    z = 1;\n  };\nin a + c.x + c.z"),
      TestLabel "Collapses deep nested assignments." $
        makeFormattingTest
          (pack "{ c = { z = { x = 2; }; }; }")
          (pack "{\n  c.z.x = 2;\n}"),
      TestLabel "Sorts string-key assignments." $
        makeFormattingTest
          (pack "{ \"b\".val1 = 1; \"a\".val2 = 2; }")
          (pack "{\n  \"a\".val2 = 2;\n  \"b\".val1 = 1;\n}"),
      TestLabel "Preserves inherited and ambiguous dynamic bindings." $
        makeFormattingTest
          (pack "{ inherit a; ${name}.x = 1; ${other}.y = 2; }")
          (pack "{\n  inherit a;\n  ${name}.x = 1;\n  ${other}.y = 2;\n}"),
      TestLabel "Preserves conflicting scalar and dotted bindings." $
        makeFormattingTest
          (pack "{ a = 1; a.b = 2; }")
          (pack "{\n  a = 1;\n  a.b = 2;\n}"),
      TestLabel "Preserves duplicate dotted bindings." $
        makeFormattingTest
          (pack "{ a.b = 1; a.b = 2; }")
          (pack "{\n  a.b = 1;\n  a.b = 2;\n}"),
      TestLabel "Preserves prefix-conflicting dotted bindings." $
        makeFormattingTest
          (pack "{ a.b = 1; a.b.c = 2; }")
          (pack "{\n  a.b = 1;\n  a.b.c = 2;\n}"),
      TestLabel "Reports parse failure without rewriting the file." $
        TestCase $
          withSystemTempFile "malformed.nix" $ \tmpFile tmpHandle -> do
            hClose tmpHandle
            let malformed = pack "{ invalid = ; }"
            TIO.writeFile tmpFile malformed
            succeeded <- formatNixFile tmpFile
            contents <- TIO.readFile tmpFile
            assertEqual "parse status" False succeeded
            assertEqual "unchanged contents" malformed contents,
      TestLabel "Preserves multiline string formatting." $
        makeFormattingTest
          (pack "{ a = ''\n  line1\n  line2\n''; }")
          (pack "{\n  a = ''\n    line1\n    line2\n    '';\n}"),
      TestLabel "Preserves Python template installPhase formatting." $
        makeFormattingTest
          (pack "{ installPhase = ''\nmkdir -p $out/bin\ncp ./main.py $out/bin/${pname}\n''; }")
          (pack "{\n  installPhase = ''\n    mkdir -p $out/bin\n    cp ./main.py $out/bin/${pname}\n    '';\n}"),
      TestLabel "The installed executable formats multiple files." $
        TestCase $
          withSystemTempDirectory "nix-alphabetize-e2e" $ \temporaryDirectory -> do
            maybeExecutable <- lookupEnv "PACKAGE_E2E_EXECUTABLE"
            case maybeExecutable of
              Nothing -> pure ()
              Just executable -> do
                let attributeSetPath = temporaryDirectory ++ "/attributes.nix"
                    listPath = temporaryDirectory ++ "/list.nix"
                TIO.writeFile attributeSetPath (pack "{ c = 1; a = 2; b = 3; }")
                TIO.writeFile listPath (pack "[ 3 1 2 ]")
                (commandExit, commandStdout, commandStderr) <-
                  readProcessWithExitCode executable [attributeSetPath, listPath] ""
                assertEqual "installed command exit" ExitSuccess commandExit
                assertEqual "installed command stdout" "" commandStdout
                assertEqual "installed command stderr" "" commandStderr
                attributeSetContents <- TIO.readFile attributeSetPath
                listContents <- TIO.readFile listPath
                assertEqual
                  "installed command attribute-set output"
                  (pack "{\n  a = 2;\n  b = 3;\n  c = 1;\n}")
                  attributeSetContents
                assertEqual
                  "installed command list output"
                  (pack "[\n  1\n  2\n  3\n]")
                  listContents
    ]
