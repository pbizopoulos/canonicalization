{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy         #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Main (main) where
import           Control.Monad             (void)
import           Data.Fix                  (Fix (Fix))
import           Data.Function             (on)
import           Data.Functor.Compose      (Compose (Compose))
import           Data.List                 (groupBy, sortBy)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Data.Ord                  (comparing)
import           Data.Text                 (Text, empty, isPrefixOf, pack,
                                            stripStart)
import qualified Data.Text.IO              as TIO
import           Nix.Expr.Types            (Antiquoted (Plain),
                                            Binding (NamedVar),
                                            NExprF (NAbs, NLet, NList, NSet),
                                            NKeyName (DynamicKey, StaticKey),
                                            NString (DoubleQuoted),
                                            Params (ParamSet),
                                            Recursivity (NonRecursive),
                                            VarName (VarName))
import           Nix.Expr.Types.Annotated  (AnnUnit (AnnUnit), NExprLoc,
                                            SrcSpan (SrcSpan), stripAnnotation)
import           Nix.Parser                (parseNixFileLoc)
import           Nix.Pretty                (prettyNix)
import           Nix.Utils                 (Path (Path))
import           Prelude                   (Bool, Either (Left, Right),
                                            Eq ((==)), FilePath, IO,
                                            Maybe (Just), Show (show), String,
                                            any, concatMap, fmap, fst, map,
                                            mapM_, putStrLn, ($), (++), (.),
                                            (||))
import           Prettyprinter             (LayoutOptions (LayoutOptions),
                                            PageWidth (AvailablePerLine),
                                            layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)
import           System.Environment        (getArgs, lookupEnv)
import           System.IO                 (hClose)
import           System.IO.Temp            (withSystemTempFile)
import           Test.HUnit                (Test (TestCase, TestList),
                                            assertEqual, assertFailure,
                                            runTestTT)
main :: IO ()
main = do
  args <- getArgs
  debug <- lookupEnv "DEBUG"
  if debug == Just "1"
    then void $ runTestTT getAllFormattingTests
    else
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
  let sortedExpr = sortExpression expr
      finalText =
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 1 1.0)) $
            prettyNix $
              stripAnnotation sortedExpr
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
    NAbs params body ->
      let sortedParams = case params of
            ParamSet atPattern variadic paramList ->
              ParamSet atPattern variadic (sortBy (comparing fst) paramList)
            _ -> params
       in NAbs sortedParams (sortExpression body)
    NList items ->
      let sortedItems = map sortExpression items
       in if any isStringExpr sortedItems
            then NList sortedItems
            else NList $ sortBy (comparing renderExpressionText) sortedItems
    NSet rec bindings ->
      NSet rec $ sortAndCollapseBindings bindings
    NLet bindings body ->
      NLet
        (sortAndCollapseBindings bindings)
        (sortExpression body)
    otherExpr -> fmap sortExpression otherExpr
getBindingName :: Binding r -> Text
getBindingName (NamedVar (StaticKey (VarName keyText) :| _) _ _) = keyText
getBindingName (NamedVar (DynamicKey (Plain (DoubleQuoted [Plain keyText])) :| _) _ _) = keyText
getBindingName _ = empty
sortAndCollapseBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
sortAndCollapseBindings = sortAndCollapseBindingsUnchecked
sortAndCollapseBindingsUnchecked :: [Binding NExprLoc] -> [Binding NExprLoc]
sortAndCollapseBindingsUnchecked =
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
       in case sortedNested of
            [] -> map (fmap sortExpression) bindings
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
getAllFormattingTests :: Test
getAllFormattingTests =
  TestList
    [ makeFormattingTest
        "list sorting (non-string)"
        (pack "[ 3 1 2 ]")
        (pack "[\n  1\n  2\n  3\n]"),
      makeFormattingTest
        "list preservation (strings)"
        (pack "[ \"c\" \"a\" \"b\" ]")
        (pack "[\n  \"c\"\n  \"a\"\n  \"b\"\n]"),
      makeFormattingTest
        "parameter sorting"
        (pack "{ x = { z, x, y }: x + y + z; }")
        (pack "{\n  x = { x\n    , y\n    , z }:\n    x + y + z;\n}"),
      makeFormattingTest
        "attribute set sorting"
        (pack "{ c = 1; a = 2; b = 3; }")
        (pack "{\n  a = 2;\n  b = 3;\n  c = 1;\n}"),
      makeFormattingTest
        "nested attribute set sorting"
        (pack "{ b = { z = 1; x = 2; }; a = 1; }")
        (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      makeFormattingTest
        "dotted list collapse"
        (pack "{ a = { b = [ \"c\" ]; }; }")
        (pack "{\n  a.b = [\n    \"c\"\n  ];\n}"),
      makeFormattingTest
        "dotted nested collapse"
        (pack "{ b = { z = 1; }; a = 1; }")
        (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      makeFormattingTest
        "dotted attribute preservation"
        (pack "{ b.z = 1; a = 1; }")
        (pack "{\n  a = 1;\n  b.z = 1;\n}"),
      makeFormattingTest
        "dotted to nested conversion"
        (pack "{ b.z = 1; b.x = 2; a = 1; }")
        (pack "{\n  a = 1;\n  b = {\n    x = 2;\n    z = 1;\n  };\n}"),
      makeFormattingTest
        "multi-dotted to nested conversion"
        (pack "{ b.z.b = 1; b.z.a = 2; }")
        (pack "{\n  b.z = {\n    a = 2;\n    b = 1;\n  };\n}"),
      makeFormattingTest
        "let expression sorting"
        (pack "let c = 1; a = 2; b = 3; in a + b + c")
        (pack "let\n  a = 2;\n  b = 3;\n  c = 1;\nin a + b + c"),
      makeFormattingTest
        "let with nested set sorting"
        (pack "let c = { z = 1; x = 2; }; a = 1; in a + c.x + c.z")
        (pack "let\n  a = 1;\n  c = {\n    x = 2;\n    z = 1;\n  };\nin a + c.x + c.z"),
      makeFormattingTest
        "deep nested collapse"
        (pack "{ c = { z = { x = 2; }; }; }")
        (pack "{\n  c.z.x = 2;\n}"),
      makeFormattingTest
        "string key sorting"
        (pack "{ \"b\".val1 = 1; \"a\".val2 = 2; }")
        (pack "{\n  \"a\".val2 = 2;\n  \"b\".val1 = 1;\n}"),
      makeFormattingTest
        "multiline string preservation"
        (pack "{ a = ''\n  line1\n  line2\n''; }")
        (pack "{\n  a = ''\n    line1\n    line2\n    '';\n}"),
      makeFormattingTest
        "python template installPhase preservation"
        (pack "{ installPhase = ''\nmkdir -p $out/bin\ncp ./main.py $out/bin/${pname}\n''; }")
        (pack "{\n  installPhase = ''\n    mkdir -p $out/bin\n    cp ./main.py $out/bin/${pname}\n    '';\n}")
    ]
