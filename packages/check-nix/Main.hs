{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Control.Monad (unless, void)
import Data.Fix (Fix (Fix))
import Data.Function (on)
import Data.Functor (Functor (fmap))
import Data.Functor.Compose (Compose (Compose))
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (comparing)
import Data.Text (Text, empty, isInfixOf, pack)
import Data.Text.IO (readFile, writeFile)
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
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.HUnit (Test (TestCase, TestList), assertEqual, assertFailure, runTestTT)
import Prelude
  ( Either (Left, Right),
    Eq ((==)),
    FilePath,
    IO,
    Show (show),
    String,
    concatMap,
    fst,
    map,
    mapM_,
    null,
    putStrLn,
    ($),
    (++),
    (.),
    (||),
  )

noCheckTag :: Text
noCheckTag = pack "# no-check"

main :: IO ()
main = do
  args <- getArgs
  if null args || args == ["--test"]
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
  fileContent <- readFile filePath
  unless (noCheckTag `isInfixOf` fileContent) $ do
    let sortedExpr = sortExpression expr
        outputText =
          renderStrict $
            layoutPretty defaultLayoutOptions $
              prettyNix $
                stripAnnotation sortedExpr
    writeFile filePath outputText

renderExpressionText :: NExprLoc -> Text
renderExpressionText =
  renderStrict . layoutPretty defaultLayoutOptions . prettyNix . stripAnnotation

sortExpression :: NExprLoc -> NExprLoc
sortExpression (Fix (Compose (AnnUnit span exprF))) =
  Fix . Compose . AnnUnit span $ case exprF of
    NAbs params body ->
      let sortedParams = case params of
            ParamSet atPattern variadic paramList ->
              ParamSet atPattern variadic (sortBy (comparing fst) paramList)
            _ -> params
       in NAbs sortedParams (sortExpression body)
    NList items ->
      NList $ sortBy (comparing renderExpressionText) (map sortExpression items)
    NSet rec bindings ->
      NSet rec $ sortAndCollapseBindings bindings
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
nextLevelBindings (NamedVar (_ :| []) (Fix (Compose (AnnUnit _ (NSet _ nested)))) _) =
  nested
nextLevelBindings _ = []

makeFormattingTest :: String -> Text -> Text -> Test
makeFormattingTest testName input expectedOutput = TestCase $ do
  withSystemTempFile "test.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expr -> do
        writeFormattedFile tmpFile expr
        formatted <- readFile tmpFile
        assertEqual testName expectedOutput formatted
      Left parseError ->
        assertFailure $ "Parse error in test '" ++ testName ++ "': " ++ show parseError

getAllFormattingTests :: Test
getAllFormattingTests =
  TestList
    [ makeFormattingTest
        "list sorting"
        (pack "[ \"c\" \"a\" \"b\" ]")
        (pack "[ \"a\" \"b\" \"c\" ]"),
      makeFormattingTest
        "parameter sorting"
        (pack "{ x = { z, x, y }: x + y + z; }")
        (pack "{ x = { x, y, z }: x + y + z; }"),
      makeFormattingTest
        "attribute set sorting"
        (pack "{ c = 1; a = 2; b = 3; }")
        (pack "{ a = 2; b = 3; c = 1; }"),
      makeFormattingTest
        "nested attribute set sorting"
        (pack "{ b = { z = 1; x = 2; }; a = 1; }")
        (pack "{ a = 1; b = { x = 2; z = 1; }; }"),
      makeFormattingTest
        "dotted list collapse"
        (pack "{ a = { b = [ \"c\" ]; }; }")
        (pack "{ a.b = [ \"c\" ]; }"),
      makeFormattingTest
        "dotted nested collapse"
        (pack "{ b = { z = 1; }; a = 1; }")
        (pack "{ a = 1; b.z = 1; }"),
      makeFormattingTest
        "dotted attribute preservation"
        (pack "{ b.z = 1; a = 1; }")
        (pack "{ a = 1; b.z = 1; }"),
      makeFormattingTest
        "dotted to nested conversion"
        (pack "{ b.z = 1; b.x = 2; a = 1; }")
        (pack "{ a = 1; b = { x = 2; z = 1; }; }"),
      makeFormattingTest
        "multi-dotted to nested conversion"
        (pack "{ b.z.b = 1; b.z.a = 2; }")
        (pack "{ b.z = { a = 2; b = 1; }; }"),
      makeFormattingTest
        "no-check comment"
        (pack "# no-check\n{ a = [ \"c\" \"a\" ]; }")
        (pack "# no-check\n{ a = [ \"c\" \"a\" ]; }"),
      makeFormattingTest
        "let expression sorting"
        (pack "let c = 1; a = 2; b = 3; in a + b + c")
        (pack "let   a = 2; b = 3; c = 1; in a + b + c"),
      makeFormattingTest
        "let with nested set sorting"
        (pack "let c = { z = 1; x = 2; }; a = 1; in a + c.x + c.z")
        (pack "let   a = 1; c = { x = 2; z = 1; }; in a + c.x + c.z"),
      makeFormattingTest
        "deep nested collapse"
        (pack "{ c = { z = { x = 2; }; }; }")
        (pack "{ c.z.x = 2; }"),
      makeFormattingTest
        "string key sorting"
        (pack "{ \"b\".val1 = 1; \"a\".val2 = 2; }")
        (pack "{ \"a\".val2 = 2; \"b\".val1 = 1; }")
    ]
