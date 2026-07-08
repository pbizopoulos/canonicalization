{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Control.Monad (unless, when)
import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), eitherDecodeStrict', encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Fix (Fix (Fix))
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Nix.Atoms (NAtom (NBool, NFloat, NInt, NNull))
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (NamedVar),
    NExprF (NAbs, NConstant, NLet, NList, NSet, NStr),
    NKeyName (DynamicKey, StaticKey),
    NString (DoubleQuoted, Indented),
    Recursivity (NonRecursive),
    VarName (VarName),
  )
import Nix.Expr.Types.Annotated (AnnUnit (AnnUnit), NExprLoc, stripAnnotation)
import Nix.Parser (parseNixFileLoc)
import Nix.Pretty (prettyNix)
import Nix.Utils (Path (Path))
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath (splitDirectories)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase, TestList),
    assertEqual,
    assertFailure,
    runTestTT,
  )
import Prelude
  ( Bool (False, True),
    Either (Left, Right),
    Eq,
    FilePath,
    Float,
    IO,
    Integer,
    Maybe (Just, Nothing),
    Ord,
    Show,
    String,
    and,
    any,
    concatMap,
    fmap,
    fst,
    map,
    mapM,
    null,
    pure,
    putStrLn,
    show,
    snd,
    ($),
    (&&),
    (++),
    (.),
    (<$>),
    (==),
  )
type Literal :: Type
data Literal
  = LiteralNull
  | LiteralBool Bool
  | LiteralInteger Integer
  | LiteralFloat Float
  | LiteralString Text
  | LiteralList [Literal]
  | LiteralSet [(Text, Literal)]
  deriving stock (Eq, Ord, Show)
type OptionPath :: Type
type OptionPath = [Text]
type DefaultResolver :: Type
type DefaultResolver = OptionPath -> IO (Maybe Literal)
main :: IO ()
main = do
  args <- getArgs
  case args of
    filePaths@(_ : _) -> do
      successResults <- mapM processFileWithInferredConfiguration filePaths
      unless (and successResults) exitFailure
    _ -> do
      putStrLn "Usage: nix-remove-defaults <file>..."
      exitFailure
processFileWithInferredConfiguration :: FilePath -> IO Bool
processFileWithInferredConfiguration filePath =
  case configurationForFile filePath of
    Just configuration -> processFile (resolveDefaultWithNix configuration) filePath
    Nothing -> do
      putStrLn ("Cannot infer a NixOS configuration from " ++ filePath ++ "; expected a path under hosts/<host>/")
      pure False
configurationForFile :: FilePath -> Maybe String
configurationForFile = fmap configurationForHost . hostNameFromPath . splitDirectories
configurationForHost :: FilePath -> String
configurationForHost hostName =
  ".#nixosConfigurations." ++ unpack (renderOptionKey (pack hostName))
hostNameFromPath :: [FilePath] -> Maybe FilePath
hostNameFromPath ("hosts" : hostName : _) = Just hostName
hostNameFromPath (_ : remainingParts) = hostNameFromPath remainingParts
hostNameFromPath [] = Nothing
processFile :: DefaultResolver -> FilePath -> IO Bool
processFile resolveDefault filePath = do
  parseResult <- parseNixFileLoc (Path filePath)
  case parseResult of
    Left parseError -> do
      putStrLn ("Error parsing " ++ filePath ++ ": " ++ show parseError)
      pure False
    Right expr -> do
      (changed, transformed) <- removeDefaultAssignments resolveDefault expr
      when changed $ TIO.writeFile filePath (renderExpression transformed)
      pure True
renderExpression :: NExprLoc -> Text
renderExpression =
  renderStrict . layoutPretty defaultLayoutOptions . prettyNix . stripAnnotation
removeDefaultAssignments :: DefaultResolver -> NExprLoc -> IO (Bool, NExprLoc)
removeDefaultAssignments resolveDefault expr = do
  let candidates = collectCandidates expr
  resolutions <-
    mapM
      ( \(optionPath, literalValue) -> do
          defaultValue <- resolveDefault optionPath
          pure (optionPath, defaultValue == Just literalValue)
      )
      candidates
  pure (any snd resolutions, rewriteModuleExpression (Map.fromList resolutions) expr)
collectCandidates :: NExprLoc -> [(OptionPath, Literal)]
collectCandidates = collectModuleExpression []
collectModuleExpression :: OptionPath -> NExprLoc -> [(OptionPath, Literal)]
collectModuleExpression prefix expr@(Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NAbs _ body -> collectModuleExpression prefix body
    NLet _ body -> collectModuleExpression prefix body
    NSet _ bindings -> collectBindings prefix bindings
    _ -> case expressionLiteral expr of
      Just literalValue -> [(prefix, literalValue)]
      Nothing -> []
collectBindings :: OptionPath -> [Binding NExprLoc] -> [(OptionPath, Literal)]
collectBindings prefix = concatMap (collectBinding prefix)
collectBinding :: OptionPath -> Binding NExprLoc -> [(OptionPath, Literal)]
collectBinding prefix (NamedVar keyPath valueExpr _) =
  case keyPathTexts keyPath of
    Nothing -> []
    Just pathSuffix ->
      let optionPath = effectivePath prefix pathSuffix
          ownCandidate = case expressionLiteral valueExpr of
            Just literalValue -> [(optionPath, literalValue)]
            Nothing -> []
       in ownCandidate ++ collectNestedCandidates optionPath valueExpr
collectBinding _ _ = []
collectNestedCandidates :: OptionPath -> NExprLoc -> [(OptionPath, Literal)]
collectNestedCandidates prefix (Fix (Compose (AnnUnit _ (NSet _ bindings)))) =
  collectBindings prefix bindings
collectNestedCandidates _ _ = []
rewriteModuleExpression :: Map OptionPath Bool -> NExprLoc -> NExprLoc
rewriteModuleExpression removals = goModule []
  where
    goModule :: OptionPath -> NExprLoc -> NExprLoc
    goModule prefix (Fix (Compose (AnnUnit exprSpan exprF))) =
      Fix . Compose . AnnUnit exprSpan $ case exprF of
        NAbs params body -> NAbs params (goModule prefix body)
        NLet bindings body -> NLet bindings (goModule prefix body)
        NSet rec bindings -> NSet rec (rewriteBindings prefix bindings)
        otherExpr -> otherExpr
    rewriteBindings :: OptionPath -> [Binding NExprLoc] -> [Binding NExprLoc]
    rewriteBindings prefix = mapMaybe (rewriteBinding prefix)
    rewriteBinding :: OptionPath -> Binding NExprLoc -> Maybe (Binding NExprLoc)
    rewriteBinding prefix binding@(NamedVar keyPath valueExpr bindingPos) =
      case keyPathTexts keyPath of
        Nothing -> Just binding
        Just pathSuffix ->
          let optionPath = effectivePath prefix pathSuffix
           in if Map.findWithDefault False optionPath removals
                then Nothing
                else
                  let rewrittenValue = rewriteNested optionPath valueExpr
                   in if isEmptySet rewrittenValue && wasNonEmptySet valueExpr
                        then Nothing
                        else Just (NamedVar keyPath rewrittenValue bindingPos)
    rewriteBinding _ binding = Just binding
    rewriteNested :: OptionPath -> NExprLoc -> NExprLoc
    rewriteNested prefix (Fix (Compose (AnnUnit exprSpan (NSet rec bindings)))) =
      Fix (Compose (AnnUnit exprSpan (NSet rec (rewriteBindings prefix bindings))))
    rewriteNested _ valueExpr = valueExpr
effectivePath :: OptionPath -> OptionPath -> OptionPath
effectivePath [] ("config" : rest) = rest
effectivePath prefix suffix = prefix ++ suffix
keyPathTexts :: NonEmpty (NKeyName NExprLoc) -> Maybe [Text]
keyPathTexts (firstKey :| restKeys) = mapM keyNameText (firstKey : restKeys)
keyNameText :: NKeyName NExprLoc -> Maybe Text
keyNameText (StaticKey (VarName keyText)) = Just keyText
keyNameText (DynamicKey (Plain (DoubleQuoted [Plain keyText]))) = Just keyText
keyNameText _ = Nothing
expressionLiteral :: NExprLoc -> Maybe Literal
expressionLiteral (Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NConstant NNull -> Just LiteralNull
    NConstant (NBool value) -> Just (LiteralBool value)
    NConstant (NInt value) -> Just (LiteralInteger value)
    NConstant (NFloat value) -> Just (LiteralFloat value)
    NStr stringValue -> LiteralString <$> plainString stringValue
    NList values -> LiteralList <$> mapM expressionLiteral values
    NSet NonRecursive bindings -> LiteralSet . sortLiteralBindings <$> mapM literalBinding bindings
    _ -> Nothing
plainString :: NString NExprLoc -> Maybe Text
plainString (DoubleQuoted parts) = plainParts parts
plainString (Indented _ parts) = plainParts parts
plainParts :: [Antiquoted Text NExprLoc] -> Maybe Text
plainParts parts = intercalate (pack "") <$> mapM plainPart parts
plainPart :: Antiquoted Text NExprLoc -> Maybe Text
plainPart (Plain textValue) = Just textValue
plainPart _ = Nothing
literalBinding :: Binding NExprLoc -> Maybe (Text, Literal)
literalBinding (NamedVar (keyName :| []) valueExpr _) = do
  keyText <- keyNameText keyName
  literalValue <- expressionLiteral valueExpr
  pure (keyText, literalValue)
literalBinding _ = Nothing
sortLiteralBindings :: [(Text, Literal)] -> [(Text, Literal)]
sortLiteralBindings = sortBy (comparing fst)
isEmptySet :: NExprLoc -> Bool
isEmptySet (Fix (Compose (AnnUnit _ (NSet _ bindings)))) = null bindings
isEmptySet _ = False
wasNonEmptySet :: NExprLoc -> Bool
wasNonEmptySet (Fix (Compose (AnnUnit _ (NSet _ bindings)))) = notNull bindings
wasNonEmptySet _ = False
notNull :: [a] -> Bool
notNull [] = False
notNull (_ : _) = True
resolveDefaultWithNix :: String -> DefaultResolver
resolveDefaultWithNix configuration optionPath = do
  let installable = configuration ++ ".options." ++ unpack (renderOptionPath optionPath) ++ ".default"
  (exitCode, standardOutput, _standardError) <-
    readProcessWithExitCode "nix" ["eval", "--json", installable] ""
  case exitCode of
    ExitSuccess ->
      case eitherDecodeStrict' (BS.pack standardOutput) of
        Right jsonValue -> pure (jsonLiteral jsonValue)
        Left _ -> pure Nothing
    _ -> pure Nothing
renderOptionPath :: OptionPath -> Text
renderOptionPath = intercalate (pack ".") . map renderOptionKey
renderOptionKey :: Text -> Text
renderOptionKey = Text.decodeUtf8 . LBS.toStrict . encode
jsonLiteral :: Value -> Maybe Literal
jsonLiteral Null = Just LiteralNull
jsonLiteral (Bool value) = Just (LiteralBool value)
jsonLiteral (String value) = Just (LiteralString value)
jsonLiteral (Number value) =
  case floatingOrInteger value of
    Left floatValue -> Just (LiteralFloat floatValue)
    Right integerValue -> Just (LiteralInteger integerValue)
jsonLiteral (Array values) = LiteralList <$> mapM jsonLiteral (toList values)
jsonLiteral (Object values) =
  LiteralSet . sortLiteralBindings
    <$> mapM
      ( \(key, value) -> do
          literalValue <- jsonLiteral value
          pure (Key.toText key, literalValue)
      )
      (KeyMap.toList values)
formatWithDefaults :: Map OptionPath Literal -> Text -> IO Text
formatWithDefaults defaults input =
  withSystemTempFile "nix-remove-defaults-test.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    TIO.writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expr -> do
        (_changed, transformed) <- removeDefaultAssignments (pure . (`Map.lookup` defaults)) expr
        pure (renderExpression transformed)
      Left parseError -> assertFailure ("Test fixture failed to parse: " ++ show parseError)
makeRemovalTest :: String -> Map OptionPath Literal -> Text -> Text -> Test
makeRemovalTest testName defaults input expectedOutput = TestCase $ do
  actualOutput <- formatWithDefaults defaults input
  assertEqual testName expectedOutput actualOutput
runPackageTests :: IO ()
runPackageTests = do
  counts <- runTestTT hUnitDebugTests
  if errors counts == 0 && failures counts == 0
    then putStrLn "test ... ok"
    else exitFailure
hUnitDebugTests :: Test
hUnitDebugTests =
  TestList
    [ makeRemovalTest
        "Removes a literal assignment equal to its default."
        (Map.singleton ["boot", "enabled"] (LiteralBool False))
        (pack "{ boot.enabled = false; keep = true; }")
        (pack "{ keep = true; }"),
      makeRemovalTest
        "Preserves a literal assignment different from its default."
        (Map.singleton ["boot", "enabled"] (LiteralBool True))
        (pack "{ boot.enabled = false; }")
        (pack "{ boot.enabled = false; }"),
      makeRemovalTest
        "Removes now-empty structural parent sets."
        (Map.singleton ["services", "example", "enable"] (LiteralBool False))
        (pack "{ services = { example = { enable = false; }; }; keep = 1; }")
        (pack "{ keep = 1; }"),
      makeRemovalTest
        "Removes literal list defaults."
        (Map.singleton ["environment", "systemPackages"] (LiteralList []))
        (pack "{ environment.systemPackages = [ ]; }")
        (pack "{}"),
      makeRemovalTest
        "Removes string, integer, null, and attribute-set defaults."
        ( Map.fromList
            [ (["example", "count"], LiteralInteger 3),
              (["example", "label"], LiteralString "default"),
              (["example", "optional"], LiteralNull),
              (["example", "settings"], LiteralSet [("enabled", LiteralBool True)])
            ]
        )
        (pack "{ example.count = 3; example.label = \"default\"; example.optional = null; example.settings = { enabled = true; }; }")
        (pack "{}"),
      makeRemovalTest
        "Preserves context-dependent expressions."
        (Map.singleton ["example", "value"] (LiteralInteger 1))
        (pack "{ example.value = let x = 1; in x; }")
        (pack "{ example.value = let   x = 1; in x; }"),
      makeRemovalTest
        "Treats a top-level config attribute as the option root."
        (Map.singleton ["networking", "useDHCP"] (LiteralBool True))
        (pack "{ config.networking.useDHCP = true; options.example = { }; }")
        (pack "{ options.example = {}; }"),
      makeRemovalTest
        "Traverses the returned set of a let-wrapped module."
        (Map.singleton ["boot", "initrd", "systemd", "enable"] (LiteralBool True))
        (pack "{ pkgs, ... }: let hostName = \"default\"; in { boot.initrd.systemd.enable = true; networking.hostName = hostName; }")
        (pack "{ pkgs, ... }:\n  let   hostName = \"default\"; in { networking.hostName = hostName; }"),
      TestCase $ do
        let defaults :: Map OptionPath Literal
            defaults = Map.singleton ["example", "enable"] (LiteralBool False)
            input = pack "{ example.enable = false; keep = true; }"
        once <- formatWithDefaults defaults input
        twice <- formatWithDefaults defaults once
        assertEqual "Transformation is idempotent." once twice,
      TestCase $
        withSystemTempFile "nix-remove-defaults-failed-evaluation.nix" $ \tmpFile tmpHandle -> do
          let input = pack "{   example.value = 1; }"
          hClose tmpHandle
          TIO.writeFile tmpFile input
          processSucceeded <- processFile (\_ -> pure Nothing) tmpFile
          output <- TIO.readFile tmpFile
          assertEqual "An unavailable default is not an execution failure." True processSucceeded
          assertEqual "An unavailable default does not rewrite the file." input output,
      TestCase $ do
        assertEqual
          "Infers a configuration from a relative host path."
          (Just ".#nixosConfigurations.\"default\"")
          (configurationForFile "hosts/default/configuration.nix")
        assertEqual
          "Infers a configuration from an absolute host path."
          (Just ".#nixosConfigurations.\"example-host\"")
          (configurationForFile "/tmp/repository/hosts/example-host/configuration.nix")
        assertEqual
          "Rejects files outside the hosts directory."
          Nothing
          (configurationForFile "packages/example/default.nix")
    ]
