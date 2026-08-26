{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Control.Applicative ((<*>))
import Control.Exception (IOException, try)
import Control.Monad (mapM_, unless, when)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Array, Bool, Null, Number, Object, String),
    eitherDecodeStrict',
    encode,
    withArray,
    withObject,
    (.:),
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Either (partitionEithers)
import Data.Fix (Fix (Fix))
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Nix.Atoms (NAtom (NBool, NFloat, NInt, NNull))
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (NamedVar),
    NExprF (NAbs, NApp, NConstant, NLet, NList, NSelect, NSet, NStr, NSym),
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, pathIsSymbolicLink, withCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.FilePath (normalise, pathSeparator, takeDirectory, takeExtension, (</>))
import System.IO (hClose, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase, TestLabel, TestList),
    assertBool,
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
    any,
    concat,
    concatMap,
    div,
    fail,
    fmap,
    fst,
    isInfinite,
    isNaN,
    length,
    map,
    mapM,
    maybe,
    not,
    null,
    otherwise,
    pure,
    putStrLn,
    replicate,
    show,
    snd,
    ($),
    (&&),
    (++),
    (.),
    (<$>),
    (==),
    (||),
  )
type Literal :: Type
data Literal
  = LiteralNull
  | LiteralBool Bool
  | LiteralInteger Integer
  | LiteralFloat Float
  | LiteralString Text
  | LiteralList [Literal]
  | LiteralSet (Map Text Literal)
  deriving stock (Eq, Ord, Show)
type OptionPath :: Type
newtype OptionPath = OptionPath (NonEmpty Text)
  deriving stock (Eq, Ord, Show)
type PathPrefix :: Type
type PathPrefix = [Text]
optionPathFromComponents :: [Text] -> Maybe OptionPath
optionPathFromComponents = fmap OptionPath . NE.nonEmpty
optionPathComponents :: OptionPath -> [Text]
optionPathComponents (OptionPath components) = NE.toList components
type NixOSCandidate :: Type
type NixOSCandidate = (OptionPath, Literal)
type NixOSDefinitionRecord :: Type
data NixOSDefinitionRecord = NixOSDefinitionRecord OptionPath Literal [FilePath]
  deriving stock (Eq, Show)
type TreefmtDefaultRecord :: Type
data TreefmtDefaultRecord = TreefmtDefaultRecord OptionPath Literal
  deriving stock (Eq, Show)
instance FromJSON OptionPath where
  parseJSON =
    withArray "non-empty option path" $ \values -> do
      components <- mapM parseJSON (toList values)
      case optionPathFromComponents components of
        Just optionPath -> pure optionPath
        Nothing -> fail "option path must not be empty"
instance FromJSON Literal where
  parseJSON Null = pure LiteralNull
  parseJSON (Bool value) = pure (LiteralBool value)
  parseJSON (String value) = pure (LiteralString value)
  parseJSON (Number value) =
    case floatingOrInteger value of
      Left floatValue
        | isNaN floatValue || isInfinite floatValue -> fail "floating literal must be finite"
        | otherwise -> pure (LiteralFloat floatValue)
      Right integerValue -> pure (LiteralInteger integerValue)
  parseJSON (Array values) = LiteralList <$> mapM parseJSON (toList values)
  parseJSON (Object values) =
    LiteralSet . Map.fromList
      <$> mapM
        ( \(key, value) -> do
            literalValue <- parseJSON value
            pure (Key.toText key, literalValue)
        )
        (KeyMap.toList values)
instance FromJSON NixOSDefinitionRecord where
  parseJSON =
    withObject "NixOS definition record" $ \values ->
      NixOSDefinitionRecord
        <$> values .: "path"
        <*> values .: "value"
        <*> values .: "files"
instance FromJSON TreefmtDefaultRecord where
  parseJSON =
    withObject "treefmt default record" $ \values ->
      TreefmtDefaultRecord
        <$> values .: "path"
        <*> values .: "default"
type ParsedNixFile :: Type
type ParsedNixFile = (FilePath, NExprLoc)
main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> processArgument "."
    [repositoryPath] -> processArgument repositoryPath
    _ -> do
      hPutStrLn stderr "usage: nix-remove-defaults <flake-or-repository-directory>"
      exitFailure
  where
    processArgument repositoryPath = do
      succeeded <- processRepositoryArgument repositoryPath
      unless succeeded exitFailure
processRepositoryArgument :: FilePath -> IO Bool
processRepositoryArgument repositoryPath = do
  repositoryRootResult <- repositoryRootFromArgument repositoryPath
  case repositoryRootResult of
    Right repositoryRoot -> processRepository repositoryRoot
    Left errorMessage -> do
      hPutStrLn stderr ("error: " ++ errorMessage)
      pure False
repositoryRootFromArgument :: FilePath -> IO (Either String FilePath)
repositoryRootFromArgument repositoryPath = do
  absolutePath <- makeAbsolute repositoryPath
  isDirectory <- doesDirectoryExist absolutePath
  if isDirectory
    then do
      repositoryRoot <- findFlakeRootFrom absolutePath
      case repositoryRoot of
        Just rootPath -> pure (Right rootPath)
        Nothing -> pure (Left ("cannot find flake.nix in " ++ repositoryPath ++ " or its parents"))
    else pure (Left ("no such flake/repository directory: " ++ repositoryPath))
processRepository :: FilePath -> IO Bool
processRepository repositoryRoot = do
  nixFiles <- findNixFiles repositoryRoot
  parseResults <- mapM parseRepositoryFile nixFiles
  let (parseErrors, parsedFiles) = partitionEithers parseResults
  if null parseErrors
    then processParsedRepository repositoryRoot parsedFiles
    else do
      mapM_ (hPutStrLn stderr) parseErrors
      pure False
processParsedRepository :: FilePath -> [ParsedNixFile] -> IO Bool
processParsedRepository repositoryRoot parsedFiles = do
  let nixOSCandidates = uniqueCandidates (concatMap (collectCandidates . snd) parsedFiles)
      treefmtCandidatePaths = uniqueCandidatePaths (concatMap (collectTreefmtCandidates . snd) parsedFiles)
  nixOSDefaultsResult <- resolveNixOSDefaults repositoryRoot nixOSCandidates
  case nixOSDefaultsResult of
    Left diagnostic -> do
      hPutStrLn stderr ("error: " ++ diagnostic)
      pure False
    Right nixOSDefaults -> do
      treefmtDefaultsResult <- resolveTreefmtDefaults repositoryRoot treefmtCandidatePaths
      case treefmtDefaultsResult of
        Left diagnostic -> do
          hPutStrLn stderr ("error: " ++ diagnostic)
          pure False
        Right treefmtDefaults -> do
          mapM_ (processParsedRepositoryFile repositoryRoot nixOSDefaults treefmtDefaults) parsedFiles
          pure True
findFlakeRootFrom :: FilePath -> IO (Maybe FilePath)
findFlakeRootFrom directoryPath = do
  hasFlake <- doesFileExist (directoryPath </> "flake.nix")
  if hasFlake
    then pure (Just directoryPath)
    else
      let parentDirectory = takeDirectory directoryPath
       in if parentDirectory == directoryPath
            then pure Nothing
            else findFlakeRootFrom parentDirectory
findNixFiles :: FilePath -> IO [FilePath]
findNixFiles directoryPath = do
  entries <- sort <$> listDirectory directoryPath
  nestedFiles <-
    mapM
      ( \entryName -> do
          let entryPath = directoryPath </> entryName
          entryIsSymbolicLink <- pathIsSymbolicLink entryPath
          if entryIsSymbolicLink
            then pure []
            else do
              entryIsDirectory <- doesDirectoryExist entryPath
              if entryIsDirectory
                then
                  if shouldSkipDirectory entryName
                    then pure []
                    else findNixFiles entryPath
                else
                  pure
                    [ entryPath
                    | takeExtension entryName == ".nix"
                    ]
      )
      entries
  pure (concat nestedFiles)
shouldSkipDirectory :: FilePath -> Bool
shouldSkipDirectory directoryName =
  directoryName `List.elem` [".agents", ".codex", ".git", "prm", "result", "target", "tmp"]
parseRepositoryFile :: FilePath -> IO (Either String ParsedNixFile)
parseRepositoryFile filePath = do
  parseResult <- parseNixFileLoc (Path filePath)
  pure $ case parseResult of
    Left parseError -> Left ("error: " ++ filePath ++ ": " ++ show parseError)
    Right expr -> Right (filePath, expr)
processParsedRepositoryFile :: FilePath -> Maybe (FilePath, Map NixOSCandidate [FilePath]) -> Map OptionPath Literal -> ParsedNixFile -> IO ()
processParsedRepositoryFile repositoryRoot nixOSDefaults treefmtDefaults (filePath, expr) = do
  let nixOSRemovals =
        case nixOSDefaults of
          Nothing -> Set.empty
          Just (flakeSourcePath, nixOSDefinitionFiles) ->
            nixOSRemovalsForFile repositoryRoot flakeSourcePath filePath nixOSDefinitionFiles (collectCandidates expr)
      treefmtRemovals = removalsFromDefaults treefmtDefaults (collectTreefmtCandidates expr)
      changed = not (Set.null nixOSRemovals) || not (Set.null treefmtRemovals)
      transformed =
        rewriteTreefmtEvalModuleArguments
          treefmtRemovals
          (rewriteModuleExpression nixOSRemovals expr)
  when changed $ TIO.writeFile filePath (renderExpression transformed)
renderExpression :: NExprLoc -> Text
renderExpression =
  renderStrict . layoutPretty defaultLayoutOptions . prettyNix . stripAnnotation
removalsFromDefaults :: Map OptionPath Literal -> [(OptionPath, Literal)] -> Set OptionPath
removalsFromDefaults defaults candidates =
  Set.fromList
    [ optionPath
    | (optionPath, literalValue) <- candidates,
      Map.lookup optionPath defaults == Just literalValue
    ]
uniqueCandidates :: [NixOSCandidate] -> [NixOSCandidate]
uniqueCandidates = Set.toAscList . Set.fromList
uniqueCandidatePaths :: [NixOSCandidate] -> [OptionPath]
uniqueCandidatePaths = Set.toAscList . Set.fromList . map fst
collectCandidates :: NExprLoc -> [(OptionPath, Literal)]
collectCandidates = collectModuleExpression []
collectTreefmtCandidates :: NExprLoc -> [(OptionPath, Literal)]
collectTreefmtCandidates (Fix (Compose (AnnUnit _ exprF))) =
  let nestedCandidates = concatMap collectTreefmtCandidates exprF
   in case exprF of
        NApp function argument
          | isTreefmtEvalModuleFunction function ->
              collectCandidates argument ++ nestedCandidates
        _ -> nestedCandidates
collectModuleExpression :: PathPrefix -> NExprLoc -> [(OptionPath, Literal)]
collectModuleExpression prefix expr@(Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NAbs _ body -> collectModuleExpression prefix body
    NLet _ body -> collectModuleExpression prefix body
    NSet _ bindings -> collectBindings prefix bindings
    _ -> case (optionPathFromComponents prefix, expressionLiteral expr) of
      (Just optionPath, Just literalValue) -> [(optionPath, literalValue)]
      _ -> []
collectBindings :: PathPrefix -> [Binding NExprLoc] -> [(OptionPath, Literal)]
collectBindings prefix = concatMap (collectBinding prefix)
collectBinding :: PathPrefix -> Binding NExprLoc -> [(OptionPath, Literal)]
collectBinding prefix (NamedVar keyPath valueExpr _) =
  case keyPathTexts keyPath of
    Nothing -> []
    Just pathSuffix ->
      let pathComponents = effectivePath prefix pathSuffix
          ownCandidate = case (optionPathFromComponents pathComponents, expressionLiteral valueExpr) of
            (Just optionPath, Just literalValue) -> [(optionPath, literalValue)]
            _ -> []
       in ownCandidate ++ collectNestedCandidates pathComponents valueExpr
collectBinding _ _ = []
collectNestedCandidates :: PathPrefix -> NExprLoc -> [(OptionPath, Literal)]
collectNestedCandidates prefix (Fix (Compose (AnnUnit _ (NSet _ bindings)))) =
  collectBindings prefix bindings
collectNestedCandidates _ _ = []
rewriteModuleExpression :: Set OptionPath -> NExprLoc -> NExprLoc
rewriteModuleExpression removals = goModule []
  where
    goModule :: PathPrefix -> NExprLoc -> NExprLoc
    goModule prefix (Fix (Compose (AnnUnit exprSpan exprF))) =
      Fix . Compose . AnnUnit exprSpan $ case exprF of
        NAbs params body -> NAbs params (goModule prefix body)
        NLet bindings body -> NLet bindings (goModule prefix body)
        NSet rec bindings -> NSet rec (rewriteBindings prefix bindings)
        otherExpr -> otherExpr
    rewriteBindings :: PathPrefix -> [Binding NExprLoc] -> [Binding NExprLoc]
    rewriteBindings prefix = mapMaybe (rewriteBinding prefix)
    rewriteBinding :: PathPrefix -> Binding NExprLoc -> Maybe (Binding NExprLoc)
    rewriteBinding prefix binding@(NamedVar keyPath valueExpr bindingPos) =
      case keyPathTexts keyPath of
        Nothing -> Just binding
        Just pathSuffix ->
          let pathComponents = effectivePath prefix pathSuffix
              shouldRemove = maybe False (`Set.member` removals) (optionPathFromComponents pathComponents)
           in if shouldRemove
                then Nothing
                else
                  let rewrittenValue = rewriteNested pathComponents valueExpr
                   in if setBecameEmpty valueExpr rewrittenValue
                        then Nothing
                        else Just (NamedVar keyPath rewrittenValue bindingPos)
    rewriteBinding _ binding = Just binding
    rewriteNested :: PathPrefix -> NExprLoc -> NExprLoc
    rewriteNested prefix (Fix (Compose (AnnUnit exprSpan (NSet rec bindings)))) =
      Fix (Compose (AnnUnit exprSpan (NSet rec (rewriteBindings prefix bindings))))
    rewriteNested _ valueExpr = valueExpr
rewriteTreefmtEvalModuleArguments :: Set OptionPath -> NExprLoc -> NExprLoc
rewriteTreefmtEvalModuleArguments removals = go
  where
    go :: NExprLoc -> NExprLoc
    go (Fix (Compose (AnnUnit exprSpan exprF))) =
      Fix . Compose . AnnUnit exprSpan $ case exprF of
        NApp function argument
          | isTreefmtEvalModuleFunction function ->
              NApp (go function) (rewriteModuleExpression removals argument)
        otherExpr -> fmap go otherExpr
isTreefmtEvalModuleFunction :: NExprLoc -> Bool
isTreefmtEvalModuleFunction expr@(Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NApp function _ -> isTreefmtEvalModuleFunction function
    _ ->
      case selectorPath expr of
        Just path -> ["treefmt-nix", "lib", "evalModule"] `isSuffixOf` path
        Nothing -> False
selectorPath :: NExprLoc -> Maybe [Text]
selectorPath (Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NSym (VarName keyText) -> Just [keyText]
    NSelect _ base keyPath -> do
      basePath <- selectorPath base
      suffix <- keyPathTexts keyPath
      pure (basePath ++ suffix)
    _ -> Nothing
effectivePath :: PathPrefix -> [Text] -> PathPrefix
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
    NSet NonRecursive bindings -> literalSetFromBindings bindings
    _ -> Nothing
literalSetFromBindings :: [Binding NExprLoc] -> Maybe Literal
literalSetFromBindings bindings = do
  literalBindings <- mapM literalBinding bindings
  let literalValues = Map.fromList literalBindings
  if Map.size literalValues == length literalBindings
    then Just (LiteralSet literalValues)
    else Nothing
plainString :: NString NExprLoc -> Maybe Text
plainString (DoubleQuoted parts) = plainParts parts
plainString (Indented _ parts) = plainParts parts
plainParts :: [Antiquoted Text NExprLoc] -> Maybe Text
plainParts parts = T.concat <$> mapM plainPart parts
plainPart :: Antiquoted Text NExprLoc -> Maybe Text
plainPart (Plain textValue) = Just textValue
plainPart _ = Nothing
literalBinding :: Binding NExprLoc -> Maybe (Text, Literal)
literalBinding (NamedVar (keyName :| []) valueExpr _) = do
  keyText <- keyNameText keyName
  literalValue <- expressionLiteral valueExpr
  pure (keyText, literalValue)
literalBinding _ = Nothing
setBecameEmpty :: NExprLoc -> NExprLoc -> Bool
setBecameEmpty
  (Fix (Compose (AnnUnit _ (NSet _ (_ : _)))))
  (Fix (Compose (AnnUnit _ (NSet _ [])))) = True
setBecameEmpty _ _ = False
flakeSourcePathForRepository :: FilePath -> IO (Either String FilePath)
flakeSourcePathForRepository repositoryRoot =
  readJSONFromNix ["eval", "--impure", "--json", "--expr", flakeSourcePathExpression repositoryRoot]
nixOSConfigurationsForRepository :: FilePath -> IO (Either String [String])
nixOSConfigurationsForRepository repositoryRoot =
  readJSONFromNix ["eval", "--impure", "--json", "--expr", nixOSConfigurationsExpression repositoryRoot]
resolveNixOSDefinitionFiles :: FilePath -> [String] -> [NixOSCandidate] -> IO (Either String (Map NixOSCandidate [FilePath]))
resolveNixOSDefinitionFiles repositoryRoot nixOSConfigurations candidates = do
  definitionFiles <- resolveGroups nixOSConfigurations candidates
  pure (Right (Map.fromListWith (++) definitionFiles))
  where
    resolveGroups :: [String] -> [NixOSCandidate] -> IO [(NixOSCandidate, [FilePath])]
    resolveGroups [] _ = pure []
    resolveGroups _ [] = pure []
    resolveGroups configurationGroup candidateGroup = do
      definitionFilesResult <-
        readNixOSDefinitionFilesFromNix
          [ "eval",
            "--impure",
            "--json",
            "--expr",
            nixOSDefaultDefinitionFilesExpression repositoryRoot configurationGroup candidateGroup
          ]
      case definitionFilesResult of
        Right definitionFiles -> pure definitionFiles
        Left _ ->
          case candidateGroup of
            _ : _ : _ ->
              let (firstCandidates, remainingCandidates) = List.splitAt (length candidateGroup `div` 2) candidateGroup
               in (++) <$> resolveGroups configurationGroup firstCandidates <*> resolveGroups configurationGroup remainingCandidates
            [candidate] ->
              case configurationGroup of
                _ : _ : _ ->
                  let (firstConfigurations, remainingConfigurations) = List.splitAt (length configurationGroup `div` 2) configurationGroup
                   in (++) <$> resolveGroups firstConfigurations candidateGroup <*> resolveGroups remainingConfigurations candidateGroup
                [configurationName] -> do
                  hPutStrLn stderr ("warning: skipping an unevaluable default for " ++ show (fst candidate) ++ " in NixOS configuration " ++ configurationName)
                  pure []
resolveNixOSDefaults :: FilePath -> [NixOSCandidate] -> IO (Either String (Maybe (FilePath, Map NixOSCandidate [FilePath])))
resolveNixOSDefaults _ [] = pure (Right Nothing)
resolveNixOSDefaults repositoryRoot candidates = do
  nixOSConfigurationsResult <- nixOSConfigurationsForRepository repositoryRoot
  case nixOSConfigurationsResult of
    Left diagnostic -> pure (Left diagnostic)
    Right [] -> pure (Right Nothing)
    Right nixOSConfigurations -> do
      flakeSourcePathResult <- flakeSourcePathForRepository repositoryRoot
      case flakeSourcePathResult of
        Left diagnostic -> pure (Left diagnostic)
        Right flakeSourcePath -> do
          definitionFilesResult <- resolveNixOSDefinitionFiles repositoryRoot nixOSConfigurations candidates
          pure (fmap (\definitionFiles -> Just (flakeSourcePath, definitionFiles)) definitionFilesResult)
nixOSRemovalsForFile :: FilePath -> FilePath -> FilePath -> Map NixOSCandidate [FilePath] -> [NixOSCandidate] -> Set OptionPath
nixOSRemovalsForFile repositoryRoot flakeSourcePath localFilePath nixOSDefinitionFiles candidates =
  Set.fromList
    [ optionPath
    | candidate@(optionPath, _) <- uniqueCandidates candidates,
      any (sourceLocationMatches repositoryRoot flakeSourcePath localFilePath) (Map.findWithDefault [] candidate nixOSDefinitionFiles)
    ]
resolveTreefmtDefaults :: FilePath -> [OptionPath] -> IO (Either String (Map OptionPath Literal))
resolveTreefmtDefaults _ [] = pure (Right Map.empty)
resolveTreefmtDefaults repositoryRoot optionPaths = do
  defaultsResult <- readTreefmtDefaultsFromNix ["eval", "--impure", "--json", "--expr", treefmtDefaultsExpression repositoryRoot optionPaths]
  pure (Map.fromList <$> defaultsResult)
flakeSourcePathExpression :: FilePath -> String
flakeSourcePathExpression repositoryRoot =
  "(builtins.getFlake (toString (/. + " ++ nixString repositoryRoot ++ "))).outPath"
nixOSConfigurationsExpression :: FilePath -> String
nixOSConfigurationsExpression repositoryRoot =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); in builtins.attrNames (flake.nixosConfigurations or {})"
nixOSDefaultDefinitionFilesExpression :: FilePath -> [String] -> [NixOSCandidate] -> String
nixOSDefaultDefinitionFilesExpression repositoryRoot configurationNames candidates =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); configurations = flake.nixosConfigurations or {}; configurationNames = "
    ++ renderStringList configurationNames
    ++ "; candidates = "
    ++ renderNixOSCandidateList candidates
    ++ "; optionAt = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; remainingPath = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then optionAt value.${key} remainingPath else { success = false; }; literalEquals = expected: actual: if builtins.isNull expected then builtins.isNull actual else if builtins.isBool expected then builtins.isBool actual && actual == expected else if builtins.isInt expected then builtins.isInt actual && actual == expected else if builtins.isFloat expected then builtins.isFloat actual && actual == expected else if builtins.isString expected then builtins.isString actual && actual == expected else if builtins.isList expected then builtins.isList actual && builtins.length actual == builtins.length expected && builtins.all (index: literalEquals (builtins.elemAt expected index) (builtins.elemAt actual index)) (builtins.genList (index: index) (builtins.length expected)) else if builtins.isAttrs expected then builtins.isAttrs actual && builtins.attrNames expected == builtins.attrNames actual && builtins.all (name: literalEquals expected.${name} actual.${name}) (builtins.attrNames expected) else false; filesFor = configurationName: candidate: let evaluated = builtins.getAttr configurationName configurations; optionAttempt = optionAt evaluated.options candidate.path; definitionFiles = if optionAttempt.success && builtins.isAttrs optionAttempt.value then builtins.concatMap (definition: if builtins.isAttrs definition && definition ? file then [ definition.file ] else []) (optionAttempt.value.definitionsWithLocations or []) else []; definitionFilesAttempt = builtins.tryEval (builtins.deepSeq definitionFiles definitionFiles); raw = if definitionFilesAttempt.success && definitionFilesAttempt.value != [] && optionAttempt.value ? default && literalEquals candidate.value optionAttempt.value.default then definitionFilesAttempt.value else []; attempted = builtins.tryEval (builtins.deepSeq raw raw); in if attempted.success then attempted.value else []; candidateFiles = candidate: { inherit (candidate) path value; files = builtins.concatMap (configurationName: filesFor configurationName candidate) configurationNames; }; in builtins.map candidateFiles candidates"
treefmtDefaultsExpression :: FilePath -> [OptionPath] -> String
treefmtDefaultsExpression repositoryRoot optionPaths =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); pkgs = import flake.inputs.nixpkgs { system = builtins.currentSystem; }; evaluated = flake.inputs.treefmt-nix.lib.evalModule pkgs {}; optionPaths = "
    ++ renderOptionPathList optionPaths
    ++ "; optionAt = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; remainingPath = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then optionAt value.${key} remainingPath else { success = false; }; defaultFor = path: let optionAttempt = optionAt evaluated.options path; raw = if optionAttempt.success && builtins.isAttrs optionAttempt.value && optionAttempt.value ? default then optionAttempt.value.default else throw \"missing option default\"; attempted = builtins.tryEval (builtins.deepSeq raw raw); in if attempted.success then [{ inherit path; default = attempted.value; }] else []; in builtins.concatMap defaultFor optionPaths"
renderStringList :: [String] -> String
renderStringList values =
  "[ " ++ List.unwords (map nixString values) ++ " ]"
renderOptionPathList :: [OptionPath] -> String
renderOptionPathList optionPaths =
  "[ " ++ List.unwords (map renderOptionPathAsList optionPaths) ++ " ]"
renderNixOSCandidateList :: [NixOSCandidate] -> String
renderNixOSCandidateList candidates =
  "[ " ++ List.unwords (map renderNixOSCandidate candidates) ++ " ]"
renderNixOSCandidate :: NixOSCandidate -> String
renderNixOSCandidate (optionPath, literalValue) =
  "{ path = " ++ renderOptionPathAsList optionPath ++ "; value = " ++ renderLiteral literalValue ++ "; }"
renderOptionPathAsList :: OptionPath -> String
renderOptionPathAsList optionPath =
  "[ " ++ List.unwords (map (nixString . unpack) (optionPathComponents optionPath)) ++ " ]"
renderLiteral :: Literal -> String
renderLiteral LiteralNull = "null"
renderLiteral (LiteralBool True) = "true"
renderLiteral (LiteralBool False) = "false"
renderLiteral (LiteralInteger value) = show value
renderLiteral (LiteralFloat value) = show value
renderLiteral (LiteralString value) = nixString (unpack value)
renderLiteral (LiteralList values) =
  "[ " ++ List.unwords (map renderLiteral values) ++ " ]"
renderLiteral (LiteralSet bindings) =
  "{ " ++ List.unwords (map renderLiteralBinding (Map.toAscList bindings)) ++ " }"
renderLiteralBinding :: (Text, Literal) -> String
renderLiteralBinding (key, value) =
  nixString (unpack key) ++ " = " ++ renderLiteral value ++ ";"
sourceLocationMatches :: FilePath -> FilePath -> FilePath -> FilePath -> Bool
sourceLocationMatches repositoryRoot flakeSourcePath localFilePath sourceFilePath =
  case stripPathPrefix flakeSourcePath sourceFilePath of
    Just relativeSourcePath ->
      normalise (repositoryRoot </> relativeSourcePath) == normalise localFilePath
    Nothing ->
      case stripStoreSourcePrefix sourceFilePath of
        Just relativeSourcePath ->
          normalise (repositoryRoot </> relativeSourcePath) == normalise localFilePath
        Nothing -> normalise sourceFilePath == normalise localFilePath
stripPathPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPathPrefix prefix path
  | prefix == path = Just ""
  | otherwise = List.stripPrefix (prefix ++ [pathSeparator]) path
stripStoreSourcePrefix :: FilePath -> Maybe FilePath
stripStoreSourcePrefix [] = Nothing
stripStoreSourcePrefix path@(_ : remainingPath) =
  case List.stripPrefix storeSourceMarker path of
    Just relativePath -> Just relativePath
    Nothing -> stripStoreSourcePrefix remainingPath
  where
    storeSourceMarker = "-source" ++ [pathSeparator]
nixString :: String -> String
nixString = unpack . renderOptionKey . pack
readNixOSDefinitionFilesFromNix :: [String] -> IO (Either String [(NixOSCandidate, [FilePath])])
readNixOSDefinitionFilesFromNix arguments =
  fmap (map nixOSDefinitionEntry) <$> readJSONFromNix arguments
  where
    nixOSDefinitionEntry :: NixOSDefinitionRecord -> (NixOSCandidate, [FilePath])
    nixOSDefinitionEntry (NixOSDefinitionRecord optionPath literalValue sourceFiles) =
      ((optionPath, literalValue), sourceFiles)
readTreefmtDefaultsFromNix :: [String] -> IO (Either String [(OptionPath, Literal)])
readTreefmtDefaultsFromNix arguments =
  fmap (map treefmtDefaultEntry) <$> readJSONFromNix arguments
  where
    treefmtDefaultEntry :: TreefmtDefaultRecord -> (OptionPath, Literal)
    treefmtDefaultEntry (TreefmtDefaultRecord optionPath defaultValue) =
      (optionPath, defaultValue)
readJSONFromNix :: (FromJSON value) => [String] -> IO (Either String value)
readJSONFromNix = readJSONFromNixWith tryReadNixProcess
readJSONFromNixWith :: (FromJSON value) => ([String] -> IO (Either IOException (ExitCode, String, String))) -> [String] -> IO (Either String value)
readJSONFromNixWith runNix arguments = do
  processResult <- runNix arguments
  case processResult of
    Left processError -> pure (Left ("cannot execute nix: " ++ show processError))
    Right (ExitFailure _, _, standardError) ->
      pure (Left ("nix evaluation failed: " ++ standardError))
    Right (ExitSuccess, standardOutput, _) ->
      pure $ case eitherDecodeStrict' (Text.encodeUtf8 (pack standardOutput)) of
        Right jsonValue -> Right jsonValue
        Left diagnostic -> Left ("cannot decode Nix JSON output: " ++ diagnostic)
tryReadNixProcess :: [String] -> IO (Either IOException (ExitCode, String, String))
tryReadNixProcess arguments =
  try (readProcessWithExitCode "nix" arguments "")
renderOptionKey :: Text -> Text
renderOptionKey = Text.decodeUtf8 . LBS.toStrict . encode
formatWithDefaults :: Map OptionPath Literal -> Text -> IO Text
formatWithDefaults defaults input =
  withTestExpression input $ \expr -> do
    let removals = removalsFromDefaults defaults (collectCandidates expr)
    pure (renderExpression (rewriteModuleExpression removals expr))
formatTreefmtWithDefaults :: Map OptionPath Literal -> Text -> IO Text
formatTreefmtWithDefaults defaults input =
  withTestExpression input $ \expr -> do
    let removals = removalsFromDefaults defaults (collectTreefmtCandidates expr)
    pure (renderExpression (rewriteTreefmtEvalModuleArguments removals expr))
withTestExpression :: Text -> (NExprLoc -> IO a) -> IO a
withTestExpression input useExpression =
  withSystemTempFile "nix-remove-defaults-test.nix" $ \temporaryFile temporaryHandle -> do
    hClose temporaryHandle
    TIO.writeFile temporaryFile input
    parseResult <- parseNixFileLoc (Path temporaryFile)
    case parseResult of
      Right expr -> useExpression expr
      Left parseError -> assertFailure ("Test fixture failed to parse: " ++ show parseError)
makeRemovalTest :: Map OptionPath Literal -> Text -> Text -> Test
makeRemovalTest defaults input expectedOutput = TestCase $ do
  actualOutput <- formatWithDefaults defaults input
  assertEqual "formatted output" expectedOutput actualOutput
makeTreefmtRemovalTest :: Map OptionPath Literal -> Text -> Text -> Test
makeTreefmtRemovalTest defaults input expectedOutput = TestCase $ do
  actualOutput <- formatTreefmtWithDefaults defaults input
  assertEqual "formatted output" expectedOutput actualOutput
runPackageTests :: IO ()
runPackageTests = runPackageTestsWith hUnitPackageTests
runPackageTestsWith :: Test -> IO ()
runPackageTestsWith packageTests = do
  counts <- runTestTT packageTests
  if errors counts == 0 && failures counts == 0
    then putStrLn "test ... ok"
    else exitFailure
hUnitPackageTests :: Test
hUnitPackageTests =
  TestList
    [ TestLabel "Removes a literal assignment equal to its default." $
        makeRemovalTest
          (Map.singleton (OptionPath ("boot" :| ["enabled"])) (LiteralBool False))
          (pack "{ boot.enabled = false; keep = true; }")
          (pack "{ keep = true; }"),
      TestLabel "Preserves a literal assignment different from its default." $
        makeRemovalTest
          (Map.singleton (OptionPath ("boot" :| ["enabled"])) (LiteralBool True))
          (pack "{ boot.enabled = false; }")
          (pack "{ boot.enabled = false; }"),
      TestLabel "Removes now-empty structural parent sets." $
        makeRemovalTest
          (Map.singleton (OptionPath ("services" :| ["example", "enable"])) (LiteralBool False))
          (pack "{ services = { example = { enable = false; }; }; keep = 1; }")
          (pack "{ keep = 1; }"),
      TestLabel "Removes literal list defaults." $
        makeRemovalTest
          (Map.singleton (OptionPath ("environment" :| ["systemPackages"])) (LiteralList []))
          (pack "{ environment.systemPackages = [ ]; }")
          (pack "{}"),
      TestLabel "Removes string, integer, null, and attribute-set defaults." $
        makeRemovalTest
          ( Map.fromList
              [ (OptionPath ("example" :| ["count"]), LiteralInteger 3),
                (OptionPath ("example" :| ["label"]), LiteralString "default"),
                (OptionPath ("example" :| ["optional"]), LiteralNull),
                (OptionPath ("example" :| ["settings"]), LiteralSet (Map.singleton "enabled" (LiteralBool True)))
              ]
          )
          (pack "{ example.count = 3; example.label = \"default\"; example.optional = null; example.settings = { enabled = true; }; }")
          (pack "{}"),
      TestLabel "Preserves context-dependent expressions." $
        makeRemovalTest
          (Map.singleton (OptionPath ("example" :| ["value"])) (LiteralInteger 1))
          (pack "{ example.value = let x = 1; in x; }")
          (pack "{ example.value = let   x = 1; in x; }"),
      TestLabel "Rejects duplicate keys in literal attribute sets." $
        TestCase $
          withTestExpression (pack "{ enabled = true; enabled = false; }") $
            assertEqual "duplicate literal keys" Nothing . expressionLiteral,
      TestLabel "Treats a top-level config attribute as the option root." $
        makeRemovalTest
          (Map.singleton (OptionPath ("networking" :| ["useDHCP"])) (LiteralBool True))
          (pack "{ config.networking.useDHCP = true; options.example = { }; }")
          (pack "{ options.example = {}; }"),
      TestLabel "Traverses the returned set of a let-wrapped module." $
        makeRemovalTest
          (Map.singleton (OptionPath ("boot" :| ["initrd", "systemd", "enable"])) (LiteralBool True))
          (pack "{ pkgs, ... }: let hostName = \"default\"; in { boot.initrd.systemd.enable = true; networking.hostName = hostName; }")
          (pack "{ pkgs, ... }:\n  let   hostName = \"default\"; in { networking.hostName = hostName; }"),
      TestLabel "Transformation is idempotent." $ TestCase $ do
        let defaults :: Map OptionPath Literal
            defaults = Map.singleton (OptionPath ("example" :| ["enable"])) (LiteralBool False)
            input = pack "{ example.enable = false; keep = true; }"
        once <- formatWithDefaults defaults input
        twice <- formatWithDefaults defaults once
        assertEqual "second transformation" once twice,
      TestLabel "Skips Nix evaluation when no literal candidates exist." $ TestCase $ do
        succeeded <- processParsedRepository "/path/that/does/not/exist" []
        assertBool "empty repository processing" succeeded,
      TestLabel "Removes defaults inside a treefmt evalModule argument." $
        makeTreefmtRemovalTest
          (Map.singleton (OptionPath ("programs" :| ["shfmt", "simplify"])) (LiteralBool True))
          (pack "{ inputs, pkgs, ... }: let treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs { programs.shfmt.simplify = true; keep = false; }; in treefmtEval.config.build.wrapper")
          (pack "{ inputs, pkgs, ... }:\n  let\n    treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs { keep = false; };\n  in treefmtEval.config.build.wrapper"),
      TestLabel "Builds the NixOS default-definition lookup expression." $ TestCase $ do
        let expression =
              nixOSDefaultDefinitionFilesExpression
                "/repository"
                ["default"]
                [(OptionPath ("boot" :| ["initrd", "systemd", "enable"]), LiteralBool True)]
        assertBool
          "candidate records"
          ("candidates = [ { path = [ \"boot\" \"initrd\" \"systemd\" \"enable\" ]; value = true; } ]" `isInfixOf` expression)
        assertBool
          "literal comparison"
          ("literalEquals = expected: actual:" `isInfixOf` expression)
        assertBool
          "candidate result"
          ("candidateFiles = candidate: { inherit (candidate) path value; files =" `isInfixOf` expression)
        assertBool
          "defaults are only forced for configurations that define the candidate"
          ("definitionFilesAttempt.success && definitionFilesAttempt.value != [] && optionAttempt.value ? default" `isInfixOf` expression)
        assertBool
          "malformed definition entries are ignored"
          ("builtins.isAttrs definition && definition ? file" `isInfixOf` expression),
      TestLabel "Rejects empty option paths from Nix JSON." $
        TestCase $
          case (eitherDecodeStrict' (Text.encodeUtf8 "[]") :: Either String OptionPath) of
            Left diagnostic -> assertBool "non-empty path diagnostic" ("must not be empty" `isInfixOf` diagnostic)
            Right _ -> assertFailure "empty option path decoded successfully",
      TestLabel "Preserves JSON decoding diagnostics." $
        TestCase $
          case (eitherDecodeStrict' (Text.encodeUtf8 "null") :: Either String String) of
            Left diagnostic -> assertBool "typed decode failure" ("expected String" `isInfixOf` diagnostic)
            Right _ -> assertFailure "null decoded as a string",
      TestLabel "Decodes Unicode Nix JSON without byte truncation." $
        TestCase $
          assertEqual
            "Unicode record"
            (Right (TreefmtDefaultRecord (OptionPath ("naïve" :| [])) (LiteralString "λ")))
            ( eitherDecodeStrict'
                (Text.encodeUtf8 "{\"path\":[\"naïve\"],\"default\":\"λ\"}") ::
                Either String TreefmtDefaultRecord
            ),
      TestLabel "Rejects non-finite floating literals." $
        TestCase $ do
          let oversizedFraction = pack (replicate 400 '9' ++ ".1")
          case (eitherDecodeStrict' (Text.encodeUtf8 oversizedFraction) :: Either String Literal) of
            Left diagnostic -> assertBool "finite floating literal diagnostic" ("must be finite" `isInfixOf` diagnostic)
            Right _ -> assertFailure "non-finite floating literal decoded successfully",
      TestLabel "Propagates the underlying Nix evaluation diagnostic." $
        TestCase $ do
          result <-
            readJSONFromNixWith
              (\_ -> pure (Right (ExitFailure 1, "", "underlying evaluation failure\n")))
              []
          assertEqual
            "Nix failure"
            (Left "nix evaluation failed: underlying evaluation failure\n")
            (result :: Either String String),
      TestLabel "The installed executable defaults to the current directory and discovers its flake." $
        TestCase $
          withSystemTempDirectory "nix-remove-defaults-e2e" $ \temporaryDirectory -> do
            maybeExecutable <- lookupEnv "PACKAGE_E2E_EXECUTABLE"
            case maybeExecutable of
              Nothing -> pure ()
              Just executable -> do
                let nestedDirectory = temporaryDirectory </> "nested"
                    modulePath = temporaryDirectory </> "empty-module.nix"
                createDirectoryIfMissing True nestedDirectory
                TIO.writeFile
                  (temporaryDirectory </> "flake.nix")
                  (pack "{ outputs = { self }: { nixosConfigurations = { }; }; }")
                TIO.writeFile modulePath (pack "{ }")
                setEnv "NIX_CONFIG" "experimental-features = nix-command flakes"
                (commandExit, commandStdout, commandStderr) <-
                  withCurrentDirectory nestedDirectory (readProcessWithExitCode executable [] "")
                assertEqual
                  ( "installed command output: stdout="
                      ++ show commandStdout
                      ++ " stderr="
                      ++ show commandStderr
                  )
                  ExitSuccess
                  commandExit
                assertEqual "installed command stdout" "" commandStdout
                assertEqual "installed command stderr" "" commandStderr
                moduleContents <- TIO.readFile modulePath
                assertEqual "empty module remains unchanged" (pack "{ }") moduleContents,
      TestLabel "The installed executable reports failures on stderr." $
        TestCase $
          withSystemTempDirectory "nix-remove-defaults-errors" $ \temporaryDirectory -> do
            maybeExecutable <- lookupEnv "PACKAGE_E2E_EXECUTABLE"
            case maybeExecutable of
              Nothing -> pure ()
              Just executable -> do
                (usageExit, usageStdout, usageStderr) <-
                  readProcessWithExitCode executable [temporaryDirectory, temporaryDirectory] ""
                assertEqual "usage failure exit" (ExitFailure 1) usageExit
                assertEqual "usage failure stdout" "" usageStdout
                assertEqual
                  "usage failure stderr"
                  "usage: nix-remove-defaults <flake-or-repository-directory>\n"
                  usageStderr
                let missingDirectory = temporaryDirectory </> "missing"
                (missingExit, missingStdout, missingStderr) <-
                  readProcessWithExitCode executable [missingDirectory] ""
                assertEqual "missing directory exit" (ExitFailure 1) missingExit
                assertEqual "missing directory stdout" "" missingStdout
                assertEqual
                  "missing directory stderr"
                  ("error: no such flake/repository directory: " ++ missingDirectory ++ "\n")
                  missingStderr
    ]
