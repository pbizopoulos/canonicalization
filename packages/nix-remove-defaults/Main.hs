{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Control.Monad (mapM_, unless, when)
import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), eitherDecodeStrict', encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Fix (Fix (Fix))
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sortBy)
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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath (normalise, pathSeparator, takeDirectory, takeExtension, (</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase, TestList),
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
    and,
    any,
    concat,
    concatMap,
    fmap,
    fst,
    map,
    mapM,
    null,
    otherwise,
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
    (>>=),
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
  | LiteralSet [(Text, Literal)]
  deriving stock (Eq, Ord, Show)
type OptionPath :: Type
type OptionPath = [Text]
type NixosCandidate :: Type
type NixosCandidate = (OptionPath, Literal)
type DefaultResolver :: Type
type DefaultResolver = OptionPath -> IO (Maybe Literal)
type ParsedNixFile :: Type
type ParsedNixFile = (FilePath, NExprLoc)
main :: IO ()
main = do
  args <- getArgs
  case args of
    [repositoryPath] -> do
      succeeded <- processRepositoryArgument repositoryPath
      unless succeeded exitFailure
    _ -> do
      putStrLn "Usage: nix-remove-defaults <flake-or-repository-directory>"
      exitFailure
processRepositoryArgument :: FilePath -> IO Bool
processRepositoryArgument repositoryPath = do
  repositoryRootResult <- repositoryRootFromArgument repositoryPath
  case repositoryRootResult of
    Right repositoryRoot -> processRepository repositoryRoot
    Left errorMessage -> do
      putStrLn errorMessage
      pure False
repositoryRootFromArgument :: FilePath -> IO (Either String FilePath)
repositoryRootFromArgument repositoryPath = do
  absolutePath <- makeAbsolute repositoryPath
  isFile <- doesFileExist absolutePath
  isDirectory <- doesDirectoryExist absolutePath
  if isFile
    then pure (Left ("Expected a flake/repository directory, got a file: " ++ repositoryPath))
    else
      if isDirectory
        then do
          repositoryRoot <- findFlakeRootFrom absolutePath
          case repositoryRoot of
            Just rootPath -> pure (Right rootPath)
            Nothing -> pure (Left ("Cannot find flake.nix in " ++ repositoryPath ++ " or its parents"))
        else pure (Left ("No such flake/repository directory: " ++ repositoryPath))
processRepository :: FilePath -> IO Bool
processRepository repositoryRoot = do
  flakeSourcePath <- flakeSourcePathForRepository repositoryRoot
  nixosConfigurations <- nixosConfigurationsForRepository repositoryRoot
  nixFiles <- findNixFiles repositoryRoot
  parseResults <- mapM parseRepositoryFile nixFiles
  let parseErrors = repositoryParseErrors parseResults
      parsedFiles = repositoryParsedFiles parseResults
  if null parseErrors
    then do
      let nixosCandidates = concatMap (collectCandidates . snd) parsedFiles
          treefmtCandidates = concatMap (collectTreefmtCandidates . snd) parsedFiles
      nixosDefinitionFiles <- resolveNixosDefinitionFiles repositoryRoot nixosConfigurations (uniqueCandidates nixosCandidates)
      treefmtDefaults <- resolveTreefmtDefaults repositoryRoot (uniqueCandidatePaths treefmtCandidates)
      successResults <- mapM (processParsedRepositoryFile repositoryRoot flakeSourcePath nixosDefinitionFiles treefmtDefaults) parsedFiles
      pure (and successResults)
    else do
      mapM_ putStrLn parseErrors
      pure False
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
  entries <- listDirectory directoryPath
  nestedFiles <-
    mapM
      ( \entryName -> do
          let entryPath = directoryPath </> entryName
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
  directoryName == ".git"
    || directoryName == ".agents"
    || directoryName == ".codex"
    || directoryName == "result"
    || directoryName == "tmp"
    || directoryName == "prm"
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
parseRepositoryFile :: FilePath -> IO (Either String ParsedNixFile)
parseRepositoryFile filePath = do
  parseResult <- parseNixFileLoc (Path filePath)
  case parseResult of
    Left parseError -> do
      pure (Left ("Error parsing " ++ filePath ++ ": " ++ show parseError))
    Right expr -> pure (Right (filePath, expr))
repositoryParseErrors :: [Either String ParsedNixFile] -> [String]
repositoryParseErrors [] = []
repositoryParseErrors (Left parseError : remainingResults) = parseError : repositoryParseErrors remainingResults
repositoryParseErrors (Right _ : remainingResults) = repositoryParseErrors remainingResults
repositoryParsedFiles :: [Either String ParsedNixFile] -> [ParsedNixFile]
repositoryParsedFiles [] = []
repositoryParsedFiles (Left _ : remainingResults) = repositoryParsedFiles remainingResults
repositoryParsedFiles (Right parsedFile : remainingResults) = parsedFile : repositoryParsedFiles remainingResults
processParsedRepositoryFile :: FilePath -> FilePath -> Map NixosCandidate [FilePath] -> Map OptionPath Literal -> ParsedNixFile -> IO Bool
processParsedRepositoryFile repositoryRoot flakeSourcePath nixosDefinitionFiles treefmtDefaults (filePath, expr) = do
  let nixosRemovals = nixosRemovalsForFile repositoryRoot flakeSourcePath filePath nixosDefinitionFiles (collectCandidates expr)
      treefmtRemovals = removalsFromDefaults treefmtDefaults (collectTreefmtCandidates expr)
      changed = any snd (Map.toList nixosRemovals) || any snd (Map.toList treefmtRemovals)
      transformed =
        rewriteTreefmtEvalModuleArguments
          treefmtRemovals
          (rewriteModuleExpression nixosRemovals expr)
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
resolveLiteralRemovals :: DefaultResolver -> [(OptionPath, Literal)] -> IO (Map OptionPath Bool)
resolveLiteralRemovals resolveDefault candidates = do
  resolutions <-
    mapM
      ( \(optionPath, literalValue) -> do
          defaultValue <- resolveDefault optionPath
          pure (optionPath, defaultValue == Just literalValue)
      )
      candidates
  pure (Map.fromList resolutions)
removalsFromDefaults :: Map OptionPath Literal -> [(OptionPath, Literal)] -> Map OptionPath Bool
removalsFromDefaults defaults candidates =
  Map.fromList
    [ (optionPath, Map.lookup optionPath defaults == Just literalValue)
    | (optionPath, literalValue) <- candidates
    ]
uniqueCandidates :: [NixosCandidate] -> [NixosCandidate]
uniqueCandidates = nub
uniqueCandidatePaths :: [NixosCandidate] -> [OptionPath]
uniqueCandidatePaths candidates = nub (map fst candidates)
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
rewriteTreefmtEvalModuleArguments :: Map OptionPath Bool -> NExprLoc -> NExprLoc
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
flakeSourcePathForRepository :: FilePath -> IO FilePath
flakeSourcePathForRepository repositoryRoot = do
  maybeSourcePath <- readStringFromNix ["eval", "--impure", "--json", "--expr", flakeSourcePathExpression repositoryRoot]
  case maybeSourcePath of
    Just sourcePath -> pure sourcePath
    Nothing -> pure repositoryRoot
nixosConfigurationsForRepository :: FilePath -> IO [String]
nixosConfigurationsForRepository repositoryRoot = do
  maybeConfigurationNames <- readStringListFromNix ["eval", "--impure", "--json", "--expr", nixosConfigurationsExpression repositoryRoot]
  case maybeConfigurationNames of
    Just configurationNames -> pure configurationNames
    Nothing -> pure []
resolveNixosDefinitionFiles :: FilePath -> [String] -> [NixosCandidate] -> IO (Map NixosCandidate [FilePath])
resolveNixosDefinitionFiles _ [] _ = pure Map.empty
resolveNixosDefinitionFiles _ _ [] = pure Map.empty
resolveNixosDefinitionFiles repositoryRoot nixosConfigurations candidates = do
  definitionMaps <-
    mapM
      ( \configurationName ->
          resolveNixosDefinitionFilesForConfiguration repositoryRoot configurationName candidates
      )
      nixosConfigurations
  pure (Map.unionsWith (++) definitionMaps)
resolveNixosDefinitionFilesForConfiguration :: FilePath -> String -> [NixosCandidate] -> IO (Map NixosCandidate [FilePath])
resolveNixosDefinitionFilesForConfiguration _ _ [] = pure Map.empty
resolveNixosDefinitionFilesForConfiguration repositoryRoot configurationName candidates = do
  maybeDefinitionFiles <- readNixosDefinitionFilesFromNix ["eval", "--impure", "--json", "--expr", nixosDefaultDefinitionFilesExpression repositoryRoot [configurationName] candidates]
  case maybeDefinitionFiles of
    Just definitionFiles -> pure (Map.fromListWith (++) definitionFiles)
    Nothing ->
      case candidates of
        [_] -> pure Map.empty
        _ -> do
          let (leftCandidates, rightCandidates) = splitAlternating candidates
          leftDefinitionFiles <- resolveNixosDefinitionFilesForConfiguration repositoryRoot configurationName leftCandidates
          rightDefinitionFiles <- resolveNixosDefinitionFilesForConfiguration repositoryRoot configurationName rightCandidates
          pure (Map.unionWith (++) leftDefinitionFiles rightDefinitionFiles)
splitAlternating :: [a] -> ([a], [a])
splitAlternating [] = ([], [])
splitAlternating [value] = ([value], [])
splitAlternating (leftValue : rightValue : remainingValues) =
  let (leftValues, rightValues) = splitAlternating remainingValues
   in (leftValue : leftValues, rightValue : rightValues)
nixosRemovalsForFile :: FilePath -> FilePath -> FilePath -> Map NixosCandidate [FilePath] -> [NixosCandidate] -> Map OptionPath Bool
nixosRemovalsForFile repositoryRoot flakeSourcePath localFilePath nixosDefinitionFiles candidates =
  Map.fromListWith
    (||)
    [ (optionPath, any (sourceLocationMatches repositoryRoot flakeSourcePath localFilePath) (Map.findWithDefault [] candidate nixosDefinitionFiles))
    | candidate@(optionPath, _) <- uniqueCandidates candidates
    ]
resolveTreefmtDefaults :: FilePath -> [OptionPath] -> IO (Map OptionPath Literal)
resolveTreefmtDefaults _ [] = pure Map.empty
resolveTreefmtDefaults repositoryRoot optionPaths = do
  maybeDefaults <- readTreefmtDefaultsFromNix ["eval", "--impure", "--json", "--expr", treefmtDefaultsExpression repositoryRoot optionPaths]
  case maybeDefaults of
    Just defaults -> pure (Map.fromList defaults)
    Nothing -> pure Map.empty
flakeSourcePathExpression :: FilePath -> String
flakeSourcePathExpression repositoryRoot =
  "(builtins.getFlake (toString (/. + " ++ nixString repositoryRoot ++ "))).outPath"
nixosConfigurationsExpression :: FilePath -> String
nixosConfigurationsExpression repositoryRoot =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); in builtins.attrNames (flake.nixosConfigurations or {})"
nixosDefaultDefinitionFilesExpression :: FilePath -> [String] -> [NixosCandidate] -> String
nixosDefaultDefinitionFilesExpression repositoryRoot configurationNames candidates =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); configurations = flake.nixosConfigurations or {}; configurationNames = "
    ++ renderStringList configurationNames
    ++ "; candidates = "
    ++ renderNixosCandidateList candidates
    ++ "; optionAt = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; remainingPath = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then optionAt value.${key} remainingPath else { success = false; }; literalEquals = expected: actual: if builtins.isNull expected then builtins.isNull actual else if builtins.isBool expected then builtins.isBool actual && actual == expected else if builtins.isInt expected then builtins.isInt actual && actual == expected else if builtins.isFloat expected then builtins.isFloat actual && actual == expected else if builtins.isString expected then builtins.isString actual && actual == expected else if builtins.isList expected then builtins.isList actual && builtins.length actual == builtins.length expected && builtins.all (index: literalEquals (builtins.elemAt expected index) (builtins.elemAt actual index)) (builtins.genList (index: index) (builtins.length expected)) else if builtins.isAttrs expected then builtins.isAttrs actual && builtins.attrNames expected == builtins.attrNames actual && builtins.all (name: literalEquals expected.${name} actual.${name}) (builtins.attrNames expected) else false; filesFor = configurationName: candidate: let evaluated = builtins.getAttr configurationName configurations; optionAttempt = optionAt evaluated.options candidate.path; raw = if optionAttempt.success && builtins.isAttrs optionAttempt.value && optionAttempt.value ? default && literalEquals candidate.value optionAttempt.value.default then builtins.map (definition: definition.file) (optionAttempt.value.definitionsWithLocations or []) else []; attempted = builtins.tryEval (builtins.deepSeq raw raw); in if attempted.success then attempted.value else []; candidateFiles = candidate: { inherit (candidate) path value; files = builtins.concatMap (configurationName: filesFor configurationName candidate) configurationNames; }; in builtins.map candidateFiles candidates"
treefmtDefaultsExpression :: FilePath -> [OptionPath] -> String
treefmtDefaultsExpression repositoryRoot optionPaths =
  "let flake = builtins.getFlake (toString (/. + "
    ++ nixString repositoryRoot
    ++ ")); pkgs = import flake.inputs.nixpkgs { system = builtins.currentSystem; }; evaluated = flake.inputs.treefmt-nix.lib.evalModule pkgs {}; optionPaths = "
    ++ renderOptionPathList optionPaths
    ++ "; optionAt = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; remainingPath = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then optionAt value.${key} remainingPath else { success = false; }; defaultFor = path: let optionAttempt = optionAt evaluated.options path; raw = if optionAttempt.success && builtins.isAttrs optionAttempt.value && optionAttempt.value ? default then optionAttempt.value.default else throw \"missing option default\"; attempted = builtins.tryEval (builtins.deepSeq raw raw); in if attempted.success then [{ inherit path; default = attempted.value; }] else []; in builtins.concatMap defaultFor optionPaths"
renderStringList :: [String] -> String
renderStringList values =
  "[ " ++ intercalateStrings " " (map nixString values) ++ " ]"
renderOptionPathList :: [OptionPath] -> String
renderOptionPathList optionPaths =
  "[ " ++ intercalateStrings " " (map renderOptionPathAsList optionPaths) ++ " ]"
renderNixosCandidateList :: [NixosCandidate] -> String
renderNixosCandidateList candidates =
  "[ " ++ intercalateStrings " " (map renderNixosCandidate candidates) ++ " ]"
renderNixosCandidate :: NixosCandidate -> String
renderNixosCandidate (optionPath, literalValue) =
  "{ path = " ++ renderOptionPathAsList optionPath ++ "; value = " ++ renderLiteral literalValue ++ "; }"
renderOptionPathAsList :: OptionPath -> String
renderOptionPathAsList optionPath =
  "[ " ++ intercalateStrings " " (map (nixString . unpack) optionPath) ++ " ]"
renderLiteral :: Literal -> String
renderLiteral LiteralNull = "null"
renderLiteral (LiteralBool True) = "true"
renderLiteral (LiteralBool False) = "false"
renderLiteral (LiteralInteger value) = show value
renderLiteral (LiteralFloat value) = show value
renderLiteral (LiteralString value) = nixString (unpack value)
renderLiteral (LiteralList values) =
  "[ " ++ intercalateStrings " " (map renderLiteral values) ++ " ]"
renderLiteral (LiteralSet bindings) =
  "{ " ++ intercalateStrings " " (map renderLiteralBinding bindings) ++ " }"
renderLiteralBinding :: (Text, Literal) -> String
renderLiteralBinding (key, value) =
  nixString (unpack key) ++ " = " ++ renderLiteral value ++ ";"
intercalateStrings :: String -> [String] -> String
intercalateStrings _ [] = ""
intercalateStrings _ [value] = value
intercalateStrings separator (value : remainingValues) = value ++ separator ++ intercalateStrings separator remainingValues
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
  | prefixWithSeparator `isPrefixOf` path = Just (dropPrefix prefixWithSeparator path)
  | otherwise = Nothing
  where
    prefixWithSeparator = prefix ++ [pathSeparator]
stripStoreSourcePrefix :: FilePath -> Maybe FilePath
stripStoreSourcePrefix path
  | storeSourceMarker `isPrefixOf` path = Just (dropPrefix storeSourceMarker path)
  | otherwise =
      case path of
        _ : remainingPath -> stripStoreSourcePrefix remainingPath
        [] -> Nothing
  where
    storeSourceMarker = "-source" ++ [pathSeparator]
dropPrefix :: String -> String -> String
dropPrefix [] path = path
dropPrefix (_ : remainingPrefix) (_ : remainingPath) = dropPrefix remainingPrefix remainingPath
dropPrefix _ [] = []
nixString :: String -> String
nixString = unpack . renderOptionKey . pack
readStringFromNix :: [String] -> IO (Maybe String)
readStringFromNix arguments = do
  maybeJsonValue <- readJsonFromNix arguments
  pure (maybeJsonValue >>= jsonString)
readStringListFromNix :: [String] -> IO (Maybe [String])
readStringListFromNix arguments = do
  maybeJsonValue <- readJsonFromNix arguments
  pure (maybeJsonValue >>= jsonStringList)
readNixosDefinitionFilesFromNix :: [String] -> IO (Maybe [(NixosCandidate, [FilePath])])
readNixosDefinitionFilesFromNix arguments = do
  maybeJsonValue <- readJsonFromNix arguments
  pure (maybeJsonValue >>= jsonNixosDefinitionFiles)
readTreefmtDefaultsFromNix :: [String] -> IO (Maybe [(OptionPath, Literal)])
readTreefmtDefaultsFromNix arguments = do
  maybeJsonValue <- readJsonFromNix arguments
  pure (maybeJsonValue >>= jsonTreefmtDefaults)
readJsonFromNix :: [String] -> IO (Maybe Value)
readJsonFromNix arguments = do
  (exitCode, standardOutput, _standardError) <-
    readProcessWithExitCode "nix" arguments ""
  case exitCode of
    ExitSuccess ->
      case eitherDecodeStrict' (BS.pack standardOutput) of
        Right jsonValue -> pure (Just jsonValue)
        Left _ -> pure Nothing
    _ -> pure Nothing
renderOptionKey :: Text -> Text
renderOptionKey = Text.decodeUtf8 . LBS.toStrict . encode
jsonString :: Value -> Maybe String
jsonString (String value) = Just (unpack value)
jsonString _ = Nothing
jsonStringList :: Value -> Maybe [String]
jsonStringList (Array values) = mapM jsonString (toList values)
jsonStringList _ = Nothing
jsonText :: Value -> Maybe Text
jsonText (String value) = Just value
jsonText _ = Nothing
jsonOptionPath :: Value -> Maybe OptionPath
jsonOptionPath (Array values) = mapM jsonText (toList values)
jsonOptionPath _ = Nothing
jsonNixosDefinitionFiles :: Value -> Maybe [(NixosCandidate, [FilePath])]
jsonNixosDefinitionFiles (Array values) = mapM jsonNixosDefinitionFile (toList values)
jsonNixosDefinitionFiles _ = Nothing
jsonNixosDefinitionFile :: Value -> Maybe (NixosCandidate, [FilePath])
jsonNixosDefinitionFile (Object values) = do
  optionPath <- KeyMap.lookup (Key.fromText "path") values >>= jsonOptionPath
  literalValue <- KeyMap.lookup (Key.fromText "value") values >>= jsonLiteral
  sourceFiles <- KeyMap.lookup (Key.fromText "files") values >>= jsonStringList
  pure ((optionPath, literalValue), sourceFiles)
jsonNixosDefinitionFile _ = Nothing
jsonTreefmtDefaults :: Value -> Maybe [(OptionPath, Literal)]
jsonTreefmtDefaults (Array values) = mapM jsonTreefmtDefault (toList values)
jsonTreefmtDefaults _ = Nothing
jsonTreefmtDefault :: Value -> Maybe (OptionPath, Literal)
jsonTreefmtDefault (Object values) = do
  optionPath <- KeyMap.lookup (Key.fromText "path") values >>= jsonOptionPath
  defaultValue <- KeyMap.lookup (Key.fromText "default") values >>= jsonLiteral
  pure (optionPath, defaultValue)
jsonTreefmtDefault _ = Nothing
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
formatTreefmtWithDefaults :: Map OptionPath Literal -> Text -> IO Text
formatTreefmtWithDefaults defaults input =
  withSystemTempFile "nix-remove-defaults-treefmt-test.nix" $ \tmpFile tmpHandle -> do
    hClose tmpHandle
    TIO.writeFile tmpFile input
    parseResult <- parseNixFileLoc (Path tmpFile)
    case parseResult of
      Right expr -> do
        removals <- resolveLiteralRemovals (pure . (`Map.lookup` defaults)) (collectTreefmtCandidates expr)
        pure (renderExpression (rewriteTreefmtEvalModuleArguments removals expr))
      Left parseError -> assertFailure ("Test fixture failed to parse: " ++ show parseError)
makeRemovalTest :: String -> Map OptionPath Literal -> Text -> Text -> Test
makeRemovalTest testName defaults input expectedOutput = TestCase $ do
  actualOutput <- formatWithDefaults defaults input
  assertEqual testName expectedOutput actualOutput
makeTreefmtRemovalTest :: String -> Map OptionPath Literal -> Text -> Text -> Test
makeTreefmtRemovalTest testName defaults input expectedOutput = TestCase $ do
  actualOutput <- formatTreefmtWithDefaults defaults input
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
      makeTreefmtRemovalTest
        "Removes defaults inside a treefmt evalModule argument."
        (Map.singleton ["programs", "shfmt", "simplify"] (LiteralBool True))
        (pack "{ inputs, pkgs, ... }: let treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs { programs.shfmt.simplify = true; keep = false; }; in treefmtEval.config.build.wrapper")
        (pack "{ inputs, pkgs, ... }:\n  let\n    treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs { keep = false; };\n  in treefmtEval.config.build.wrapper"),
      TestCase $
        withSystemTempFile "nix-remove-defaults-direct-file-input.nix" $ \tmpFile tmpHandle -> do
          hClose tmpHandle
          repositoryRootResult <- repositoryRootFromArgument tmpFile
          assertEqual
            "Direct file input is rejected."
            (Left ("Expected a flake/repository directory, got a file: " ++ tmpFile))
            repositoryRootResult,
      TestCase $ do
        let expression =
              nixosDefaultDefinitionFilesExpression
                "/repository"
                ["default"]
                [(["boot", "initrd", "systemd", "enable"], LiteralBool True)]
        assertBool
          "Builds candidate records into the NixOS default-definition lookup expression."
          ("candidates = [ { path = [ \"boot\" \"initrd\" \"systemd\" \"enable\" ]; value = true; } ]" `isInfixOf` expression)
        assertBool
          "Compares defaults using the parsed literal shape."
          ("literalEquals = expected: actual:" `isInfixOf` expression)
        assertBool
          "Returns path, value, and matching definition files for each candidate."
          ("candidateFiles = candidate: { inherit (candidate) path value; files =" `isInfixOf` expression)
    ]
