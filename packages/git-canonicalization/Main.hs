{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Control.Applicative (some, (<|>))
import Control.Exception (IOException, finally, onException, try)
import Control.Monad (filterM, forM, forM_, guard, unless, void, when)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isAsciiLower, isDigit, isLower, isSpace, isUpper, ord, toLower, toUpper)
import Data.Either (fromRight, lefts, rights)
import Data.Fix (Fix (Fix))
import Data.Foldable (asum, toList)
import Data.Functor.Compose (Compose (Compose))
import Data.Generics (everything, mkQ)
import Data.Kind (Type)
import Data.List (dropWhileEnd, find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, sort, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Distribution.Fields.Field (Field (Field, Section), FieldLine (FieldLine), Name (Name), SectionArg (SecArgName))
import Distribution.Fields.Parser (readFields)
import Language.Haskell.Exts qualified as HS
import Network.URI (URIAuth (uriRegName), parseURI, uriAuthority, uriPath, uriScheme)
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (Inherit, NamedVar),
    NExprF (NAbs, NLet, NSet, NStr),
    NKeyName (DynamicKey, StaticKey),
    NString (DoubleQuoted),
    Params (Param, ParamSet),
    VarName (VarName),
  )
import Nix.Expr.Types.Annotated (AnnUnit (AnnUnit), NExprLoc, stripAnnotation)
import Nix.Parser (parseNixFileLoc)
import Nix.Pretty (prettyNix)
import Nix.Utils (Path (Path))
import Options.Applicative qualified as OA
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getTemporaryDirectory, listDirectory, makeAbsolute, pathIsSymbolicLink, removeFile, removePathForcibly, renameFile, withCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath (isAbsolute, (<.>), (</>))
import System.FilePath.Posix (makeRelative, splitDirectories, takeBaseName, takeDirectory, takeFileName)
import System.IO (hClose, hPutStr, hPutStrLn, openTempFile, stderr)
import System.Posix.Files qualified as Posix
import System.Posix.Process (executeFile)
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import TOML qualified
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertBool, assertEqual, assertFailure, runTestTT)
import Text.Regex.TDFA ((=~))
import Prelude
defaultAllowedNixDifferenceKeys :: Set.Set T.Text
defaultAllowedNixDifferenceKeys =
  Set.fromList
    [ "buildInputs",
      "cargoHash",
      "executableHaskellDepends",
      "executableToolDepends",
      "installCheckPhase",
      "nativeBuildInputs",
      "nativeCheckInputs",
      "nativeInstallCheckInputs",
      "passthru.canonicalizationDependencies",
      "postInstall",
      "passthru.rustCheckNativeBuildInputs",
      "meta",
      "propagatedBuildInputs",
      "runtimeInputs",
      "version"
    ]
type TemplateSpec :: Type
data TemplateSpec = TemplateSpec
  { templateName :: FilePath,
    templateMatches :: PackageKind -> String -> Bool,
    templateAllowedDifferenceKeys :: Set.Set T.Text,
    templateBaselineSource :: T.Text
  }
type CheckTemplateSpec :: Type
data CheckTemplateSpec = CheckTemplateSpec
  { checkTemplateName :: FilePath,
    checkTemplateMatches :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool,
    checkTemplateBaselineSource :: T.Text,
    checkTemplateComparisonMode :: CheckTemplateComparisonMode,
    checkTemplatePackageAssociation :: Maybe (String, [PackageKind])
  }
type CheckTemplateComparisonMode :: Type
data CheckTemplateComparisonMode
  = ExactCheckTemplate
  | StructuralCPackageVm
templateSpecs :: [TemplateSpec]
templateSpecs =
  [ TemplateSpec
      { templateName = "haskell_package_baseline",
        templateMatches = \_ nixSource -> "stdenv.mkDerivation" `isInfixOf` nixSource && "ghcWithPackages" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = haskellTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        templateMatches = \_ nixSource -> "rustPlatform.buildRustPackage" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.fromList ["cargoToml", "cargoTomlFormat", "passthru", "pname", "postPatch"] `Set.union` defaultAllowedNixDifferenceKeys,
        templateBaselineSource = rustTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "html_template",
        templateMatches = \_ nixSource -> "writeShellApplication" `isInfixOf` nixSource && "http-server" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.insert "text" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = htmlTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_latex_template",
        templateMatches = matchesPythonLaTeXTemplate,
        templateAllowedDifferenceKeys = Set.insert "passthru.canonicalizationDependencies" (Set.fromList ["meta", "propagatedBuildInputs", "pythonDeps", "version"]),
        templateBaselineSource = pythonLaTeXTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_application_template",
        templateMatches = matchesPythonPyPIApplicationTemplate,
        templateAllowedDifferenceKeys = Set.insert "passthru.canonicalizationDependencies" (Set.fromList ["installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"]),
        templateBaselineSource = pythonPyPIApplicationTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_template",
        templateMatches = matchesPythonPyPITemplate,
        templateAllowedDifferenceKeys = Set.insert "passthru.canonicalizationDependencies" (Set.fromList ["format", "installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"]),
        templateBaselineSource = pythonPyPITemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "binary_release_template",
        templateMatches = matchesBinaryReleaseTemplate,
        templateAllowedDifferenceKeys = Set.insert "passthru.canonicalizationDependencies" (Set.fromList ["installCheckPhase", "src", "version"]),
        templateBaselineSource = binaryReleaseTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_template",
        templateMatches = \_ nixSource -> "buildPythonPackage" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.insert "passthru.canonicalizationDependencies" (Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"]),
        templateBaselineSource = pythonTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "deploy_host_template",
        templateMatches = \_ nixSource ->
          "writeShellApplication" `isInfixOf` nixSource
            && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
        templateBaselineSource = deployHostTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "latex_template",
        templateMatches = \_ nixSource ->
          "stdenv.mkDerivation" `isInfixOf` nixSource
            && "latexmk -pdf ms.tex" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
        templateBaselineSource = latexTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "c_template",
        templateMatches = \_ nixSource ->
          "stdenv.mkDerivation" `isInfixOf` nixSource
            && "cc -o ${pname} main.c -std=c89" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]),
        templateBaselineSource = cTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "uncomment_template",
        templateMatches = \_ nixSource ->
          "stdenv.mkDerivation" `isInfixOf` nixSource
            && "autoPatchelfHook" `isInfixOf` nixSource
            && "Goldziher" `isInfixOf` nixSource,
        templateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["pname", "src"]),
        templateBaselineSource = uncommentTemplateBaselineNixSource
      }
  ]
matchesPythonLaTeXTemplate :: PackageKind -> String -> Bool
matchesPythonLaTeXTemplate packageKind nixSource =
  packageKind == PythonLaTeXPackage && "buildPythonPackage" `isInfixOf` nixSource
matchesPythonPyPITemplateLike :: String -> PackageKind -> String -> Bool
matchesPythonPyPITemplateLike buildFunction _ nixSource =
  buildFunction `isInfixOf` nixSource
    && isExternalPythonPackageSource (T.pack nixSource)
matchesPythonPyPIApplicationTemplate :: PackageKind -> String -> Bool
matchesPythonPyPIApplicationTemplate = matchesPythonPyPITemplateLike "buildPythonApplication"
matchesPythonPyPITemplate :: PackageKind -> String -> Bool
matchesPythonPyPITemplate = matchesPythonPyPITemplateLike "buildPythonPackage"
matchesBinaryReleaseTemplate :: PackageKind -> String -> Bool
matchesBinaryReleaseTemplate _ nixSource =
  "stdenv.mkDerivation" `isInfixOf` nixSource
    && "src = pkgs.fetchurl" `isInfixOf` nixSource
    && "sourceRoot = \".\";" `isInfixOf` nixSource
    && "install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname}" `isInfixOf` nixSource
matchesCheckNameSuffixAndSourceContains :: String -> [String] -> Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesCheckNameSuffixAndSourceContains suffix requiredNeedles _ checkName nixSource =
  suffix `isSuffixOf` checkName
    && all (`isInfixOf` nixSource) requiredNeedles
checkTemplateSpecs :: [CheckTemplateSpec]
checkTemplateSpecs =
  [ CheckTemplateSpec
      { checkTemplateName = "haskell_coverage_check",
        checkTemplateMatches = matchesHaskellCoverageCheck,
        checkTemplateBaselineSource = haskellCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate,
        checkTemplatePackageAssociation = Just ("-coverage", [HaskellPackage])
      },
    CheckTemplateSpec
      { checkTemplateName = "python_coverage_check",
        checkTemplateMatches = matchesPythonCoverageCheck,
        checkTemplateBaselineSource = pythonCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate,
        checkTemplatePackageAssociation = Just ("_coverage", [PythonPackage, PythonLaTeXPackage])
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_coverage_check",
        checkTemplateMatches = matchesRustCoverageCheck,
        checkTemplateBaselineSource = rustCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate,
        checkTemplatePackageAssociation = Just ("-coverage", [RustPackage])
      },
    CheckTemplateSpec
      { checkTemplateName = "c_package_vm_check",
        checkTemplateMatches = matchesCPackageVmCheck,
        checkTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkTemplateComparisonMode = StructuralCPackageVm,
        checkTemplatePackageAssociation = Nothing
      },
    CheckTemplateSpec
      { checkTemplateName = "default_vm_with_disko_check",
        checkTemplateMatches = matchesDefaultVmWithDiskoCheck,
        checkTemplateBaselineSource = defaultVmWithDiskoCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate,
        checkTemplatePackageAssociation = Nothing
      },
    CheckTemplateSpec
      { checkTemplateName = "host_default_check",
        checkTemplateMatches = \_ checkName _ -> checkName == "host_default",
        checkTemplateBaselineSource = hostDefaultCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate,
        checkTemplatePackageAssociation = Nothing
      }
  ]
matchesHaskellCoverageCheck :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesHaskellCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["ghcWithPackages", "-fhpc"]
matchesPythonCoverageCheck :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesPythonCoverageCheck = matchesCheckNameSuffixAndSourceContains "_coverage" ["--cov=\"$src\""]
matchesRustCoverageCheck :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesRustCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["cargo llvm-cov"]
matchesCPackageVmCheck :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesCPackageVmCheck packageKinds checkName nixSource =
  maybe False (`isCPackageVmCheckShape` nixSource) (Map.lookup checkName packageKinds)
matchesDefaultVmWithDiskoCheck :: Map.Map FilePath PackageKind -> FilePath -> String -> Bool
matchesDefaultVmWithDiskoCheck = matchesCheckNameSuffixAndSourceContains "VmWithDisko" ["pkgs.runCommand", "config.system.build.vmWithDisko"]
isCPackageVmCheckShape :: PackageKind -> String -> Bool
isCPackageVmCheckShape packageKind nixSource =
  packageKind == CPackage
    && "pkgs.testers.runNixOSTest" `isInfixOf` nixSource
    && "nodes.machine" `isInfixOf` nixSource
    && "testScript = ''" `isInfixOf` nixSource
type Command :: Type
data Command
  = CheckCommand
  | CheckFixCommand
  | StatusCommand
  | InitCommand InitSpec
  | AddPackageCommand String FilePath (Maybe String)
  | RemovePackageCommand RemoveSpec
  deriving stock (Eq, Show)
type InitSpec :: Type
data InitSpec = InitSpec
  { initLocalPath :: FilePath,
    initStatusSource :: Maybe FilePath
  }
  deriving stock (Eq, Show)
type StatusImport :: Type
data StatusImport = StatusImport
  { statusImportReadme :: Maybe String,
    statusImportResources :: [StatusImportResource]
  }
type StatusImportResource :: Type
data StatusImportResource
  = StatusImportPackage String String (Maybe String) [String] [String]
  | StatusImportHost String
  deriving stock (Eq, Show)
type StatusImportTests :: Type
newtype StatusImportTests = StatusImportTests
  { statusImportTestNames :: [String]
  }
instance Aeson.FromJSON StatusImportTests where
  parseJSON =
    Aeson.withObject "StatusImportTests" $ \object ->
      StatusImportTests . map statusImportTestCaseName <$> object Aeson..:? "cases" Aeson..!= []
type StatusImportTestCase :: Type
newtype StatusImportTestCase = StatusImportTestCase
  { statusImportTestCaseName :: String
  }
instance Aeson.FromJSON StatusImportTestCase where
  parseJSON = Aeson.withObject "StatusImportTestCase" (fmap StatusImportTestCase . (Aeson..: "name"))
instance Aeson.FromJSON StatusImport where
  parseJSON =
    Aeson.withObject "StatusImport" $ \object -> do
      repositoryType <- object Aeson..: "repositoryType"
      when (repositoryType /= ("flake" :: String)) (fail "repositoryType must be flake")
      StatusImport <$> object Aeson..: "readme" <*> object Aeson..: "resources"
instance Aeson.FromJSON StatusImportResource where
  parseJSON =
    Aeson.withObject "StatusImportResource" $ \object -> do
      kind <- object Aeson..: "kind"
      case (kind :: String) of
        "package" -> do
          packageName <- object Aeson..: "name"
          packageType <- object Aeson..: "type"
          packageDescription <- object Aeson..: "description"
          packageDependencies <- object Aeson..:? "dependencies" Aeson..!= []
          packageTests <- object Aeson..:? "tests"
          let packageTestNames = maybe [] statusImportTestNames packageTests
          pure (StatusImportPackage packageName packageType packageDescription packageDependencies packageTestNames)
        "host" -> StatusImportHost <$> object Aeson..: "name"
        _ -> fail ("unsupported resource kind: " ++ kind)
type RemoveSpec :: Type
data RemoveSpec = RemoveSpec
  { removePackageName :: FilePath,
    removeDryRun :: Bool,
    removeForce :: Bool
  }
  deriving stock (Eq, Show)
main :: IO ()
main = getArgs >>= runCli
runCli :: [String] -> IO ()
runCli commandLineArguments =
  case OA.execParserPure cliParserPreferences cliParserInfo (normalizeHelpArguments commandLineArguments) of
    OA.Success command -> runCommand command
    OA.Failure parserFailure -> do
      let (message, parserExitCode) = OA.renderFailure parserFailure "git canonicalization"
      if parserExitCode == ExitSuccess
        then putStr message >> exitSuccess
        else hPutStr stderr message >> exitWith usageExitCode
    OA.CompletionInvoked completion -> do
      completionText <- OA.execCompletion completion "git canonicalization"
      putStr completionText
      exitSuccess
runCommand :: Command -> IO ()
runCommand = \case
  CheckCommand -> withDetectedRepositoryProfile $ \repositoryRoot -> \case
    HomeProfile -> checkHomeProfile repositoryRoot False
    FlakeProfile -> checkRepositoryLocation repositoryRoot
  CheckFixCommand -> withDetectedRepositoryProfile $ \repositoryRoot -> \case
    HomeProfile -> checkHomeProfile repositoryRoot True
    FlakeProfile -> fixAndCheckRepositoryLocation repositoryRoot
  StatusCommand -> withDetectedRepositoryProfile $ \repositoryRoot -> \case
    HomeProfile -> renderHomeProfileStatus repositoryRoot
    FlakeProfile -> summarizeRepositoryLocation renderRepositorySummariesJSON repositoryRoot
  InitCommand initSpec -> initializeCanonicalization initSpec
  AddPackageCommand packageKindName packageName packageDescription ->
    withRequiredProfile FlakeProfile "package" $ \repositoryRoot ->
      withCurrentDirectory repositoryRoot $
        case parseSupportedAddPackageKind packageKindName of
          Nothing -> do
            hPutStrLn stderr ("error: unsupported package type: " ++ packageKindName)
            hPutStrLn stderr ("hint: supported package types: " ++ intercalate ", " (map fst supportedAddPackageKinds))
            exitFailure
          Just scaffoldPackageKind -> do
            addResult <- addPackageToCurrentRepository scaffoldPackageKind packageName packageDescription
            stageGeneratedPathsOrExit addResult
  RemovePackageCommand removeSpec ->
    withRequiredProfile FlakeProfile "package" $ \repositoryRoot ->
      withCurrentDirectory repositoryRoot $ do
        removeResult <- removePackageFromCurrentRepository removeSpec
        case removeResult of
          Left removeError -> hPutStrLn stderr ("error: " ++ removeError) >> exitFailure
          Right () -> exitSuccess
stageGeneratedPathsOrExit :: Either String [FilePath] -> IO a
stageGeneratedPathsOrExit = \case
  Left addError -> do
    hPutStrLn stderr ("error: " ++ addError)
    exitFailure
  Right generatedPaths -> delegateToGit (["add", "--"] ++ generatedPaths)
usageExitCode :: ExitCode
usageExitCode = ExitFailure 129
normalizeHelpArguments :: [String] -> [String]
normalizeHelpArguments = \case
  ["help"] -> ["--help"]
  ["help", commandName] -> [commandName, "--help"]
  arguments -> arguments
cliParserPreferences :: OA.ParserPrefs
cliParserPreferences = OA.prefs OA.showHelpOnError
cliParserInfo :: OA.ParserInfo Command
cliParserInfo =
  OA.info
    (OA.helper <*> commandParser)
    ( OA.fullDesc
        <> OA.progDesc "Check canonical home repositories and manage Nix flake repositories."
        <> OA.header "git canonicalization - canonical repository checks and scaffolding"
    )
parseCommandForTest :: [String] -> Maybe Command
parseCommandForTest arguments =
  case OA.execParserPure cliParserPreferences cliParserInfo (normalizeHelpArguments arguments) of
    OA.Success command -> Just command
    _ -> Nothing
commandParser :: OA.Parser Command
commandParser =
  OA.hsubparser
    ( command "init" "Initialize a canonical flake repository." initCommandParser
        <> command "status" "Write repository status as JSON." (pure StatusCommand)
        <> command "add" "Scaffold a package." addCommandParser
        <> command "rm" "Remove a package and its generated check." removeCommandParser
        <> command "check" "Check the selected repository." checkCommandParser
    )
  where
    command :: String -> String -> OA.Parser Command -> OA.Mod OA.CommandFields Command
    command name description parser = OA.command name (OA.info parser (OA.progDesc description))
initCommandParser :: OA.Parser Command
initCommandParser =
  InitCommand
    <$> ( InitSpec
            <$> OA.strArgument
              ( OA.metavar "DIRECTORY"
                  <> OA.value "."
                  <> OA.showDefault
                  <> OA.help "Flake repository directory."
              )
            <*> OA.optional
              ( OA.strOption
                  ( OA.long "from-status"
                      <> OA.metavar "FILE|-"
                      <> OA.help "Initialize resources from status JSON; - reads standard input."
                  )
              )
        )
addCommandParser :: OA.Parser Command
addCommandParser =
  AddPackageCommand
    <$> OA.strArgument (OA.metavar "TYPE")
    <*> OA.strArgument (OA.metavar "NAME")
    <*> OA.optional (unwords <$> some (OA.strArgument (OA.metavar "DESCRIPTION")))
removeCommandParser :: OA.Parser Command
removeCommandParser =
  RemovePackageCommand
    <$> ( RemoveSpec
            <$> OA.strArgument (OA.metavar "PACKAGE_NAME")
            <*> OA.switch (OA.short 'n' <> OA.long "dry-run" <> OA.help "Show what would be removed.")
            <*> OA.switch (OA.short 'f' <> OA.long "force" <> OA.help "Allow removal with local changes.")
        )
checkCommandParser :: OA.Parser Command
checkCommandParser =
  flagToCommand <$> OA.switch (OA.long "fix" <> OA.help "Repair managed files before checking.")
  where
    flagToCommand True = CheckFixCommand
    flagToCommand False = CheckCommand
type RepositoryProfile :: Type
data RepositoryProfile = HomeProfile | FlakeProfile deriving stock (Eq, Show)
withDetectedRepositoryProfile :: (FilePath -> RepositoryProfile -> IO a) -> IO a
withDetectedRepositoryProfile action = do
  repositoryRoot <- discoverGitRepositoryRoot "."
  profile <- detectRepositoryProfile repositoryRoot
  action repositoryRoot profile
withRequiredProfile :: RepositoryProfile -> String -> (FilePath -> IO a) -> IO a
withRequiredProfile requiredProfile resourceKind action =
  withDetectedRepositoryProfile $ \repositoryRoot actualProfile ->
    if actualProfile == requiredProfile
      then action repositoryRoot
      else unsupportedResource (renderRepositoryProfile actualProfile) resourceKind
detectRepositoryProfile :: FilePath -> IO RepositoryProfile
detectRepositoryProfile = detectRepositoryProfileWithDefault Nothing
detectRepositoryProfileWithDefault :: Maybe RepositoryProfile -> FilePath -> IO RepositoryProfile
detectRepositoryProfileWithDefault emptyDefault repositoryRoot = withCurrentDirectory repositoryRoot $ do
  flakeMarkerExists <- or <$> mapM doesPathExist ["flake.nix", "flake.lock", "packages", "checks", "hosts"]
  gitmodulesExists <- doesPathExist ".gitmodules"
  gitignoreSource <- readTextFileIfExists ".gitignore"
  let homeMarkerExists = gitmodulesExists || maybe False (elem "!/.gitmodules" . T.lines) gitignoreSource
  case (homeMarkerExists, flakeMarkerExists) of
    (True, False) -> pure HomeProfile
    (False, True) -> pure FlakeProfile
    (False, False) ->
      case emptyDefault of
        Just profile -> pure profile
        Nothing -> hPutStrLn stderr ("error: cannot determine the repository type; run 'git canonicalization init " ++ repositoryRoot ++ "'") >> exitFailure
    (True, True) -> hPutStrLn stderr "error: repository contains markers for both home and flake layouts" >> exitFailure
renderRepositoryProfile :: RepositoryProfile -> String
renderRepositoryProfile = \case
  HomeProfile -> "home"
  FlakeProfile -> "flake"
unsupportedResource :: String -> String -> IO a
unsupportedResource repositoryType resourceKind =
  hPutStrLn stderr ("error: a " ++ repositoryType ++ " repository does not support " ++ resourceKind ++ " resources") >> exitFailure
homeGitignoreSource :: T.Text
homeGitignoreSource = "*\n!/.gitignore\n!/.gitmodules\n"
homeRequiredGitignorePatterns :: [T.Text]
homeRequiredGitignorePatterns =
  [ "!/.gitignore",
    "!/.gitmodules"
  ]
homeDirectory :: IO FilePath
homeDirectory =
  lookupEnv "HOME" >>= \case
    Just directory
      | isAbsolute directory -> canonicalizePath directory
      | otherwise -> hPutStrLn stderr "error: HOME must be an absolute path" >> exitFailure
    Nothing -> hPutStrLn stderr "error: HOME is not set" >> exitFailure
minimalProjectFlakeSource :: T.Text
minimalProjectFlakeSource =
  T.unlines
    [ "{",
      "  inputs = {",
      "    canonicalization.url = \"github:pbizopoulos/canonicalization\";",
      "    nixpkgs.follows = \"canonicalization/nixpkgs\";",
      "  };",
      "  outputs =",
      "    inputs:",
      "    inputs.canonicalization.blueprint {",
      "      inherit inputs;",
      "    }",
      "    // {",
      "      inherit (inputs.canonicalization) formatter;",
      "    };",
      "}"
    ]
initializeCanonicalization :: InitSpec -> IO ()
initializeCanonicalization initSpec = do
  let specifiedPath = initLocalPath initSpec
  importedStatus <- traverse readStatusImport (initStatusSource initSpec)
  home <- homeDirectory
  targetExists <- doesPathExist specifiedPath
  targetIsDirectory <- doesDirectoryExist specifiedPath
  when (targetExists && not targetIsDirectory) $ do
    hPutStrLn stderr ("error: initialization target exists and is not a directory: " ++ specifiedPath)
    exitFailure
  target <- if targetExists then canonicalizePath specifiedPath else makeAbsolute specifiedPath
  when (target == home) $ do
    hPutStrLn stderr "error: cannot initialize the home directory as a flake repository"
    hPutStrLn stderr "hint: initialize the home repository with Git as documented in README"
    exitFailure
  initializeProjectRepository targetExists home target
  forM_ importedStatus $ \statusImport -> withCurrentDirectory target $ do
    initializeStatusReadme statusImport
    initializeStatusResources statusImport
    renderRootGitignoreFromCurrentRepository >>= writeTextFileAtomically ".gitignore" >>= \case
      Left writeError -> hPutStrLn stderr ("error: could not update .gitignore: " ++ writeError) >> exitFailure
      Right () -> pure ()
readStatusImport :: FilePath -> IO StatusImport
readStatusImport statusFile = do
  source <- if statusFile == "-" then getContents else readFile statusFile
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack source)) of
    Left decodeError -> hPutStrLn stderr ("error: invalid status JSON: " ++ decodeError) >> exitFailure
    Right statusImport -> validateStatusImport statusImport >> pure statusImport
validateStatusImport :: StatusImport -> IO ()
validateStatusImport statusImport =
  forM_ (statusImportResources statusImport) $ \case
    StatusImportPackage _ packageType _ _ _
      | isNothing (parseSupportedAddPackageKind packageType) -> hPutStrLn stderr ("error: cannot scaffold package type from status: " ++ packageType) >> exitFailure
    StatusImportHost hostName
      | not (isDelimitedLowercaseName '-' hostName) -> hPutStrLn stderr "error: host name must use kebab-case" >> exitFailure
    _ -> pure ()
initializeStatusResources :: StatusImport -> IO ()
initializeStatusResources statusImport =
  forM_ (statusImportResources statusImport) $ \case
    StatusImportPackage packageName packageType packageDescription packageDependencies packageTestNames ->
      case parseSupportedAddPackageKind packageType of
        Nothing -> hPutStrLn stderr ("error: cannot scaffold package type from status: " ++ packageType) >> exitFailure
        Just packageKind ->
          addPackageToCurrentRepositoryWithForce True packageKind packageName packageDescription >>= \case
            Left addError -> hPutStrLn stderr ("error: " ++ addError) >> exitFailure
            Right _ -> applyStatusPackageMetadata packageKind packageName packageDescription packageDependencies packageTestNames
    StatusImportHost hostName -> createHostFromStatus hostName
initializeStatusReadme :: StatusImport -> IO ()
initializeStatusReadme statusImport =
  forM_ (statusImportReadme statusImport) $ \readmeSource -> do
    readmeExists <- doesPathExist "README"
    unless readmeExists (TIO.writeFile "README" (T.pack readmeSource))
applyStatusPackageMetadata :: PackageKind -> FilePath -> Maybe String -> [String] -> [String] -> IO ()
applyStatusPackageMetadata packageKind packageName packageDescription packageDependencies packageTestNames = do
  let defaultNixPath = "packages" </> packageName </> "default.nix"
  defaultNixSource <- TIO.readFile defaultNixPath
  let withoutDefaultDescription =
        if isNothing packageDescription
          then T.unlines (filter (not . T.isInfixOf "description =") (T.lines defaultNixSource))
          else defaultNixSource
      withDependencies =
        if null packageDependencies
          then withoutDefaultDescription
          else addStatusPackageDependencies packageDependencies (ensureStatusPackageInputsArgument withoutDefaultDescription)
  TIO.writeFile defaultNixPath withDependencies
  writeStatusPackageTests packageKind packageName packageTestNames
ensureStatusPackageInputsArgument :: T.Text -> T.Text
ensureStatusPackageInputsArgument source =
  case T.stripPrefix "{\n" source of
    Just remainingSource
      | not ("  inputs," `T.isPrefixOf` remainingSource) -> "{\n  inputs,\n" <> remainingSource
    _ -> source
addStatusPackageDependencies :: [String] -> T.Text -> T.Text
addStatusPackageDependencies packageDependencies source =
  case T.unsnoc (T.stripEnd source) of
    Just (sourceWithoutFinalBrace, '}') ->
      sourceWithoutFinalBrace
        <> "  passthru.canonicalizationDependencies = [\n"
        <> T.concat ["    inputs.self.packages.${pkgs.stdenv.system}." <> T.pack dependency <> "\n" | dependency <- packageDependencies]
        <> "  ];\n}\n"
    _ -> error "generated package default.nix must end with a derivation attribute set"
writeStatusPackageTests :: PackageKind -> FilePath -> [String] -> IO ()
writeStatusPackageTests packageKind packageName testNames =
  case packageKind of
    HaskellPackage -> TIO.writeFile ("packages" </> packageName </> "Main.hs") (renderStatusHaskellMainSource testNames)
    RustPackage -> TIO.writeFile ("packages" </> packageName </> "src/main.rs") (renderStatusRustMainSource testNames)
    PythonPackage -> TIO.writeFile ("packages" </> packageName </> "main.py") (renderStatusPythonMainSource testNames)
    PythonLaTeXPackage -> TIO.writeFile ("packages" </> packageName </> "main.py") (renderStatusPythonMainSource testNames)
    _ -> pure ()
renderStatusPythonMainSource :: [String] -> T.Text
renderStatusPythonMainSource testNames =
  T.unlines
    ( [ "#!/usr/bin/env python3",
        "\"\"\"Implementation placeholders imported from repository status.\"\"\"",
        "",
        "from __future__ import annotations",
        "",
        "",
        "def main() -> None:",
        "    message = \"implementation pending\"",
        "    raise NotImplementedError(message)",
        ""
      ]
        ++ concatMap renderPythonStatusTest (zip (statusTestIdentifiers "test" testNames) testNames)
        ++ ["", "if __name__ == \"__main__\":", "    main()"]
    )
renderPythonStatusTest :: (String, String) -> [T.Text]
renderPythonStatusTest (testIdentifier, testName) =
  [ "",
    T.pack ("def " ++ testIdentifier ++ "() -> None:"),
    "    " <> T.pack (renderJSON testName),
    "    message = \"TODO: implement imported contract\"",
    "    raise AssertionError(message)"
  ]
renderStatusHaskellMainSource :: [String] -> T.Text
renderStatusHaskellMainSource testNames =
  T.unlines
    ( [ "{-# LANGUAGE Trustworthy #-}",
        "{-# OPTIONS_GHC -Wno-unsafe #-}",
        "module Main (main, runPackageTests) where",
        "import System.Exit (exitFailure)",
        "import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertFailure, runTestTT)",
        "",
        "runPackageTests :: IO ()",
        "runPackageTests = do",
        "  counts <- runTestTT hUnitPackageTests",
        "  if errors counts == 0 && failures counts == 0 then putStrLn \"test ... ok\" else exitFailure",
        "",
        "hUnitPackageTests :: Test",
        "hUnitPackageTests = TestList"
      ]
        ++ renderHaskellStatusTests testNames
        ++ ["", "main :: IO ()", "main = exitFailure"]
    )
renderHaskellStatusTests :: [String] -> [T.Text]
renderHaskellStatusTests testNames =
  case testNames of
    [] -> ["  []"]
    firstTestName : remainingTestNames ->
      ("  [ TestLabel " <> T.pack (show firstTestName) <> " (TestCase (assertFailure \"TODO: implement imported contract\"))")
        : ["  , TestLabel " <> T.pack (show testName) <> " (TestCase (assertFailure \"TODO: implement imported contract\"))" | testName <- remainingTestNames]
        ++ ["  ]"]
renderStatusRustMainSource :: [String] -> T.Text
renderStatusRustMainSource testNames =
  T.unlines
    ( [ "fn main() {",
        "    panic!(\"implementation pending\");",
        "}",
        "",
        "#[cfg(test)]",
        "mod tests {",
        "    #[allow(unused_imports)]",
        "    use super::*;"
      ]
        ++ concatMap renderRustStatusTest (zip (statusTestIdentifiers "" testNames) testNames)
        ++ ["}"]
    )
renderRustStatusTest :: (String, String) -> [T.Text]
renderRustStatusTest (testIdentifier, _) =
  [ "",
    "    #[test]",
    T.pack ("    fn " ++ testIdentifier ++ "() {"),
    "        panic!(\"TODO: implement imported contract\");",
    "    }"
  ]
statusTestIdentifiers :: String -> [String] -> [String]
statusTestIdentifiers prefix = go Map.empty
  where
    go :: Map.Map String Int -> [String] -> [String]
    go _ [] = []
    go counts (testName : remainingTestNames) =
      let baseIdentifier = intercalate "_" (filter (not . null) (prefix : wordsFromTestSpecification testName))
          occurrence = Map.findWithDefault 0 baseIdentifier counts
          identifier = if occurrence == 0 then baseIdentifier else baseIdentifier ++ "_" ++ show (occurrence + 1)
       in identifier : go (Map.insert baseIdentifier (occurrence + 1) counts) remainingTestNames
wordsFromTestSpecification :: String -> [String]
wordsFromTestSpecification = concatMap splitCamelCaseWord . splitTestSpecificationTokens
splitTestSpecificationTokens :: String -> [String]
splitTestSpecificationTokens = reverse . go [] []
  where
    go :: [String] -> String -> String -> [String]
    go completed reversedWord [] = finishWord completed reversedWord
    go completed reversedWord (character : remainingCharacters)
      | isAlphaNum character = go completed (character : reversedWord) remainingCharacters
      | otherwise = go (finishWord completed reversedWord) [] remainingCharacters
    finishWord :: [String] -> String -> [String]
    finishWord completed [] = completed
    finishWord completed reversedWord = reverse reversedWord : completed
splitCamelCaseWord :: String -> [String]
splitCamelCaseWord = map (map toLower) . reverse . go [] []
  where
    go :: [String] -> String -> String -> [String]
    go completed reversedWord [] = finishWord completed reversedWord
    go completed reversedWord (character : remainingCharacters)
      | startsNewWord reversedWord character remainingCharacters = go (finishWord completed reversedWord) [character] remainingCharacters
      | otherwise = go completed (character : reversedWord) remainingCharacters
    finishWord :: [String] -> String -> [String]
    finishWord completed [] = completed
    finishWord completed reversedWord = reverse reversedWord : completed
    startsNewWord :: String -> Char -> String -> Bool
    startsNewWord [] _ _ = False
    startsNewWord (previousCharacter : _) character remainingCharacters =
      isUpper character
        && (isLower previousCharacter || (isUpper previousCharacter && maybe False isLower (listToMaybe remainingCharacters)))
createHostFromStatus :: FilePath -> IO ()
createHostFromStatus hostName
  | not (isDelimitedLowercaseName '-' hostName) = hPutStrLn stderr "error: host name must use kebab-case" >> exitFailure
  | otherwise = do
      let hostPath = "hosts" </> hostName
          configurationPath = hostPath </> "configuration.nix"
      exists <- doesPathExist hostPath
      when exists $ hPutStrLn stderr ("error: path already exists: " ++ hostPath) >> exitFailure
      createDirectoryIfMissing True hostPath
      TIO.writeFile configurationPath hostConfigurationScaffoldSource
      renderRootGitignoreFromCurrentRepository >>= writeTextFileAtomically ".gitignore" >>= \case
        Left writeError -> hPutStrLn stderr ("error: could not update .gitignore: " ++ writeError) >> exitFailure
        Right () -> pure ()
hostConfigurationScaffoldSource :: T.Text
hostConfigurationScaffoldSource =
  T.unlines
    [ "{ ... }:",
      "{",
      "  networking.hostName = baseNameOf ./.;",
      "  system.stateVersion = \"25.11\";",
      "}",
      ""
    ]
initializeProjectRepository :: Bool -> FilePath -> FilePath -> IO ()
initializeProjectRepository targetExisted home repositoryRoot = do
  validateProjectLocation home repositoryRoot
  gitmodulesExists <- doesPathExist (repositoryRoot </> ".gitmodules")
  let gitignorePath = repositoryRoot </> ".gitignore"
  gitignoreExists <- doesPathExist gitignorePath
  gitignoreSource <- readTextFileIfExists gitignorePath
  let hasHomeGitignoreMarker = maybe False (elem "!/.gitmodules" . T.lines) gitignoreSource
  when (gitmodulesExists || hasHomeGitignoreMarker) $ do
    hPutStrLn stderr "error: cannot initialize the flake layout because home layout markers exist"
    exitFailure
  let flakePath = repositoryRoot </> "flake.nix"
      lockPath = repositoryRoot </> "flake.lock"
  flakeExists <- doesPathExist flakePath
  lockExists <- doesPathExist lockPath
  gitMetadataExisted <- doesPathExist (repositoryRoot </> ".git")
  let createdPaths =
        [flakePath | not flakeExists]
          ++ [lockPath | not lockExists]
          ++ [gitignorePath | not gitignoreExists]
          ++ [repositoryRoot </> ".git" | targetExisted && not gitMetadataExisted]
  ( do
      createDirectoryIfMissing True repositoryRoot
      expectedGitignore <- withCurrentDirectory repositoryRoot renderInitializedProjectGitignore
      unless flakeExists (TIO.writeFile flakePath minimalProjectFlakeSource)
      unless lockExists $ do
        hPutStrLn stderr "Locking flake inputs..."
        withCurrentDirectory repositoryRoot (runNixOrExit ["flake", "lock", "path:."])
      runGitInitialization repositoryRoot
      unless gitignoreExists (TIO.writeFile gitignorePath expectedGitignore)
    )
    `onException` rollbackInitialization targetExisted repositoryRoot createdPaths
runGitInitialization :: FilePath -> IO ()
runGitInitialization repositoryRoot =
  runGitOrExit ["init", "--", repositoryRoot]
rollbackInitialization :: Bool -> FilePath -> [FilePath] -> IO ()
rollbackInitialization targetExisted repositoryRoot createdPaths = do
  forM_ createdPaths $ \path -> doesPathExist path >>= \exists -> when exists (removePathForcibly path)
  unless targetExisted (doesPathExist repositoryRoot >>= \exists -> when exists (removePathForcibly repositoryRoot))
renderInitializedProjectGitignore :: IO T.Text
renderInitializedProjectGitignore = do
  repositoryEntries <- collectStructurallyAllowedRepositoryEntriesWith []
  pure (renderRootGitignore ("flake.nix" : "flake.lock" : concatMap whitelistPathsForRepositoryEntry repositoryEntries))
runNixOrExit :: [String] -> IO ()
runNixOrExit arguments = do
  nixExecutable <- fromMaybe "nix" <$> lookupEnv "GIT_CANONICALIZATION_NIX"
  _ <- runProcessOrExit SuppressSuccessfulOutput nixExecutable arguments
  pure ()
validateProjectLocation :: FilePath -> FilePath -> IO ()
validateProjectLocation home repositoryRoot =
  when (isStrictDescendantOf home repositoryRoot) $ do
    ownsGitMetadata <- doesPathExist (repositoryRoot </> ".git")
    when ownsGitMetadata $ do
      maybeOrigin <- readGitOrigin repositoryRoot
      forM_ maybeOrigin $ \origin ->
        resolveGitRemoteUrl repositoryRoot origin >>= \resolvedOrigin ->
          case canonicalHomeRepositoryPath resolvedOrigin of
            Left originError -> hPutStrLn stderr ("error: origin: " ++ originError) >> exitFailure
            Right expectedRelativePath -> do
              let actualRelativePath = makeRelative home repositoryRoot
                  expectedPath = home </> expectedRelativePath
              when (actualRelativePath /= expectedRelativePath) $ do
                hPutStrLn stderr "error: project location does not match origin"
                hPutStrLn stderr ("actual:   " ++ repositoryRoot)
                hPutStrLn stderr ("expected: " ++ expectedPath)
                hPutStrLn stderr ("hint: from the home repository, run 'git submodule add --force " ++ origin ++ " " ++ expectedRelativePath ++ "'")
                exitFailure
              validateRegisteredHomeRepository home actualRelativePath expectedRelativePath
isStrictDescendantOf :: FilePath -> FilePath -> Bool
isStrictDescendantOf parent child =
  let relativePath = makeRelative parent child
   in relativePath /= "."
        && not (isAbsolute relativePath)
        && case splitDirectories relativePath of
          ".." : _ -> False
          _ -> True
readGitOrigin :: FilePath -> IO (Maybe String)
readGitOrigin repositoryRoot = do
  (originExit, originStdout, originStderr) <- readGitProcess ["-C", repositoryRoot, "config", "--get", "remote.origin.url"] ""
  case originExit of
    ExitSuccess -> pure (Just (T.unpack (T.strip (T.pack originStdout))))
    ExitFailure 1 | null originStdout -> pure Nothing
    _ -> do
      hPutStr stderr originStderr
      exitWith originExit
validateRegisteredHomeRepository :: FilePath -> FilePath -> FilePath -> IO ()
validateRegisteredHomeRepository home actualRelativePath originRelativePath =
  readHomeRepositories home >>= \case
    Left issues -> reportHomeCheckIssues issues
    Right repositories ->
      forM_ repositories $ \repository ->
        case canonicalHomeRepositoryPath (homeRepositoryUrl repository) of
          Right registeredRelativePath
            | homeRepositoryPath repository == actualRelativePath || registeredRelativePath == originRelativePath ->
                when (homeRepositoryPath repository /= actualRelativePath || registeredRelativePath /= originRelativePath) $ do
                  hPutStrLn stderr "error: project path, origin, and home submodule registration do not agree"
                  exitFailure
          _ -> pure ()
type HomeRepository :: Type
data HomeRepository = HomeRepository
  { homeRepositoryName :: String,
    homeRepositoryPath :: FilePath,
    homeRepositoryUrl :: String
  }
  deriving stock (Eq, Show)
homeGitignoreIsCompatible :: T.Text -> Bool
homeGitignoreIsCompatible source =
  case T.lines source of
    "*" : remainingLines -> all (T.isPrefixOf "!/") remainingLines
    _ -> False
homeGitignoreIsComplete :: T.Text -> Bool
homeGitignoreIsComplete source =
  homeGitignoreIsCompatible source
    && all (`elem` T.lines source) homeRequiredGitignorePatterns
completeHomeGitignore :: T.Text -> T.Text
completeHomeGitignore source =
  let missingLines = filter (`notElem` T.lines source) homeRequiredGitignorePatterns
      separator :: T.Text
      separator = if T.null source || T.isSuffixOf "\n" source then "" else "\n"
   in source <> separator <> T.unlines missingLines
checkHomeProfile :: FilePath -> Bool -> IO ()
checkHomeProfile repositoryRoot fix = do
  let gitignorePath = repositoryRoot </> ".gitignore"
  gitignoreStatus <- try (Posix.getSymbolicLinkStatus gitignorePath) :: IO (Either IOException Posix.FileStatus)
  case gitignoreStatus of
    Right status
      | not (Posix.isRegularFile status) -> reportHomeCheckIssues [gitignorePath ++ ": must be a regular file"]
    _ -> do
      actualGitignore <- readTextFileIfExists gitignorePath
      case actualGitignore of
        Nothing ->
          if fix
            then TIO.writeFile gitignorePath homeGitignoreSource
            else reportHomeCheckIssues [gitignorePath ++ ": is missing"]
        Just source
          | not (homeGitignoreIsCompatible source) ->
              reportHomeCheckIssues [gitignorePath ++ ": must start with * and subsequent lines must start with !/"]
          | not (homeGitignoreIsComplete source) ->
              if fix
                then TIO.writeFile gitignorePath (completeHomeGitignore source)
                else reportHomeCheckIssues [gitignorePath ++ ": must whitelist .gitignore and .gitmodules"]
          | otherwise -> pure ()
  readHomeRepositories repositoryRoot >>= \case
    Left issues -> reportHomeCheckIssues issues
    Right repositories -> do
      let duplicateConfiguredPaths = duplicateHomeRepositoryValues homeRepositoryPath repositories
          duplicateCanonicalPaths = duplicateHomeRepositoryValues (fromRight "" . canonicalHomeRepositoryPath . homeRepositoryUrl) repositories
          configuredPathOwners = Map.fromList [(homeRepositoryPath repository, homeRepositoryName repository) | repository <- repositories]
          occupiedTargetIssues =
            [ "submodule \"" ++ homeRepositoryName repository ++ "\": canonical path '" ++ expectedPath ++ "' is currently used by submodule \"" ++ existingName ++ "\""
            | repository <- repositories,
              Right expectedPath <- [canonicalHomeRepositoryPath (homeRepositoryUrl repository)],
              expectedPath /= homeRepositoryPath repository,
              Just existingName <- [Map.lookup expectedPath configuredPathOwners]
            ]
          urlAndPathIssues = concatMap validateHomeRepository repositories
          duplicateIssues =
            map ("duplicate configured repository path: " ++) duplicateConfiguredPaths
              ++ map ("duplicate canonical repository path: " ++) (filter (not . null) duplicateCanonicalPaths)
          issues = urlAndPathIssues ++ duplicateIssues ++ occupiedTargetIssues
      if not (null issues)
        then reportHomeCheckIssues issues
        else forM_ repositories $ \repository ->
          case canonicalHomeRepositoryPath (homeRepositoryUrl repository) of
            Right expectedPath
              | expectedPath /= homeRepositoryPath repository ->
                  if fix
                    then fixHomeRepositoryPath repositoryRoot repository expectedPath
                    else reportHomeCheckIssues ["submodule \"" ++ homeRepositoryName repository ++ "\": path '" ++ homeRepositoryPath repository ++ "' does not match URL; expected '" ++ expectedPath ++ "'"]
            _ -> pure ()
renderHomeProfileStatus :: FilePath -> IO ()
renderHomeProfileStatus repositoryRoot = do
  checkHomeProfile repositoryRoot False
  readHomeRepositories repositoryRoot >>= \case
    Left issues -> reportHomeCheckIssues issues
    Right repositories -> putStrLn (renderJSON (homeStatusJSON repositories))
homeStatusJSON :: [HomeRepository] -> Aeson.Value
homeStatusJSON repositories =
  Aeson.object
    [ "repositoryType" Aeson..= ("home" :: String),
      "resources" Aeson..= map homeRepositoryJSON repositories
    ]
homeRepositoryJSON :: HomeRepository -> Aeson.Value
homeRepositoryJSON repository =
  Aeson.object
    [ "kind" Aeson..= ("repository" :: String),
      "name" Aeson..= homeRepositoryName repository,
      "path" Aeson..= homeRepositoryPath repository,
      "url" Aeson..= homeRepositoryUrl repository
    ]
readHomeRepositories :: FilePath -> IO (Either [String] [HomeRepository])
readHomeRepositories repositoryRoot = do
  let gitmodulesPath = repositoryRoot </> ".gitmodules"
  gitmodulesStatus <- try (Posix.getSymbolicLinkStatus gitmodulesPath)
  case gitmodulesStatus of
    Left (_ :: IOException) -> pure (Right [])
    Right status
      | not (Posix.isRegularFile status) -> pure (Left [gitmodulesPath ++ ": must be a regular file"])
      | otherwise -> do
          (configExit, configStdout, configStderr) <-
            readGitProcess ["config", "get", "--file", gitmodulesPath, "--null", "--show-names", "--all", "--regexp", "^submodule\\..*"] ""
          if configExit == ExitFailure 1 && null configStdout && null configStderr
            then pure (Right [])
            else
              if configExit /= ExitSuccess
                then pure (Left ["could not read " ++ gitmodulesPath ++ ": " ++ T.unpack (T.strip (T.pack configStderr))])
                else pure (parseHomeRepositoryConfig configStdout)
parseHomeRepositoryConfig :: String -> Either [String] [HomeRepository]
parseHomeRepositoryConfig source =
  let records = splitNullTerminated source
      fields = mapMaybe parseHomeRepositoryField records
      malformedCount = length (filter isMalformedHomeRepositoryRecord records)
      grouped = Map.fromListWith (++) [(section, [(fieldName, value)]) | (section, fieldName, value) <- fields]
      parseSection :: (String, [(String, String)]) -> Either String HomeRepository
      parseSection (section, sectionFields) =
        case ([value | ("path", value) <- sectionFields], [value | ("url", value) <- sectionFields]) of
          ([path], [url]) -> Right (HomeRepository section path url)
          (paths, urls) -> Left ("submodule \"" ++ section ++ "\": must have exactly one path and one URL (found " ++ show (length paths) ++ " paths and " ++ show (length urls) ++ " URLs)")
      parsed = map parseSection (Map.toAscList grouped)
      issues = lefts parsed ++ ["malformed .gitmodules field" | malformedCount > 0]
   in if null issues then Right (rights parsed) else Left issues
isMalformedHomeRepositoryRecord :: String -> Bool
isMalformedHomeRepositoryRecord record =
  case T.breakOn "\n" (T.pack record) of
    (_, valueWithSeparator) -> isNothing (T.stripPrefix "\n" valueWithSeparator)
parseHomeRepositoryField :: String -> Maybe (String, String, String)
parseHomeRepositoryField record = do
  let (keyText, valueWithSeparator) = T.breakOn "\n" (T.pack record)
  valueText <- T.stripPrefix "\n" valueWithSeparator
  let key = T.unpack keyText
      value = T.unpack valueText
  remainder <- stripPrefix "submodule." key
  (section, fieldName) <-
    ((,"path") . T.unpack <$> T.stripSuffix ".path" (T.pack remainder))
      <|> ((,"url") . T.unpack <$> T.stripSuffix ".url" (T.pack remainder))
  guard (not (null section))
  pure (section, fieldName, value)
validateHomeRepository :: HomeRepository -> [String]
validateHomeRepository repository =
  case canonicalHomeRepositoryPath (homeRepositoryUrl repository) of
    Left urlError -> ["submodule \"" ++ homeRepositoryName repository ++ "\": " ++ urlError]
    Right _ ->
      [ "submodule \"" ++ homeRepositoryName repository ++ "\": invalid path '" ++ homeRepositoryPath repository ++ "'"
      | not (validCanonicalHomePath (homeRepositoryPath repository))
      ]
duplicateHomeRepositoryValues :: (Ord value) => (HomeRepository -> value) -> [HomeRepository] -> [value]
duplicateHomeRepositoryValues select repositories =
  Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(select repository, 1 :: Int) | repository <- repositories]))
canonicalHomeRepositoryPath :: String -> Either String FilePath
canonicalHomeRepositoryPath repositoryUrl = do
  (hostname, repositoryPathWithSuffix) <-
    maybe (Left ("remote URL has no canonical host and repository path: " ++ repositoryUrl)) Right (parseHostedGitRemote repositoryUrl)
  let trimmedRepositoryPath = dropWhileEnd (== '/') repositoryPathWithSuffix
      repositoryComponents = map T.unpack (T.splitOn "/" (T.pack trimmedRepositoryPath))
  case repositoryComponents of
    _ : _ -> do
      mapM_ validateHomePathComponent (hostname : repositoryComponents)
      let repositoryPath = intercalate "/" repositoryComponents
          withoutGitSuffix = maybe repositoryPath T.unpack (T.stripSuffix ".git" (T.pack repositoryPath))
      pure (hostname ++ "/" ++ withoutGitSuffix)
    _ -> Left ("unsupported or incomplete repository URL: " ++ repositoryUrl)
parseHostedGitRemote :: String -> Maybe (String, String)
parseHostedGitRemote repositoryUrl =
  parseUrlStyle <|> parseScpStyle
  where
    parseUrlStyle :: Maybe (String, String)
    parseUrlStyle = do
      uri <- parseURI repositoryUrl
      guard (uriScheme uri `elem` ["https:", "http:", "ssh:", "git+ssh:", "git:"])
      authority <- uriAuthority uri
      path <- stripPrefix "/" (uriPath uri)
      let hostname = uriRegName authority
      guard (not (null path))
      pure (map toLower hostname, path)
    parseScpStyle :: Maybe (String, String)
    parseScpStyle = do
      let (authority, pathWithSeparator) = break (== ':') repositoryUrl
      path <- stripPrefix ":" pathWithSeparator
      guard (not (null authority) && '/' `notElem` authority && not (null path))
      let hostname = reverse (takeWhile (/= '@') (reverse authority))
      guard (not (null hostname))
      pure (map toLower hostname, path)
resolveGitRemoteUrl :: FilePath -> String -> IO String
resolveGitRemoteUrl repositoryRoot repositoryUrl = do
  (resolveExit, resolveStdout, resolveStderr) <- readGitProcess ["-C", repositoryRoot, "ls-remote", "--get-url", repositoryUrl] ""
  if resolveExit == ExitSuccess
    then pure (T.unpack (T.strip (T.pack resolveStdout)))
    else do
      putStr resolveStdout
      hPutStr stderr resolveStderr
      exitWith resolveExit
validateHomePathComponent :: String -> Either String ()
validateHomePathComponent component
  | null component = Left "repository path components must not be empty"
  | component `elem` [".", ".."] = Left "repository path components must not be '.' or '..'"
  | all (\character -> isAlphaNum character && ord character < 128 || character `elem` (".-_" :: String)) component = Right ()
  | otherwise = Left "repository path components must contain only ASCII letters, digits, '.', '-', or '_'"
validCanonicalHomePath :: FilePath -> Bool
validCanonicalHomePath path =
  case components of
    _hostname : _repository : _ -> all (either (const False) (const True) . validateHomePathComponent) components
    _ -> False
  where
    components = map T.unpack (T.splitOn "/" (T.pack path))
fixHomeRepositoryPath :: FilePath -> HomeRepository -> FilePath -> IO ()
fixHomeRepositoryPath repositoryRoot repository expectedPath = do
  createDirectoryIfMissing True (takeDirectory (repositoryRoot </> expectedPath))
  runGitOrExit ["-C", repositoryRoot, "mv", "--", homeRepositoryPath repository, expectedPath]
reportHomeCheckIssues :: [String] -> IO a
reportHomeCheckIssues issues = do
  forM_ issues (hPutStrLn stderr . ("error: " ++))
  exitFailure
runGitOrExit :: [String] -> IO ()
runGitOrExit arguments = do
  _ <- runProcessOrExit PrintSuccessfulOutput "git" arguments
  pure ()
readGitProcess :: [String] -> String -> IO (ExitCode, String, String)
readGitProcess = readProcessWithExitCode "git"
discoverGitRepositoryRoot :: FilePath -> IO FilePath
discoverGitRepositoryRoot repositoryDirectory = do
  repositoryRootStdout <-
    captureGitOrExit ["-C", repositoryDirectory, "rev-parse", "--path-format=absolute", "--show-toplevel"]
  pure (T.unpack (T.strip (T.pack repositoryRootStdout)))
captureGitOrExit :: [String] -> IO String
captureGitOrExit = runProcessOrExit SuppressSuccessfulOutput "git"
type SuccessfulProcessOutput :: Type
data SuccessfulProcessOutput = PrintSuccessfulOutput | SuppressSuccessfulOutput deriving stock (Eq)
runProcessOrExit :: SuccessfulProcessOutput -> FilePath -> [String] -> IO String
runProcessOrExit successfulOutput executable arguments = do
  (processExit, processStdout, processStderr) <- readProcessWithExitCode executable arguments ""
  case processExit of
    ExitSuccess -> do
      when (successfulOutput == PrintSuccessfulOutput) $ do
        putStr processStdout
        hPutStr stderr processStderr
      pure processStdout
    _ -> do
      putStr processStdout
      hPutStr stderr processStderr
      exitWith processExit
delegateToGit :: [String] -> IO a
delegateToGit gitArguments = executeFile "git" True gitArguments Nothing
checkRepositoryLocation :: FilePath -> IO ()
checkRepositoryLocation repositoryRoot =
  withCurrentDirectory repositoryRoot $
    collectRepositoryCompliance >>= \case
      Left repositoryComplianceFailure -> do
        reportCheckRepositoryFailure repositoryComplianceFailure
        exitFailure
      Right _ -> pure ()
fixAndCheckRepositoryLocation :: FilePath -> IO ()
fixAndCheckRepositoryLocation repositoryRoot = do
  repairResult <- withCurrentDirectory repositoryRoot $ do
    ensureRootGitignoreIsSafeToRepair >>= \case
      Left repairError -> pure (Left repairError)
      Right () -> do
        rootGitignoreSource <- renderRootGitignoreFromCurrentRepository
        writeTextFileAtomically ".gitignore" rootGitignoreSource
  case repairResult of
    Left repairError -> hPutStrLn stderr ("error: " ++ repairError) >> exitFailure
    Right () -> pure ()
  checkRepositoryLocation repositoryRoot
ensureRootGitignoreIsSafeToRepair :: IO (Either String ())
ensureRootGitignoreIsSafeToRepair = do
  gitignoreStatus <- try (Posix.getSymbolicLinkStatus ".gitignore") :: IO (Either IOException Posix.FileStatus)
  pure $
    case gitignoreStatus of
      Left _ -> Right ()
      Right status
        | Posix.isRegularFile status -> Right ()
        | otherwise -> Left ".gitignore: must be a regular file before it can be repaired"
requiredRepositoryRootFiles :: [FilePath]
requiredRepositoryRootFiles = [".gitignore", "flake.nix", "flake.lock"]
checkRequiredRepositoryRootFiles :: IO [String]
checkRequiredRepositoryRootFiles = do
  missingFiles <- filterM (fmap not . doesFileExist) requiredRepositoryRootFiles
  pure ["missing required file: " ++ missingFile | missingFile <- missingFiles]
type RepositoryCheckPhase :: Type
data RepositoryCheckPhase
  = RequiredRootFilesPhase
  | DirectoryStructurePhase
  | FileCompliancePhase
type RepositoryComplianceFailure :: Type
data RepositoryComplianceFailure = RepositoryComplianceFailure RepositoryCheckPhase (NonEmpty String)
collectRepositoryCompliance :: IO (Either RepositoryComplianceFailure RepositoryComplianceSuccess)
collectRepositoryCompliance = do
  requiredRootFileIssues <- checkRequiredRepositoryRootFiles
  case requiredRootFileIssues of
    [] -> collectRepositoryContentCompliance
    firstIssue : remainingIssues ->
      pure (Left (RepositoryComplianceFailure RequiredRootFilesPhase (firstIssue :| remainingIssues)))
collectRepositoryContentCompliance :: IO (Either RepositoryComplianceFailure RepositoryComplianceSuccess)
collectRepositoryContentCompliance = do
  (packages, repositoryStructureIssues) <- inspectRepositoryStructure
  case repositoryStructureIssues of
    [] -> do
      packageComplianceResults <- forM packages (uncurry checkPackage)
      let packageComplianceIssues = concatMap fst packageComplianceResults
          packageTestNames = Map.fromList (zip (map fst packages) (map snd packageComplianceResults))
      checkNames <- listGitVisibleChildNames "checks"
      let packageKinds = Map.fromList packages
      checkComplianceIssues <- concat <$> forM checkNames (checkTemplateWith packageKinds)
      rootGitignoreIssues <- checkRootGitignore
      case packageComplianceIssues ++ checkComplianceIssues ++ rootGitignoreIssues of
        [] ->
          pure
            ( Right
                RepositoryComplianceSuccess
                  { repositoryCompliancePackages = packages,
                    repositoryComplianceCheckNames = checkNames,
                    repositoryCompliancePackageTestNames = packageTestNames
                  }
            )
        firstIssue : remainingIssues ->
          pure (Left (RepositoryComplianceFailure FileCompliancePhase (firstIssue :| remainingIssues)))
    firstIssue : remainingIssues ->
      pure (Left (RepositoryComplianceFailure DirectoryStructurePhase (firstIssue :| remainingIssues)))
checkRootGitignore :: IO [String]
checkRootGitignore = do
  actualSource <- TIO.readFile ".gitignore"
  expectedSource <- renderRootGitignoreFromCurrentRepository
  pure [".gitignore: does not match the canonical root whitelist" | actualSource /= expectedSource]
reportCheckRepositoryFailure :: RepositoryComplianceFailure -> IO ()
reportCheckRepositoryFailure (RepositoryComplianceFailure checkPhase checkPhaseIssues) = do
  let checkPhaseName = renderRepositoryCheckPhase checkPhase
  hPutStrLn stderr ("error: git canonicalization check failed at phase: " ++ checkPhaseName)
  forM_ (NE.toList checkPhaseIssues) $ \issue ->
    hPutStrLn stderr ("- [" ++ checkPhaseName ++ "] " ++ issue)
  hPutStrLn stderr ("hint: " ++ repositoryCheckPhaseHint checkPhase)
renderRepositoryCheckPhase :: RepositoryCheckPhase -> String
renderRepositoryCheckPhase = \case
  RequiredRootFilesPhase -> "required-root-files"
  DirectoryStructurePhase -> "directory-structure"
  FileCompliancePhase -> "file-compliance"
repositoryCheckPhaseHint :: RepositoryCheckPhase -> String
repositoryCheckPhaseHint = \case
  RequiredRootFilesPhase -> "add flake.nix and run 'nix flake update' to create or update flake.lock."
  DirectoryStructurePhase -> "fix directory and required-file layout under packages/, hosts/, checks/, and repository root."
  FileCompliancePhase -> "align package files with the expected internal templates and language-specific policy checks."
type RepositoryPackageTestStatus :: Type
data RepositoryPackageTestStatus
  = RepositoryTestsNotConfigured
  | RepositoryTestsConfigured
  deriving stock (Eq, Show)
type RepositoryComplianceSuccess :: Type
data RepositoryComplianceSuccess = RepositoryComplianceSuccess
  { repositoryCompliancePackages :: [(FilePath, PackageKind)],
    repositoryComplianceCheckNames :: [FilePath],
    repositoryCompliancePackageTestNames :: Map.Map FilePath [String]
  }
  deriving stock (Eq, Show)
type RepositoryPackageSummary :: Type
data RepositoryPackageSummary = RepositoryPackageSummary
  { repositoryPackageName :: FilePath,
    repositoryPackageKind :: PackageKind,
    repositoryPackageDescription :: Maybe String,
    repositoryPackageDependencies :: [String],
    repositoryPackageTestNames :: [String],
    repositoryPackageTestStatus :: RepositoryPackageTestStatus
  }
  deriving stock (Eq, Show)
type RepositorySummary :: Type
data RepositorySummary = RepositorySummary
  { repositorySummaryReadme :: Maybe String,
    repositorySummaryPackages :: [RepositoryPackageSummary],
    repositorySummaryHosts :: [FilePath]
  }
  deriving stock (Eq, Show)
summarizeRepositoryLocation :: ([RepositorySummary] -> String) -> FilePath -> IO ()
summarizeRepositoryLocation render repositoryRoot = do
  repositoryPath <- canonicalizePath repositoryRoot
  repositorySummary <- summarizeRepositoryAt repositoryPath repositoryRoot
  putStr (render [repositorySummary])
summarizeRepositoryAt :: FilePath -> FilePath -> IO RepositorySummary
summarizeRepositoryAt repositoryPath repositoryRoot =
  withCurrentDirectory repositoryRoot $
    collectRepositoryCompliance >>= \case
      Left repositoryComplianceFailure -> do
        hPutStrLn stderr ("error: repository: " ++ repositoryPath)
        reportCheckRepositoryFailure repositoryComplianceFailure
        exitFailure
      Right (RepositoryComplianceSuccess packages checkNames packageTestNames) -> do
        let repositoryCheckNames = Set.fromList checkNames
        repositoryReadme <- fmap T.unpack <$> readTextFileIfExists "README"
        packageSummaries <- forM packages $ \(packageName, packageKind) ->
          summarizeRepositoryPackage repositoryCheckNames packageName packageKind (Map.findWithDefault [] packageName packageTestNames)
        hostNames <- listFilesystemChildDirectories "hosts"
        pure
          RepositorySummary
            { repositorySummaryReadme = repositoryReadme,
              repositorySummaryPackages = packageSummaries,
              repositorySummaryHosts = hostNames
            }
renderRepositorySummariesJSON :: [RepositorySummary] -> String
renderRepositorySummariesJSON summaries = renderJSON (repositoryStatusJSON summaries) ++ "\n"
repositoryStatusJSON :: [RepositorySummary] -> Aeson.Value
repositoryStatusJSON summaries =
  case summaries of
    [] -> statusObject Nothing []
    repositorySummary : _ ->
      statusObject
        (repositorySummaryReadme repositorySummary)
        ( map repositoryPackageSummaryJSON (repositorySummaryPackages repositorySummary)
            ++ map repositoryHostSummaryJSON (repositorySummaryHosts repositorySummary)
        )
  where
    statusObject :: Maybe String -> [Aeson.Value] -> Aeson.Value
    statusObject repositoryReadme resources =
      Aeson.object
        [ "repositoryType" Aeson..= ("flake" :: String),
          "readme" Aeson..= repositoryReadme,
          "resources" Aeson..= resources
        ]
repositoryPackageSummaryJSON :: RepositoryPackageSummary -> Aeson.Value
repositoryPackageSummaryJSON packageSummary =
  Aeson.object
    ( [ "kind" Aeson..= ("package" :: String),
        "name" Aeson..= repositoryPackageName packageSummary,
        "type" Aeson..= renderPackageKind (repositoryPackageKind packageSummary),
        "description" Aeson..= repositoryPackageDescription packageSummary,
        "dependencies" Aeson..= repositoryPackageDependencies packageSummary
      ]
        ++ ["tests" Aeson..= repositoryPackageTestsJSON packageSummary | not (null (repositoryPackageTestNames packageSummary))]
    )
repositoryPackageTestsJSON :: RepositoryPackageSummary -> Aeson.Value
repositoryPackageTestsJSON packageSummary =
  Aeson.object
    [ "status" Aeson..= renderRepositoryPackageTestStatus (repositoryPackageTestStatus packageSummary),
      "cases" Aeson..= map repositoryPackageTestCaseJSON (repositoryPackageTestNames packageSummary)
    ]
repositoryHostSummaryJSON :: FilePath -> Aeson.Value
repositoryHostSummaryJSON hostName =
  Aeson.object
    [ "kind" Aeson..= ("host" :: String),
      "name" Aeson..= hostName
    ]
repositoryPackageTestCaseJSON :: String -> Aeson.Value
repositoryPackageTestCaseJSON testName = Aeson.object ["name" Aeson..= testName]
renderRepositoryPackageTestStatus :: RepositoryPackageTestStatus -> String
renderRepositoryPackageTestStatus = \case
  RepositoryTestsNotConfigured -> "not-configured"
  RepositoryTestsConfigured -> "configured"
summarizeRepositoryPackage :: Set.Set FilePath -> FilePath -> PackageKind -> [String] -> IO RepositoryPackageSummary
summarizeRepositoryPackage repositoryCheckNames packageName packageKind repositoryPackageTestNamesValue = do
  let packageRoot = "packages" </> packageName
  maybeDefaultNixContents <- readTextFileIfExists (packageRoot </> "default.nix")
  maybeDefaultNixDescription <- maybe (pure Nothing) extractDefaultNixPackageDescription maybeDefaultNixContents
  let repositoryPackageDependenciesValue = maybe [] extractLocalPackageDependencies maybeDefaultNixContents
  repositoryPackageDescriptionValue <-
    case packageKind of
      HaskellPackage -> do
        maybeCabalContents <- readTextFileIfExists (packageRoot </> (packageName <.> "cabal"))
        pure ((maybeCabalContents >>= extractHaskellPackageDescription) <|> maybeDefaultNixDescription)
      RustPackage -> do
        pure maybeDefaultNixDescription
      _
        | packageKind `elem` [PythonPackage, PythonLaTeXPackage, PythonPyPIPackage] -> do
            maybePyprojectTomlContents <- readTextFileIfExists (packageRoot </> "pyproject.toml")
            let maybePyprojectDescription = maybePyprojectTomlContents >>= extractPythonPackageDescriptionFromPyprojectToml
            pure (maybePyprojectDescription <|> maybeDefaultNixDescription)
      _ -> pure maybeDefaultNixDescription
  let configuredRepositoryCheckName =
        repositoryCheckNameForPackage packageKind packageName
          >>= \checkName ->
            if checkName `Set.member` repositoryCheckNames
              then Just checkName
              else Nothing
      repositoryPackageTestStatusValue =
        maybe RepositoryTestsNotConfigured (const RepositoryTestsConfigured) configuredRepositoryCheckName
  pure
    RepositoryPackageSummary
      { repositoryPackageName = packageName,
        repositoryPackageKind = packageKind,
        repositoryPackageDescription = repositoryPackageDescriptionValue,
        repositoryPackageDependencies = repositoryPackageDependenciesValue,
        repositoryPackageTestNames = repositoryPackageTestNamesValue,
        repositoryPackageTestStatus = repositoryPackageTestStatusValue
      }
extractHaskellPackageDescription :: T.Text -> Maybe String
extractHaskellPackageDescription cabalContents = do
  cabalFields <- either (const Nothing) Just (parseCabalFields cabalContents)
  T.unpack <$> (lookupCabalField "description" cabalFields <|> lookupCabalField "synopsis" cabalFields)
extractPythonPackageDescriptionFromPyprojectToml :: T.Text -> Maybe String
extractPythonPackageDescriptionFromPyprojectToml pyprojectTomlContents = do
  pyprojectToml <- either (const Nothing) Just (parseToml pyprojectTomlContents)
  T.unpack <$> lookupTomlStringAt ["project", "description"] pyprojectToml
extractDefaultNixPackageDescription :: T.Text -> IO (Maybe String)
extractDefaultNixPackageDescription defaultNixContents = do
  parseResult <- parseNixExprFromText defaultNixContents
  pure (either (const Nothing) (fmap T.unpack . findNixStringAtPath ["meta", "description"]) parseResult)
extractDefaultNixPackageVersion :: T.Text -> IO (Maybe T.Text)
extractDefaultNixPackageVersion defaultNixContents = do
  parseResult <- parseNixExprFromText defaultNixContents
  pure (either (const Nothing) (findNixStringAtPath ["version"]) parseResult)
extractDefaultNixRustFlags :: T.Text -> IO (Maybe T.Text)
extractDefaultNixRustFlags defaultNixContents = do
  parseResult <- parseNixExprFromText defaultNixContents
  pure (either (const Nothing) (findNixStringAtPath ["env", "RUSTFLAGS"]) parseResult)
findNixStringAtPath :: [T.Text] -> NExprLoc -> Maybe T.Text
findNixStringAtPath targetPath (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NAbs _ body -> findNixStringAtPath targetPath body
    NLet bindings body -> findInBindings targetPath bindings <|> findNixStringAtPath targetPath body
    NSet _ bindings -> findInBindings targetPath bindings
    NStr (DoubleQuoted fragments) | null targetPath -> T.concat <$> traverse plainFragment fragments
    _ -> asum (map (findNixStringAtPath targetPath) (toList expressionFunctor))
  where
    plainFragment :: Antiquoted T.Text NExprLoc -> Maybe T.Text
    plainFragment (Plain fragment) = Just fragment
    plainFragment _ = Nothing
    findInBindings :: [T.Text] -> [Binding NExprLoc] -> Maybe T.Text
    findInBindings remainingPath = listToMaybe . mapMaybe (findInBinding remainingPath)
    findInBinding :: [T.Text] -> Binding NExprLoc -> Maybe T.Text
    findInBinding remainingPath (NamedVar keyPath value _) = do
      staticPath <- traverse nixKeyNameText (NE.toList keyPath)
      suffix <- stripPrefix staticPath remainingPath
      findNixStringAtPath suffix value
    findInBinding _ Inherit {} = Nothing
extractLocalPackageDependencies :: T.Text -> [String]
extractLocalPackageDependencies defaultNixContents =
  Set.toAscList . Set.fromList $
    [ T.unpack dependencyName
    | dependencyReference <- drop 1 (T.splitOn localPackagePrefix defaultNixContents),
      let dependencyName = T.takeWhile isInputNameCharacter dependencyReference,
      not (T.null dependencyName)
    ]
  where
    localPackagePrefix :: T.Text
    localPackagePrefix = "inputs.self.packages.${pkgs.stdenv.system}."
    isInputNameCharacter character = isAlphaNum character || character `elem` ("_-" :: String)
renderJSON :: (Aeson.ToJSON value) => value -> String
renderJSON = T.unpack . TE.decodeUtf8 . BL.toStrict . Aeson.encode
renderNixString :: String -> String
renderNixString value =
  "(builtins.fromJSON " ++ escapeInterpolation (renderJSON (renderJSON value)) ++ ")"
  where
    escapeInterpolation :: String -> String
    escapeInterpolation ('$' : '{' : remainingCharacters) = '\\' : '$' : '{' : escapeInterpolation remainingCharacters
    escapeInterpolation (character : remainingCharacters) = character : escapeInterpolation remainingCharacters
    escapeInterpolation [] = []
supportedAddPackageKinds :: [(String, PackageKind)]
supportedAddPackageKinds =
  [ (renderPackageKind packageKind, packageKind)
  | packageKind <- allPackageKinds,
    packageKindSupportsScaffold (packageKindSpec packageKind)
  ]
parseSupportedAddPackageKind :: String -> Maybe PackageKind
parseSupportedAddPackageKind packageKindName = lookup packageKindName supportedAddPackageKinds
validatePackageNameForKind :: PackageKind -> FilePath -> Maybe String
validatePackageNameForKind packageKind packageName =
  let packageSpec = packageKindSpec packageKind
      conventionName = packageKindNameConvention packageSpec
      separator = packageKindNameSeparator packageSpec
   in if isDelimitedLowercaseName separator packageName
        then Nothing
        else
          Just
            ( "package name must use "
                ++ conventionName
                ++ " for "
                ++ renderPackageKind packageKind
                ++ " packages"
            )
isDelimitedLowercaseName :: Char -> String -> Bool
isDelimitedLowercaseName separator packageName =
  all isValidNamePart (T.split (== separator) (T.pack packageName))
  where
    isValidNamePart :: T.Text -> Bool
    isValidNamePart namePart =
      not (T.null namePart)
        && T.all (\character -> isAsciiLower character || isDigit character) namePart
type ScaffoldFile :: Type
data ScaffoldFile = ScaffoldFile
  { scaffoldFilePath :: FilePath,
    scaffoldFileContents :: T.Text
  }
addPackageToCurrentRepository :: PackageKind -> FilePath -> Maybe String -> IO (Either String [FilePath])
addPackageToCurrentRepository = addPackageToCurrentRepositoryWithForce False
addPackageToCurrentRepositoryWithForce :: Bool -> PackageKind -> FilePath -> Maybe String -> IO (Either String [FilePath])
addPackageToCurrentRepositoryWithForce force packageKind packageName packageDescription =
  case validatePackageNameForKind packageKind packageName of
    Just validationError -> pure (Left validationError)
    Nothing -> do
      let packageRootDirectory = "packages" </> packageName
      packageRootExists <- doesPathExist packageRootDirectory
      if packageRootExists
        then pure (Left ("path already exists: " ++ packageRootDirectory))
        else do
          ensureRootGitignoreClean force >>= \case
            Left cleanlinessError -> pure (Left cleanlinessError)
            Right () -> do
              let packageScaffoldFiles = renderScaffoldFiles packageKind packageName packageDescription
                  checkScaffoldFiles = maybeToList (renderRepositoryCheckScaffoldFile packageName <$> checkTemplateSpecForPackageKind packageKind)
                  scaffoldFiles = packageScaffoldFiles ++ checkScaffoldFiles
                  scaffoldRoots = packageRootDirectory : map (takeDirectory . scaffoldFilePath) checkScaffoldFiles
              existingScaffoldRoots <- filterM doesPathExist scaffoldRoots
              case existingScaffoldRoots of
                existingScaffoldRoot : _ -> pure (Left ("path already exists: " ++ existingScaffoldRoot))
                [] ->
                  createScaffoldFiles scaffoldRoots scaffoldFiles >>= \case
                    Left scaffoldError -> pure (Left scaffoldError)
                    Right scaffoldPaths -> do
                      rootGitignoreSource <- renderRootGitignoreFromCurrentRepositoryWith scaffoldPaths
                      writeTextFileAtomically ".gitignore" rootGitignoreSource >>= \case
                        Left writeError -> do
                          removeScaffoldRoots scaffoldRoots
                          pure (Left ("could not update .gitignore: " ++ writeError))
                        Right () -> pure (Right (".gitignore" : scaffoldPaths))
ensureRootGitignoreClean :: Bool -> IO (Either String ())
ensureRootGitignoreClean force
  | force = pure (Right ())
  | otherwise = do
      (statusExit, statusStdout, statusStderr) <-
        readGitProcess ["status", "--porcelain=v1", "--untracked-files=all", "--ignored=matching", "--", ".gitignore"] ""
      pure $
        if statusExit /= ExitSuccess
          then Left ("could not inspect root .gitignore state: " ++ T.unpack (T.strip (T.pack statusStderr)))
          else
            if null statusStdout
              then Right ()
              else Left ("root .gitignore is not clean:\n" ++ statusStdout)
createScaffoldFiles :: [FilePath] -> [ScaffoldFile] -> IO (Either String [FilePath])
createScaffoldFiles scaffoldRoots scaffoldFiles = do
  let scaffoldPaths = map scaffoldFilePath scaffoldFiles
  existingPaths <- filterM doesPathExist scaffoldPaths
  case existingPaths of
    existingPath : _ -> pure (Left ("path already exists: " ++ existingPath))
    [] -> do
      writeResult <- try $ forM_ scaffoldFiles $ \scaffoldFile -> do
        let path = scaffoldFilePath scaffoldFile
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path (scaffoldFileContents scaffoldFile)
      case writeResult of
        Left (writeError :: IOException) -> do
          removeScaffoldRoots scaffoldRoots
          pure (Left ("could not create scaffold files: " ++ show writeError))
        Right () -> pure (Right scaffoldPaths)
removeScaffoldRoots :: [FilePath] -> IO ()
removeScaffoldRoots scaffoldRoots =
  forM_ scaffoldRoots $ \scaffoldRoot -> do
    scaffoldRootExists <- doesPathExist scaffoldRoot
    when scaffoldRootExists (removePathForcibly scaffoldRoot)
writeTextFileAtomically :: FilePath -> T.Text -> IO (Either String ())
writeTextFileAtomically target source = do
  writeResult <- try $ do
    let targetDirectory = takeDirectory target
    (temporaryPath, temporaryHandle) <- openTempFile targetDirectory (takeFileName target ++ ".git-canonicalization-")
    hClose temporaryHandle
    (TIO.writeFile temporaryPath source >> renameFile temporaryPath target)
      `onException` (doesPathExist temporaryPath >>= \exists -> when exists (removeFile temporaryPath))
  pure $
    case writeResult of
      Left (writeError :: IOException) -> Left (show writeError)
      Right () -> Right ()
renderRepositoryCheckScaffoldFile :: FilePath -> CheckTemplateSpec -> ScaffoldFile
renderRepositoryCheckScaffoldFile packageName checkSpec =
  case checkTemplatePackageAssociation checkSpec of
    Just (suffix, _) ->
      ScaffoldFile
        ("checks" </> (packageName ++ suffix) </> "default.nix")
        (checkTemplateBaselineSource checkSpec)
    Nothing -> error "cannot scaffold an unassociated check template"
repositoryCheckNameForPackage :: PackageKind -> FilePath -> Maybe FilePath
repositoryCheckNameForPackage packageKind packageName =
  checkTemplateSpecForPackageKind packageKind
    >>= (fmap ((packageName ++) . fst) . checkTemplatePackageAssociation)
checkTemplateSpecForPackageKind :: PackageKind -> Maybe CheckTemplateSpec
checkTemplateSpecForPackageKind packageKind =
  find
    (maybe False (elem packageKind . snd) . checkTemplatePackageAssociation)
    checkTemplateSpecs
renderScaffoldFiles :: PackageKind -> FilePath -> Maybe String -> [ScaffoldFile]
renderScaffoldFiles packageKind packageName packageDescription =
  map prefixPackagePath $
    case packageKind of
      HaskellPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultHaskellTemplateDescription packageDescription haskellTemplateBaselineNixSource),
          ScaffoldFile "Main.hs" haskellMainSource
        ]
      RustPackage ->
        [ ScaffoldFile "default.nix" rustTemplateBaselineNixSource,
          ScaffoldFile "Cargo.lock" (renderScaffoldCargoLock packageName),
          ScaffoldFile "src/main.rs" rustMainSource
        ]
      HTMLPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultHtmlTemplateDescription packageDescription htmlTemplateBaselineNixSource),
          ScaffoldFile "index.html" htmlIndexSource,
          ScaffoldFile "script.js" htmlScriptSource,
          ScaffoldFile "style.css" htmlStyleSource
        ]
      PythonLaTeXPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultPythonLaTeXTemplateDescription packageDescription pythonLaTeXTemplateBaselineNixSource),
          ScaffoldFile "main.py" pythonLaTeXMainSource,
          ScaffoldFile "ms.tex" pythonLaTeXMsTexSource,
          ScaffoldFile "ms.bib" pythonLaTeXMsBibSource
        ]
      PythonPackage ->
        [ ScaffoldFile "default.nix" (renderPythonTemplateNixSource (scaffoldDescription defaultPythonTemplateDescription packageDescription)),
          ScaffoldFile "main.py" pythonMainSource
        ]
      PythonPyPIPackage ->
        [ ScaffoldFile "default.nix" pythonPyPITemplateBaselineNixSource
        ]
      CPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultCTemplateDescription packageDescription cTemplateBaselineNixSource),
          ScaffoldFile "main.c" cMainSource
        ]
      TerraformPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultTerraformTemplateDescription packageDescription deployHostTemplateBaselineNixSource),
          ScaffoldFile "main.tf" "terraform { }\n"
        ]
      LaTeXPackage ->
        [ ScaffoldFile "default.nix" (renderNixTemplateDescription defaultLaTeXTemplateDescription packageDescription latexTemplateBaselineNixSource),
          ScaffoldFile "ms.tex" latexMsTexSource,
          ScaffoldFile "ms.bib" latexMsBibSource
        ]
      unsupportedPackageKind -> error ("unsupported scaffold package kind: " ++ renderPackageKind unsupportedPackageKind)
  where
    prefixPackagePath scaffoldFile =
      scaffoldFile
        { scaffoldFilePath = "packages" </> packageName </> scaffoldFilePath scaffoldFile
        }
renderRootGitignoreFromCurrentRepository :: IO T.Text
renderRootGitignoreFromCurrentRepository = renderRootGitignoreFromCurrentRepositoryWith []
renderRootGitignoreFromCurrentRepositoryWith :: [FilePath] -> IO T.Text
renderRootGitignoreFromCurrentRepositoryWith projectedPaths = do
  repositoryEntries <- collectStructurallyAllowedRepositoryEntriesWith projectedPaths
  pure (renderRootGitignore (concatMap whitelistPathsForRepositoryEntry repositoryEntries))
renderRootGitignore :: [FilePath] -> T.Text
renderRootGitignore whitelistPaths =
  T.unlines ("*" : map (T.pack . ("!/" ++)) (Set.toAscList (Set.fromList (".gitignore" : whitelistPaths))))
whitelistPathsForRepositoryEntry :: RepositoryEntry -> [FilePath]
whitelistPathsForRepositoryEntry (repositoryPath, status) =
  case trackableTreeRoot repositoryPath of
    Just treeRoot -> whitelistPathsForTree treeRoot
    Nothing
      | Posix.isDirectory status -> []
      | otherwise -> whitelistPathsForExactFile repositoryPath
whitelistPathsForExactFile :: FilePath -> [FilePath]
whitelistPathsForExactFile filePath = parentDirectoryWhitelistPaths filePath ++ [filePath]
whitelistPathsForTree :: FilePath -> [FilePath]
whitelistPathsForTree treeRoot = parentDirectoryWhitelistPaths treeRoot ++ [treeRoot ++ "/", treeRoot ++ "/**"]
parentDirectoryWhitelistPaths :: FilePath -> [FilePath]
parentDirectoryWhitelistPaths path =
  reverse (go (takeDirectory path))
  where
    go "." = []
    go parentDirectory = (parentDirectory ++ "/") : go (takeDirectory parentDirectory)
trackableTreeRoot :: FilePath -> Maybe FilePath
trackableTreeRoot repositoryPath =
  case splitDirectories repositoryPath of
    "prm" : _ -> Just "prm"
    ["hosts", hostName, "prm"] -> Just ("hosts" </> hostName </> "prm")
    "hosts" : hostName : "prm" : _ -> Just ("hosts" </> hostName </> "prm")
    ["packages", packageName, "prm"] -> Just ("packages" </> packageName </> "prm")
    "packages" : packageName : "prm" : _ -> Just ("packages" </> packageName </> "prm")
    "packages" : packageName : "figures" : _ -> Just ("packages" </> packageName </> "figures")
    _ -> Nothing
removePackageFromCurrentRepository :: RemoveSpec -> IO (Either String ())
removePackageFromCurrentRepository removeSpec = do
  let packageName = removePackageName removeSpec
  let packagePath = "packages" </> packageName
      packageNameIsSafe = splitDirectories packageName == [packageName] && packageName `notElem` ["", ".", ".."]
  packageExists <- doesDirectoryExist packagePath
  if not packageNameIsSafe
    then pure (Left ("invalid package name: " ++ packageName))
    else
      if not packageExists
        then pure (Left ("package does not exist: " ++ packagePath))
        else do
          (packages, structureIssues) <- inspectRepositoryStructure
          case lookup packageName packages of
            Nothing ->
              pure
                ( Left
                    ( "cannot determine package type for "
                        ++ packagePath
                        ++ if null structureIssues then "" else ": " ++ intercalate "; " structureIssues
                    )
                )
            Just _ -> do
              existingCheckPaths <- filterM doesPathExist (managedCheckPathsForPackage packageName)
              let removalPaths = packagePath : existingCheckPaths
              packageCleanlinessResult <-
                if removeForce removeSpec
                  then pure (Right ())
                  else do
                    (statusExit, statusStdout, statusStderr) <-
                      readGitProcess
                        (["status", "--porcelain=v1", "--untracked-files=all", "--ignored=matching", "--"] ++ removalPaths)
                        ""
                    pure $
                      if statusExit /= ExitSuccess
                        then Left ("could not inspect package state: " ++ T.unpack (T.strip (T.pack statusStderr)))
                        else
                          if null statusStdout
                            then Right ()
                            else
                              Left ("package or check is not clean:\n" ++ statusStdout)
              rootGitignoreCleanlinessResult <-
                ensureRootGitignoreClean (removeForce removeSpec)
              let cleanlinessResult = packageCleanlinessResult >> rootGitignoreCleanlinessResult
              case cleanlinessResult of
                Left cleanlinessError -> pure (Left cleanlinessError)
                Right () ->
                  if removeDryRun removeSpec
                    then do
                      forM_ removalPaths (putStrLn . ("rm '" ++) . (++ "'"))
                      putStrLn "update '.gitignore'"
                      pure (Right ())
                    else do
                      let forceArguments :: [String]
                          forceArguments = ["-f" | removeForce removeSpec]
                      (removeExit, removeStdout, removeStderr) <- readGitProcess (["rm", "-r"] ++ forceArguments ++ ["--"] ++ removalPaths) ""
                      if removeExit /= ExitSuccess
                        then pure (Left ("git rm failed: " ++ T.unpack (T.strip (T.pack removeStderr))))
                        else do
                          putStr removeStdout
                          originalRootGitignoreSource <- TIO.readFile ".gitignore"
                          rootGitignoreSource <- renderRootGitignoreFromCurrentRepository
                          let restoreRemovedPaths = do
                                (restoreExit, _restoreStdout, restoreStderr) <- readGitProcess (["restore", "--staged", "--worktree", "--"] ++ removalPaths) ""
                                pure $
                                  if restoreExit == ExitSuccess
                                    then Nothing
                                    else Just ("; could not restore removed paths: " ++ T.unpack (T.strip (T.pack restoreStderr)))
                          writeTextFileAtomically ".gitignore" rootGitignoreSource >>= \case
                            Left writeError -> do
                              maybeRestoreError <- restoreRemovedPaths
                              pure (Left ("could not update .gitignore: " ++ writeError ++ fromMaybe "" maybeRestoreError))
                            Right () -> do
                              (addExit, _addStdout, addStderr) <- readGitProcess ["add", "--", ".gitignore"] ""
                              if addExit == ExitSuccess
                                then pure (Right ())
                                else do
                                  rootRestoreResult <- writeTextFileAtomically ".gitignore" originalRootGitignoreSource
                                  maybeRestoreError <- restoreRemovedPaths
                                  pure
                                    ( Left
                                        ( "could not stage .gitignore: "
                                            ++ T.unpack (T.strip (T.pack addStderr))
                                            ++ maybe "" ("; could not restore .gitignore: " ++) (either Just (const Nothing) rootRestoreResult)
                                            ++ fromMaybe "" maybeRestoreError
                                        )
                                    )
managedCheckPathsForPackage :: FilePath -> [FilePath]
managedCheckPathsForPackage packageName =
  Set.toAscList . Set.fromList $
    [ "checks" </> (packageName ++ suffix)
    | checkSpec <- checkTemplateSpecs,
      Just (suffix, _) <- [checkTemplatePackageAssociation checkSpec]
    ]
defaultPythonTemplateDescription :: String
defaultPythonTemplateDescription = "A Python template package."
defaultHaskellTemplateDescription :: String
defaultHaskellTemplateDescription = "Canonical Haskell package template"
defaultHtmlTemplateDescription :: String
defaultHtmlTemplateDescription = "An HTML, CSS, and JavaScript template package."
defaultPythonLaTeXTemplateDescription :: String
defaultPythonLaTeXTemplateDescription = "A Python and LaTeX template package."
defaultCTemplateDescription :: String
defaultCTemplateDescription = "A C template package."
defaultTerraformTemplateDescription :: String
defaultTerraformTemplateDescription = "A Terraform template package for deploying a host."
defaultLaTeXTemplateDescription :: String
defaultLaTeXTemplateDescription = "A LaTeX template package."
scaffoldDescription :: String -> Maybe String -> String
scaffoldDescription defaultDescription = maybe defaultDescription (unwords . words)
escapeNixDoubleQuotedString :: String -> String
escapeNixDoubleQuotedString = go
  where
    go [] = []
    go ('$' : '{' : remainingCharacters) = "\\${" ++ go remainingCharacters
    go ('"' : remainingCharacters) = "\\\"" ++ go remainingCharacters
    go ('\\' : remainingCharacters) = "\\\\" ++ go remainingCharacters
    go (character : remainingCharacters) = character : go remainingCharacters
renderNixTemplateDescription :: String -> Maybe String -> T.Text -> T.Text
renderNixTemplateDescription defaultDescription packageDescription =
  T.replace
    (T.pack defaultDescription)
    (T.pack (escapeNixDoubleQuotedString (scaffoldDescription defaultDescription packageDescription)))
renderPythonTemplateNixSource :: String -> T.Text
renderPythonTemplateNixSource packageDescription =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python3;",
      "in",
      "python.pkgs.buildPythonPackage rec {",
      "  installPhase = ''",
      "    install -Dm644 main.py \"$out/${python.sitePackages}/${pname}.py\"",
      "    install -Dm755 main.py \"$out/bin/${pname}\"",
      "    if [ -d prm ]; then",
      "      cp -R prm/ \"$out/${python.sitePackages}/\"",
      "      cp -R prm/ \"$out/bin/\"",
      "    fi",
      "  '';",
      "  meta = {",
      T.pack ("    description = \"" ++ escapeNixDoubleQuotedString packageDescription ++ "\";"),
      "    mainProgram = pname;",
      "  };",
      "  passthru.python = python;",
      "  pname = baseNameOf ./.;",
      "  pyproject = false;",
      "  shellHook = ''",
      "    source ${",
      "      pkgs.lib.getExe (",
      "        inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {",
      "          secrets.secrets.file = ../../secrets/secrets.age;",
      "        }",
      "      )",
      "    }",
      "    export $secrets",
      "  '';",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}",
      ""
    ]
pythonTemplateBaselineNixSource :: T.Text
pythonTemplateBaselineNixSource = renderPythonTemplateNixSource defaultPythonTemplateDescription
renderScaffoldCargoLock :: FilePath -> T.Text
renderScaffoldCargoLock packageName =
  T.unlines
    [ "# This file is automatically @generated by Cargo.",
      "# It is not intended for manual editing.",
      "version = 4",
      "",
      "[[package]]",
      T.pack ("name = \"" ++ packageName ++ "\""),
      "version = \"0.1.0\"",
      ""
    ]
inspectRepositoryStructure :: IO ([(FilePath, PackageKind)], [String])
inspectRepositoryStructure = do
  repositoryEntries <- collectRepositoryEntries "."
  let relativePaths = sort (map fst repositoryEntries)
      leafPaths = Set.fromList relativePaths
      packageRootPaths = Set.fromList (mapMaybe packageRootPathFromRepositoryPath relativePaths)
      hostRootPaths = Set.fromList (mapMaybe hostRootPathFromRepositoryPath relativePaths)
  packageInfos <- mapM (buildPackageInfo leafPaths) (Set.toAscList packageRootPaths)
  let allowedEntryRules = repositoryEntryRules packageInfos
      missingPackageDefaultNixIssues =
        [ packageRootDirectory ++ ": missing required file default.nix"
        | packageRootDirectory <- Set.toAscList packageRootPaths,
          Set.notMember (packageRootDirectory </> "default.nix") leafPaths
        ]
      missingHostConfigurationIssues =
        [ hostRootDirectory ++ ": missing required file configuration.nix"
        | hostRootDirectory <- Set.toAscList hostRootPaths,
          Set.notMember (hostRootDirectory </> "configuration.nix") leafPaths
        ]
      ambiguousPackageMarkerIssues =
        concatMap ambiguousPackageMarkerIssuesForPackage packageInfos
      packageNameConventionIssues =
        [ packageRootPath packageInfo ++ ": " ++ issue
        | packageInfo <- packageInfos,
          Just packageKind <- [packageKindFromDetection (packageDetection packageInfo)],
          Just issue <- [validatePackageNameForKind packageKind (packageRootDirectoryName packageInfo)]
        ]
      detectedPackages =
        [ (packageRootDirectoryName packageInfo, packageKind)
        | packageInfo <- packageInfos,
          Just packageKind <- [packageKindFromDetection (packageDetection packageInfo)]
        ]
      entryPolicyIssues = mapMaybe (validateRepositoryEntry allowedEntryRules) repositoryEntries
  pure (detectedPackages, entryPolicyIssues ++ missingPackageDefaultNixIssues ++ missingHostConfigurationIssues ++ packageNameConventionIssues ++ ambiguousPackageMarkerIssues)
type RepositoryEntry :: Type
type RepositoryEntry = (FilePath, Posix.FileStatus)
type EntryKind :: Type
data EntryKind = RegularFile | Directory
type EntryRule :: Type
data EntryRule = EntryRule
  { entryRuleKind :: EntryKind,
    entryRuleRegex :: String
  }
globalRegularFileRegexes :: [String]
globalRegularFileRegexes =
  map exactPathRegex globalExactRegularFilePaths
    ++ [ "^checks/[^/]+/default\\.nix$",
         "^hosts/[^/]+/configuration\\.nix$",
         "^hosts/[^/]+/hardware-configuration\\.nix$"
       ]
globalExactRegularFilePaths :: [FilePath]
globalExactRegularFilePaths =
  [ ".github/workflows/workflow.yml",
    ".gitignore",
    "LICENSE",
    "README",
    "flake.lock",
    "flake.nix",
    "formatter.nix",
    "secrets/secrets.age",
    "secrets/secrets.env.example",
    "secrets/secrets.nix"
  ]
exactPathRegex :: FilePath -> String
exactPathRegex path = "^" ++ escapeRegexLiteral path ++ "$"
repositoryEntryRules :: [PackageInfo] -> [EntryRule]
repositoryEntryRules packageInfos =
  map (EntryRule RegularFile) (globalRegularFileRegexes ++ packageRegularFileRegexes)
    ++ map (EntryRule Directory) opaqueDirectoryRegexes
  where
    packageRegularFileRegexes =
      concat
        [ allowedRegularFileRegexesForPackageKind (packageRootPath packageInfo) (packageRootDirectoryName packageInfo) (packageKindFromDetection (packageDetection packageInfo))
        | packageInfo <- packageInfos
        ]
validateRepositoryEntry :: [EntryRule] -> RepositoryEntry -> Maybe String
validateRepositoryEntry rules (path, status) =
  case filter ((path =~) . entryRuleRegex) rules of
    [] -> Just (path ++ ": is not allowed")
    matchingRules
      | any (`entryRuleAccepts` status) matchingRules -> Nothing
      | otherwise -> Just (path ++ ": expected " ++ intercalate " or " (map renderEntryRule matchingRules) ++ ", found " ++ renderFileStatus status)
entryRuleAccepts :: EntryRule -> Posix.FileStatus -> Bool
entryRuleAccepts rule = case entryRuleKind rule of
  RegularFile -> Posix.isRegularFile
  Directory -> Posix.isDirectory
renderEntryRule :: EntryRule -> String
renderEntryRule rule = case entryRuleKind rule of
  RegularFile -> "regular file"
  Directory -> "directory"
renderFileStatus :: Posix.FileStatus -> String
renderFileStatus status
  | Posix.isSymbolicLink status = "symbolic link"
  | Posix.isRegularFile status = "regular file"
  | Posix.isDirectory status = "directory"
  | otherwise = "special file"
opaqueDirectoryRegexes :: [String]
opaqueDirectoryRegexes =
  [ "^prm$",
    "^hosts/[^/]+/prm$",
    "^packages/[^/]+/prm$"
  ]
type PackageKind :: Type
data PackageKind
  = HaskellPackage
  | RustPackage
  | HTMLPackage
  | PythonLaTeXPackage
  | PythonPackage
  | PythonPyPIPackage
  | CPackage
  | TerraformPackage
  | LaTeXPackage
  | BinaryReleasePackage
  deriving stock (Eq, Ord, Show)
type PackageKindSpec :: Type
data PackageKindSpec = PackageKindSpec
  { packageKindRenderedName :: String,
    packageKindNameConvention :: String,
    packageKindNameSeparator :: Char,
    packageKindSupportsScaffold :: Bool
  }
allPackageKinds :: [PackageKind]
allPackageKinds =
  [ HaskellPackage,
    RustPackage,
    HTMLPackage,
    PythonLaTeXPackage,
    PythonPackage,
    PythonPyPIPackage,
    CPackage,
    TerraformPackage,
    LaTeXPackage,
    BinaryReleasePackage
  ]
packageKindSpec :: PackageKind -> PackageKindSpec
packageKindSpec = \case
  HaskellPackage -> PackageKindSpec "haskell" "kebab-case" '-' True
  RustPackage -> PackageKindSpec "rust" "kebab-case" '-' True
  HTMLPackage -> PackageKindSpec "html" "snake_case" '_' True
  PythonLaTeXPackage -> PackageKindSpec "python-latex" "snake_case" '_' True
  PythonPackage -> PackageKindSpec "python" "snake_case" '_' True
  PythonPyPIPackage -> PackageKindSpec "python-pypi" "snake_case" '_' True
  CPackage -> PackageKindSpec "c" "snake_case" '_' True
  TerraformPackage -> PackageKindSpec "terraform" "snake_case" '_' True
  LaTeXPackage -> PackageKindSpec "latex" "snake_case" '_' True
  BinaryReleasePackage -> PackageKindSpec "binary-release" "kebab-case" '-' False
renderPackageKind :: PackageKind -> String
renderPackageKind = packageKindRenderedName . packageKindSpec
type PackageInfo :: Type
data PackageInfo = PackageInfo
  { packageRootPath :: FilePath,
    packageRootDirectoryName :: FilePath,
    packageDetection :: PackageDetection
  }
type PackageDetection :: Type
data PackageDetection
  = DetectedPackageKind PackageKind
  | AmbiguousPackageMarkers (NonEmpty String)
  deriving stock (Eq, Show)
buildPackageInfo :: Set.Set FilePath -> FilePath -> IO PackageInfo
buildPackageInfo leafPaths packageRootDirectory = do
  maybeDefaultNixSource <- readTextFileIfExists (packageRootDirectory </> "default.nix")
  let packageDirectoryName = takeBaseName packageRootDirectory
      packageRelativeLeafPaths = mapMaybe (stripPrefix (packageRootDirectory ++ "/")) (Set.toAscList leafPaths)
      markers = detectPackageMarkers packageRelativeLeafPaths
  pure
    PackageInfo
      { packageRootPath = packageRootDirectory,
        packageRootDirectoryName = packageDirectoryName,
        packageDetection = detectPackageFromEvidence markers maybeDefaultNixSource
      }
detectPackageMarkers :: [FilePath] -> [(String, PackageKind)]
detectPackageMarkers packageRelativeLeafPaths =
  let hasLeafPath leafPath = leafPath `elem` packageRelativeLeafPaths
   in [ marker
      | (markerExists, marker) <-
          [ (hasLeafPath "Main.hs", ("Main.hs", HaskellPackage)),
            (hasLeafPath "Cargo.lock" && hasLeafPath "src/main.rs", ("Cargo.lock+src/main.rs", RustPackage)),
            (hasLeafPath "index.html", ("index.html", HTMLPackage)),
            (hasLeafPath "main.py" && hasLeafPath "ms.tex", ("main.py+ms.tex", PythonLaTeXPackage)),
            (hasLeafPath "main.py" && not (hasLeafPath "ms.tex"), ("main.py", PythonPackage)),
            (hasLeafPath "main.c", ("main.c", CPackage)),
            (hasLeafPath "main.tf", ("main.tf", TerraformPackage)),
            (hasLeafPath "ms.tex" && not (hasLeafPath "main.py"), ("ms.tex", LaTeXPackage))
          ],
        markerExists
      ]
detectPackageFromEvidence :: [(String, PackageKind)] -> Maybe T.Text -> PackageDetection
detectPackageFromEvidence markers maybeDefaultNixSource =
  case markers of
    []
      | maybe False isExternalPythonPackageSource maybeDefaultNixSource ->
          DetectedPackageKind PythonPyPIPackage
      | otherwise -> DetectedPackageKind BinaryReleasePackage
    [(_, markerKind)] -> DetectedPackageKind markerKind
    firstMarker : remainingMarkers ->
      AmbiguousPackageMarkers (fst firstMarker :| map fst remainingMarkers)
packageKindFromDetection :: PackageDetection -> Maybe PackageKind
packageKindFromDetection = \case
  DetectedPackageKind packageKind -> Just packageKind
  AmbiguousPackageMarkers _ -> Nothing
ambiguousPackageMarkerIssuesForPackage :: PackageInfo -> [String]
ambiguousPackageMarkerIssuesForPackage packageInfo =
  case packageDetection packageInfo of
    AmbiguousPackageMarkers matchedPackageMarkers ->
      [ packageRootPath packageInfo
          ++ ": has ambiguous project markers: "
          ++ intercalate ", " (NE.toList matchedPackageMarkers)
      ]
    DetectedPackageKind _ -> []
allowedRegularFileRegexesForPackageKind :: FilePath -> FilePath -> Maybe PackageKind -> [String]
allowedRegularFileRegexesForPackageKind packageRootDirectory packageDirectoryName maybePackageKind =
  let policy = packagePathPolicy packageRootDirectory packageDirectoryName maybePackageKind
   in map exactPathRegex (packagePolicyRegularFiles policy)
        ++ map treePathRegex (packagePolicyTraversedTrees policy)
allowedRegularFilePathsForPackageKind :: FilePath -> FilePath -> Maybe PackageKind -> [FilePath]
allowedRegularFilePathsForPackageKind packageRootDirectory packageDirectoryName maybePackageKind =
  packagePolicyRegularFiles (packagePathPolicy packageRootDirectory packageDirectoryName maybePackageKind)
type PackagePathPolicy :: Type
data PackagePathPolicy = PackagePathPolicy
  { packagePolicyRegularFiles :: [FilePath],
    packagePolicyTraversedTrees :: [FilePath]
  }
packagePathPolicy :: FilePath -> FilePath -> Maybe PackageKind -> PackagePathPolicy
packagePathPolicy packageRootDirectory _packageDirectoryName maybePackageKind =
  let packagePath relativePath = packageRootDirectory </> relativePath
      basePaths = [packagePath "default.nix"]
      kindFiles = case maybePackageKind of
        Just HaskellPackage -> [packagePath "Main.hs"]
        Just RustPackage -> map packagePath ["Cargo.lock", "src/main.rs"]
        Just HTMLPackage -> map packagePath ["index.html", "script.js", "style.css"]
        Just PythonLaTeXPackage -> map packagePath ["main.py", "ms.tex", "ms.bib", "refs.bib"]
        Just PythonPackage -> [packagePath "main.py"]
        Just PythonPyPIPackage -> []
        Just CPackage -> [packagePath "main.c"]
        Just TerraformPackage -> map packagePath ["main.tf", ".terraform.lock.hcl"]
        Just LaTeXPackage -> map packagePath ["ms.tex", "ms.bib"]
        Just BinaryReleasePackage -> []
        Nothing -> []
      traversedTrees = case maybePackageKind of
        Just PythonLaTeXPackage -> [packagePath "figures"]
        Just TerraformPackage -> [packagePath ".terraform"]
        _ -> []
   in PackagePathPolicy (basePaths ++ kindFiles) traversedTrees
treePathRegex :: FilePath -> String
treePathRegex path = "^" ++ escapeRegexLiteral path ++ "(/.*)?$"
collectStructurallyAllowedRepositoryEntriesWith :: [FilePath] -> IO [RepositoryEntry]
collectStructurallyAllowedRepositoryEntriesWith projectedPaths = do
  packageInfos <- discoverPackageInfosFromFilesystem
  checkNames <- listFilesystemChildDirectories "checks"
  hostNames <- listFilesystemChildDirectories "hosts"
  let checkPaths = ["checks" </> checkName </> "default.nix" | checkName <- checkNames]
      hostPaths =
        concat
          [ ["hosts" </> hostName </> "configuration.nix", "hosts" </> hostName </> "hardware-configuration.nix"]
          | hostName <- hostNames
          ]
      packagePaths = concatMap allowedExistingPackageFilePaths packageInfos
      opaquePaths =
        "prm"
          : ["hosts" </> hostName </> "prm" | hostName <- hostNames]
          ++ [packageRootPath packageInfo </> "prm" | packageInfo <- packageInfos]
      traversedTreePaths = concatMap allowedTraversedPackageTreePaths packageInfos
      allowedEntryRules = repositoryEntryRules packageInfos
  regularAndOpaqueEntries <- collectExistingRepositoryEntries (globalExactRegularFilePaths ++ checkPaths ++ hostPaths ++ packagePaths ++ opaquePaths)
  traversedTreeEntries <- concat <$> mapM collectTreeLeafRepositoryEntries traversedTreePaths
  projectedEntries <- collectExistingRepositoryEntries projectedPaths
  pure
    [ repositoryEntry
    | repositoryEntry <- regularAndOpaqueEntries ++ traversedTreeEntries ++ projectedEntries,
      isNothing (validateRepositoryEntry allowedEntryRules repositoryEntry)
    ]
discoverPackageInfosFromFilesystem :: IO [PackageInfo]
discoverPackageInfosFromFilesystem = do
  packageNames <- listFilesystemChildDirectories "packages"
  forM packageNames $ \packageName -> do
    let packageRootDirectory = "packages" </> packageName
        markerPaths =
          [ packageRootDirectory </> markerPath
          | markerPath <- ["Main.hs", "Cargo.lock", "src/main.rs", "index.html", "main.py", "main.c", "main.tf", "ms.tex"]
          ]
    markerEntries <- collectExistingRepositoryEntries markerPaths
    buildPackageInfo (Set.fromList (map fst markerEntries)) packageRootDirectory
allowedExistingPackageFilePaths :: PackageInfo -> [FilePath]
allowedExistingPackageFilePaths packageInfo =
  allowedRegularFilePathsForPackageKind (packageRootPath packageInfo) (packageRootDirectoryName packageInfo) (packageKindFromDetection (packageDetection packageInfo))
allowedTraversedPackageTreePaths :: PackageInfo -> [FilePath]
allowedTraversedPackageTreePaths packageInfo =
  packagePolicyTraversedTrees
    ( packagePathPolicy
        (packageRootPath packageInfo)
        (packageRootDirectoryName packageInfo)
        (packageKindFromDetection (packageDetection packageInfo))
    )
collectExistingRepositoryEntries :: [FilePath] -> IO [RepositoryEntry]
collectExistingRepositoryEntries paths = catMaybes <$> mapM collectExistingRepositoryEntry paths
collectExistingRepositoryEntry :: FilePath -> IO (Maybe RepositoryEntry)
collectExistingRepositoryEntry path = do
  parentDirectoriesAreRegular <- pathParentDirectoriesAreRegular path
  if not parentDirectoriesAreRegular
    then pure Nothing
    else do
      statusResult <- try (Posix.getSymbolicLinkStatus path)
      pure $
        case statusResult of
          Left (_ :: IOException) -> Nothing
          Right status -> Just (path, status)
pathParentDirectoriesAreRegular :: FilePath -> IO Bool
pathParentDirectoriesAreRegular path = go (takeDirectory path)
  where
    go "." = pure True
    go parentDirectory = do
      statusResult <- try (Posix.getSymbolicLinkStatus parentDirectory)
      case statusResult of
        Left (_ :: IOException) -> pure False
        Right status ->
          if Posix.isDirectory status
            then go (takeDirectory parentDirectory)
            else pure False
collectTreeLeafRepositoryEntries :: FilePath -> IO [RepositoryEntry]
collectTreeLeafRepositoryEntries path =
  collectExistingRepositoryEntry path >>= \case
    Nothing -> pure []
    Just repositoryEntry@(_, status)
      | not (Posix.isDirectory status) -> pure [repositoryEntry]
      | otherwise -> do
          childNames <- sort <$> listDirectory path
          childEntries <- concat <$> mapM (collectTreeLeafRepositoryEntries . (path </>)) childNames
          pure (if null childEntries then [repositoryEntry] else childEntries)
listFilesystemChildDirectories :: FilePath -> IO [FilePath]
listFilesystemChildDirectories parentDirectory = do
  collectExistingRepositoryEntry parentDirectory >>= \case
    Just (_, parentStatus) | Posix.isDirectory parentStatus -> do
      childNames <- sort <$> listDirectory parentDirectory
      filterM
        ( \childName ->
            collectExistingRepositoryEntry (parentDirectory </> childName) >>= \case
              Just (_, status) -> pure (Posix.isDirectory status)
              Nothing -> pure False
        )
        childNames
    _ -> pure []
escapeRegexLiteral :: String -> String
escapeRegexLiteral = concatMap escapeCharacter
  where
    escapeCharacter character
      | character `elem` ("\\.^$|?*+()[]{}" :: String) = ['\\', character]
      | otherwise = [character]
collectRepositoryEntries :: FilePath -> IO [RepositoryEntry]
collectRepositoryEntries rootPath = do
  trackedDirectories <- collectTrackedRepositoryDirectories
  repositoryEntries <- collectUnfilteredRepositoryEntries trackedDirectories rootPath
  filterGitIgnoredRepositoryEntries repositoryEntries
collectTrackedRepositoryDirectories :: IO (Set.Set FilePath)
collectTrackedRepositoryDirectories = do
  (gitLsFilesExit, trackedPathsOutput, gitLsFilesStderr) <- readGitProcess ["ls-files", "-z"] ""
  case gitLsFilesExit of
    ExitSuccess -> pure (Set.fromList (concatMap parentDirectoryPaths (splitNullTerminated trackedPathsOutput)))
    _ -> ioError (userError ("git ls-files failed: " ++ T.unpack (T.strip (T.pack gitLsFilesStderr))))
  where
    parentDirectoryPaths path =
      case takeDirectory path of
        "." -> []
        parentDirectory -> parentDirectory : parentDirectoryPaths parentDirectory
collectUnfilteredRepositoryEntries :: Set.Set FilePath -> FilePath -> IO [RepositoryEntry]
collectUnfilteredRepositoryEntries trackedDirectories rootPath = do
  childNames <- listDirectory rootPath
  let childPaths = sort [rootPath </> childName | childName <- childNames]
  concat <$> mapM (collectUnfilteredRepositoryEntry trackedDirectories) childPaths
collectUnfilteredRepositoryEntry :: Set.Set FilePath -> FilePath -> IO [RepositoryEntry]
collectUnfilteredRepositoryEntry trackedDirectories path = do
  status <- Posix.getSymbolicLinkStatus path
  let relativePath = makeRelative "." path
      collectDirectoryDescendants = do
        descendants <- collectUnfilteredRepositoryEntries trackedDirectories path
        pure (if null descendants then [(relativePath, status)] else descendants)
  case () of
    _
      | relativePath == ".git" -> pure []
      | not (Posix.isDirectory status) -> pure [(relativePath, status)]
      | isOpaqueDirectory relativePath -> pure [(relativePath, status)]
      | Set.notMember relativePath trackedDirectories -> do
          visibleEntries <- filterGitIgnoredRepositoryEntries [(relativePath, status)]
          case visibleEntries of
            [] -> pure []
            _ -> collectDirectoryDescendants
      | otherwise -> do
          collectDirectoryDescendants
filterGitIgnoredRepositoryEntries :: [RepositoryEntry] -> IO [RepositoryEntry]
filterGitIgnoredRepositoryEntries [] = pure []
filterGitIgnoredRepositoryEntries repositoryEntries = do
  let gitCheckIgnoreInput = concatMap ((++ "\0") . fst) repositoryEntries
  (checkIgnoreExit, ignoredPathsOutput, checkIgnoreStderr) <-
    readGitProcess ["check-ignore", "--stdin", "-z"] gitCheckIgnoreInput
  case checkIgnoreExit of
    ExitSuccess -> do
      let ignoredPaths = Set.fromList (splitNullTerminated ignoredPathsOutput)
      pure (filter ((`Set.notMember` ignoredPaths) . fst) repositoryEntries)
    ExitFailure 1 -> pure repositoryEntries
    _ -> ioError (userError ("git check-ignore failed: " ++ T.unpack (T.strip (T.pack checkIgnoreStderr))))
splitNullTerminated :: String -> [String]
splitNullTerminated = map T.unpack . filter (not . T.null) . T.split (== '\0') . T.pack
isOpaqueDirectory :: FilePath -> Bool
isOpaqueDirectory path = any (path =~) opaqueDirectoryRegexes
packageRootPathFromRepositoryPath :: FilePath -> Maybe FilePath
packageRootPathFromRepositoryPath repositoryPath =
  case splitDirectories repositoryPath of
    "packages" : packageName : _ -> Just ("packages" </> packageName)
    _ -> Nothing
hostRootPathFromRepositoryPath :: FilePath -> Maybe FilePath
hostRootPathFromRepositoryPath repositoryPath =
  case splitDirectories repositoryPath of
    "hosts" : hostDirectoryName : _ -> Just ("hosts" </> hostDirectoryName)
    _ -> Nothing
listGitVisibleChildNames :: FilePath -> IO [FilePath]
listGitVisibleChildNames parentDirectory = do
  repositoryEntries <- collectRepositoryEntries "."
  pure
    ( Set.toAscList
        ( Set.fromList
            [ childName
            | (repositoryPath, _) <- repositoryEntries,
              parent : childName : _ <- [splitDirectories repositoryPath],
              parent == parentDirectory
            ]
        )
    )
checkPackage :: FilePath -> PackageKind -> IO ([String], [String])
checkPackage packageName packageKind = do
  let packageDefaultNixPath = "packages" </> packageName </> "default.nix"
  maybePackageDefaultNixSource <- readTextFileIfExists packageDefaultNixPath
  templateIssues <-
    case maybePackageDefaultNixSource of
      Nothing -> pure []
      Just packageDefaultNixSource ->
        case inferTemplateSpec packageKind (T.unpack packageDefaultNixSource) of
          Nothing ->
            pure ["packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"]
          Just templateSpec -> do
            let matchedTemplateName = templateName templateSpec
                allowedNixDifferenceKeysForPackage =
                  if packageName == "c_template" && matchedTemplateName == "c_template"
                    then defaultAllowedNixDifferenceKeys
                    else templateAllowedDifferenceKeys templateSpec
                ignoredTopLevelFunctionParams = optionalTemplateFunctionParams packageKind ("packages" </> matchedTemplateName </> "default.nix")
            compareNixFileWithTemplate ignoredTopLevelFunctionParams packageDefaultNixPath ("packages" </> matchedTemplateName </> "default.nix") allowedNixDifferenceKeysForPackage (templateBaselineSource templateSpec)
  cargoTomlIssues <- checkCargoToml packageName
  cabalFileIssues <- checkCabalFile packageName
  defaultNixConventionIssues <- checkDefaultNixConventions packageName packageKind
  (testConventionIssues, packageTestNames) <- inspectPackageTests packageName packageKind
  pure
    ( templateIssues
        ++ defaultNixConventionIssues
        ++ cargoTomlIssues
        ++ cabalFileIssues
        ++ testConventionIssues,
      packageTestNames
    )
checkTemplateWith :: Map.Map FilePath PackageKind -> FilePath -> IO [String]
checkTemplateWith packageKinds checkName = do
  let checkTemplatePath = "checks" </> checkName </> "default.nix"
  maybeCheckTemplateText <- readTextFileIfExists checkTemplatePath
  case maybeCheckTemplateText of
    Nothing -> pure []
    Just checkTemplateText ->
      case inferCheckTemplateSpec packageKinds checkName (T.unpack checkTemplateText) of
        Nothing ->
          pure
            [ "checks/" ++ checkName ++ "/default.nix: could not infer corresponding check template"
            ]
        Just checkTemplateSpec -> do
          let packageAssociationIssues = validateCheckPackageAssociation packageKinds checkName checkTemplatePath checkTemplateSpec
          templateIssues <- validateCheckTemplate packageKinds checkName checkTemplatePath checkTemplateSpec
          pure (packageAssociationIssues ++ templateIssues)
validateCheckPackageAssociation :: Map.Map FilePath PackageKind -> FilePath -> FilePath -> CheckTemplateSpec -> [String]
validateCheckPackageAssociation packageKinds checkName checkTemplatePath checkTemplateSpec =
  case checkPackageAssociation checkTemplateSpec checkName of
    Nothing -> []
    Just (packageName, expectedPackageKinds) ->
      [ checkTemplatePath
          ++ ": "
          ++ checkTemplateName checkTemplateSpec
          ++ " requires corresponding "
          ++ intercalate " or " (map renderPackageKind expectedPackageKinds)
          ++ " package packages/"
          ++ packageName
      | maybe True (`notElem` expectedPackageKinds) (Map.lookup packageName packageKinds)
      ]
checkPackageAssociation :: CheckTemplateSpec -> FilePath -> Maybe (FilePath, [PackageKind])
checkPackageAssociation checkTemplateSpec checkName =
  case checkTemplatePackageAssociation checkTemplateSpec of
    Just (suffix, expectedPackageKinds) -> withSuffix suffix expectedPackageKinds
    Nothing -> Nothing
  where
    withSuffix :: String -> [PackageKind] -> Maybe (FilePath, [PackageKind])
    withSuffix checkNameSuffix expectedPackageKinds =
      (\packageName -> (T.unpack packageName, expectedPackageKinds))
        <$> T.stripSuffix (T.pack checkNameSuffix) (T.pack checkName)
isExternalPythonPackageSource :: T.Text -> Bool
isExternalPythonPackageSource packageDefaultNixSource =
  let source = T.unpack packageDefaultNixSource
   in ("buildPythonPackage" `isInfixOf` source || "buildPythonApplication" `isInfixOf` source)
        && not ("src = ./.;" `isInfixOf` source)
        && ("fetchPypi" `isInfixOf` source || "fetchurl" `isInfixOf` source)
readTextFileIfExists :: FilePath -> IO (Maybe T.Text)
readTextFileIfExists filePath = do
  pathExists <- doesPathExist filePath
  if not pathExists
    then pure Nothing
    else do
      isSymbolicLink <- pathIsSymbolicLink filePath
      fileExists <- doesFileExist filePath
      if isSymbolicLink || not fileExists
        then pure Nothing
        else Just <$> TIO.readFile filePath
checkDefaultNixConventions :: FilePath -> PackageKind -> IO [String]
checkDefaultNixConventions packageName packageKind = do
  let packageDefaultNixPath = "packages" </> packageName </> "default.nix"
  maybeDefaultNixText <- readTextFileIfExists packageDefaultNixPath
  case maybeDefaultNixText of
    Nothing -> pure []
    Just defaultNixText ->
      let defaultNixSource = T.unpack defaultNixText
          hasMetaMainProgram =
            "meta.mainProgram = pname;" `isInfixOf` defaultNixSource
              || ("meta = {" `isInfixOf` defaultNixSource && "mainProgram = pname;" `isInfixOf` defaultNixSource)
          hasExternalFetchUrlSource = "src = pkgs.fetchurl" `isInfixOf` defaultNixSource
          hasLocalSource = "src = ./.;" `isInfixOf` defaultNixSource
          hasPlaceholderVersion = "version = \"0.0.0\";" `isInfixOf` defaultNixSource
          hasVersionAssignment = "version = \"" `isInfixOf` defaultNixSource
          expectsMetaMainProgram =
            packageKind `elem` [RustPackage, PythonLaTeXPackage, PythonPackage, CPackage]
       in pure $
            catMaybes
              [ if expectsMetaMainProgram && not hasMetaMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: package kind requires meta.mainProgram = pname;")
                  else Nothing,
                if hasExternalFetchUrlSource && hasPlaceholderVersion
                  then Just ("packages/" ++ packageName ++ "/default.nix: fetchurl-based packages must use a non-placeholder version")
                  else Nothing,
                if packageKind /= RustPackage && hasLocalSource && hasVersionAssignment && not hasPlaceholderVersion
                  then Just ("packages/" ++ packageName ++ "/default.nix: src = ./.; packages must use version = \"0.0.0\";")
                  else Nothing
              ]
inspectPackageTests :: FilePath -> PackageKind -> IO ([String], [String])
inspectPackageTests packageName packageKind
  | packageKind `elem` [PythonPackage, PythonLaTeXPackage] = inspectPythonPackageTests packageName
  | packageKind == HaskellPackage = inspectHaskellPackageTests packageName
  | packageKind == RustPackage = inspectRustPackageTests packageName
  | otherwise = pure ([], [])
inspectPythonPackageTests :: FilePath -> IO ([String], [String])
inspectPythonPackageTests packageName =
  do
    let mainPythonPath = "packages" </> packageName </> "main.py"
    mainPythonFileExists <- doesFileExist mainPythonPath
    if not mainPythonFileExists
      then pure ([], [])
      else do
        inspection <- inspectPythonTests mainPythonPath
        pure $ case inspection of
          Left inspectionError -> (["packages/" ++ packageName ++ "/main.py: " ++ inspectionError], [])
          Right testNames -> ([], testNames)
type PythonTestInspection :: Type
newtype PythonTestInspection = PythonTestInspection
  { inspectedPythonTestName :: String
  }
instance Aeson.FromJSON PythonTestInspection where
  parseJSON =
    Aeson.withObject "PythonTestInspection" $ \object ->
      PythonTestInspection <$> object Aeson..: "name"
type PythonInspectionResult :: Type
newtype PythonInspectionResult = PythonInspectionResult [PythonTestInspection]
instance Aeson.FromJSON PythonInspectionResult where
  parseJSON =
    Aeson.withObject "PythonInspectionResult" $ \object ->
      PythonInspectionResult <$> object Aeson..: "tests"
inspectPythonTests :: FilePath -> IO (Either String [String])
inspectPythonTests pythonSourcePath = do
  findExecutable "python3" >>= \case
    Nothing -> pure (Left "missing Python 3 interpreter")
    Just pythonCommand -> do
      (inspectionExit, inspectionStdout, inspectionStderr) <-
        readProcessWithExitCode pythonCommand ["-c", pythonTestInspectorPythonSource, pythonSourcePath] ""
      pure $
        case inspectionExit of
          ExitSuccess ->
            case Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack inspectionStdout)) of
              Left decodeError -> Left ("python AST inspector produced malformed output: " ++ decodeError)
              Right (PythonInspectionResult tests) ->
                Right
                  ( Set.toAscList
                      ( Set.fromList
                          [ testSpecificationFromIdentifier testName
                          | test <- tests,
                            let testName = inspectedPythonTestName test
                          ]
                      )
                  )
          ExitFailure 1 -> Left "python source could not be parsed"
          ExitFailure _ -> Left ("python AST inspector execution failed: " ++ compactTextToSingleLine (T.pack inspectionStderr))
testSpecificationFromIdentifier :: String -> String
testSpecificationFromIdentifier identifier =
  case renderTestWords (wordsFromTestIdentifier (stripTestFrameworkPrefixes identifier)) of
    [] -> identifier
    (firstCharacter : firstWordRest) : remainingWords ->
      let sentence = unwords ((toUpper firstCharacter : firstWordRest) : remainingWords)
       in case reverse sentence of
            punctuation : _ | punctuation `elem` (".!?" :: String) -> sentence
            _ -> sentence ++ "."
    _ -> identifier
renderTestWords :: [String] -> [String]
renderTestWords = \case
  "non" : word : remainingWords -> ("non-" ++ renderTestWord word) : renderTestWords remainingWords
  "top" : "level" : remainingWords -> "top-level" : renderTestWords remainingWords
  word : remainingWords -> renderTestWord word : renderTestWords remainingWords
  [] -> []
renderTestWord :: String -> String
renderTestWord = \case
  "cli" -> "CLI"
  "e2e" -> "end-to-end"
  "gitignore" -> ".gitignore"
  "gitmodules" -> ".gitmodules"
  "url" -> "URL"
  "urls" -> "URLs"
  "utf8" -> "UTF-8"
  word -> word
stripTestFrameworkPrefixes :: String -> String
stripTestFrameworkPrefixes identifier =
  maybe
    identifier
    stripTestFrameworkPrefixes
    (listToMaybe [remainder | prefix <- ["test_", "quickcheck_", "property_", "prop_"], Just remainder <- [stripPrefix prefix identifier]])
wordsFromTestIdentifier :: String -> [String]
wordsFromTestIdentifier = filter (not . null) . go [] []
  where
    go :: [String] -> String -> String -> [String]
    go completed reversedWord [] = reverse (finishWord completed reversedWord)
    go completed reversedWord (character : remainingCharacters)
      | not (isAlphaNum character) = go (finishWord completed reversedWord) [] remainingCharacters
      | startsCamelWord reversedWord character remainingCharacters =
          go (finishWord completed reversedWord) [toLower character] remainingCharacters
      | otherwise = go completed (toLower character : reversedWord) remainingCharacters
    finishWord :: [String] -> String -> [String]
    finishWord completed [] = completed
    finishWord completed reversedWord = reverse reversedWord : completed
    startsCamelWord :: String -> Char -> String -> Bool
    startsCamelWord [] _ _ = False
    startsCamelWord (previousCharacter : _) character remainingCharacters =
      isUpper character
        && (isLower previousCharacter || maybe False isLower (listToMaybe remainingCharacters))
inspectHaskellPackageTests :: FilePath -> IO ([String], [String])
inspectHaskellPackageTests packageName = do
  let mainHaskellPath = "packages" </> packageName </> "Main.hs"
  maybeMainHaskellSourceText <- readTextFileIfExists mainHaskellPath
  case maybeMainHaskellSourceText of
    Nothing -> pure ([], [])
    Just mainHaskellSourceText ->
      case inspectHaskellSource (T.unpack mainHaskellSourceText) of
        Left parseError -> pure (["packages/" ++ packageName ++ "/Main.hs: parse error: " ++ parseError], [])
        Right inspection ->
          pure
            ( catMaybes
                [ if Set.member "runTestTT" (haskellInspectionNames inspection)
                    then Nothing
                    else Just ("packages/" ++ packageName ++ "/Main.hs: must run HUnit tests with runTestTT"),
                  if Set.member "hUnitPackageTests" (haskellInspectionNames inspection)
                    then Nothing
                    else Just ("packages/" ++ packageName ++ "/Main.hs: HUnit tests must use hUnitPackageTests"),
                  if any isMeaningfulTestLabel (haskellInspectionLabels inspection)
                    then Nothing
                    else Just ("packages/" ++ packageName ++ "/Main.hs: HUnit tests must use literal TestLabel descriptions")
                ],
              Set.toAscList . Set.fromList $
                haskellInspectionTestNames inspection
            )
discoverHaskellUnitTestNamesFromSource :: String -> [String]
discoverHaskellUnitTestNamesFromSource haskellSource = case inspectHaskellSource haskellSource of
  Left _ -> []
  Right inspection -> Set.toAscList . Set.fromList $ haskellInspectionTestNames inspection
isMeaningfulTestLabel :: String -> Bool
isMeaningfulTestLabel label =
  case dropWhile isSpace label of
    firstCharacter : _ -> isAlphaNum firstCharacter
    [] -> False
type HaskellInspection :: Type
data HaskellInspection = HaskellInspection
  { haskellInspectionLabels :: [String],
    haskellInspectionProperties :: [String],
    haskellInspectionNames :: Set.Set String
  }
haskellInspectionTestNames :: HaskellInspection -> [String]
haskellInspectionTestNames inspection =
  filter isMeaningfulTestLabel (haskellInspectionLabels inspection)
    ++ map testSpecificationFromIdentifier (haskellInspectionProperties inspection)
inspectHaskellSource :: String -> Either String HaskellInspection
inspectHaskellSource source = case HS.parseModuleWithMode haskellParseMode (normalizeHaskellImportsForParser source) of
  HS.ParseFailed location message -> Left (show location ++ ": " ++ message)
  HS.ParseOk haskellModule ->
    Right
      HaskellInspection
        { haskellInspectionLabels = everything (++) ([] `mkQ` labelsFromExpression) haskellModule,
          haskellInspectionProperties = everything (++) ([] `mkQ` propertiesFromDeclaration) haskellModule,
          haskellInspectionNames = Set.fromList (everything (++) ([] `mkQ` namesFromName) haskellModule)
        }
  where
    labelsFromExpression :: HS.Exp HS.SrcSpanInfo -> [String]
    labelsFromExpression (HS.App _ (HS.Con _ (HS.UnQual _ (HS.Ident _ "TestLabel"))) (HS.Lit _ (HS.String _ label _))) = [label]
    labelsFromExpression _ = []
    propertiesFromDeclaration :: HS.Decl HS.SrcSpanInfo -> [String]
    propertiesFromDeclaration (HS.TypeSig _ names _) = filter (isPrefixOf "prop_") (map haskellNameText names)
    propertiesFromDeclaration (HS.FunBind _ matches) = filter (isPrefixOf "prop_") (map matchName matches)
    propertiesFromDeclaration (HS.PatBind _ (HS.PVar _ name) _ _) = [propertyName | let propertyName = haskellNameText name, "prop_" `isPrefixOf` propertyName]
    propertiesFromDeclaration _ = []
    matchName :: HS.Match annotation -> String
    matchName (HS.Match _ name _ _ _) = haskellNameText name
    matchName (HS.InfixMatch _ _ name _ _ _) = haskellNameText name
    namesFromName :: HS.Name HS.SrcSpanInfo -> [String]
    namesFromName = pure . haskellNameText
    haskellParseMode =
      HS.defaultParseMode
        { HS.extensions =
            map
              HS.EnableExtension
              [HS.LambdaCase, HS.RoleAnnotations, HS.ScopedTypeVariables, HS.TupleSections]
        }
    normalizeHaskellImportsForParser = unlines . map normalizeImport . lines
    normalizeImport sourceLine = case words compatibleLine of
      "import" : moduleName : "qualified" : remainingWords ->
        unwords ("import" : "qualified" : moduleName : remainingWords)
      "type" : _typeName : "::" : _kind -> ""
      "deriving" : "stock" : _remainingWords -> ""
      _ -> compatibleLine
      where
        compatibleLine = T.unpack (T.replace " deriving stock (" " deriving (" (T.pack sourceLine))
haskellNameText :: HS.Name annotation -> String
haskellNameText (HS.Ident _ name) = name
haskellNameText (HS.Symbol _ name) = name
inspectRustPackageTests :: FilePath -> IO ([String], [String])
inspectRustPackageTests packageName = do
  let mainRustPath = "packages" </> packageName </> "src/main.rs"
  mainRustSource <- maybe "" T.unpack <$> readTextFileIfExists mainRustPath
  let hasRustTestModule = "#[cfg(test)]" `isInfixOf` mainRustSource && "mod tests" `isInfixOf` mainRustSource
      hasRustTestCases = "#[test]" `isInfixOf` mainRustSource
  pure
    ( catMaybes
        [ if hasRustTestModule
            then Nothing
            else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[cfg(test)] mod tests"),
          if hasRustTestCases
            then Nothing
            else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[test] test cases")
        ],
      discoverRustUnitTestNamesFromSource mainRustSource
    )
discoverRustUnitTestNamesFromSource :: String -> [String]
discoverRustUnitTestNamesFromSource =
  Set.toAscList
    . Set.fromList
    . map testSpecificationFromIdentifier
    . extractRustUnitTestNames
    . lines
extractRustUnitTestNames :: [String] -> [String]
extractRustUnitTestNames = Set.toAscList . Set.fromList . go False
  where
    go _ [] = []
    go awaitingFunctionAfterTestAttribute (line : rest)
      | "#[test]" `isPrefixOf` trimmed = go True rest
      | awaitingFunctionAfterTestAttribute && "fn " `isPrefixOf` trimmed =
          let functionName = takeWhile (\character -> character /= '(' && character /= ' ') (drop 3 trimmed)
           in [functionName | not (null functionName)] ++ go False rest
      | otherwise = go False rest
      where
        trimmed = dropWhile (== ' ') line
pythonTestInspectorPythonSource :: String
pythonTestInspectorPythonSource =
  unlines
    [ "import ast",
      "import json",
      "import sys",
      "",
      "def main():",
      "    path = sys.argv[1]",
      "    try:",
      "        source = open(path, encoding='utf-8').read()",
      "        module = ast.parse(source, filename=path)",
      "    except (OSError, SyntaxError, UnicodeError):",
      "        sys.exit(1)",
      "    test_containers = [module] + [",
      "        node for node in module.body",
      "        if isinstance(node, ast.ClassDef) and node.name.startswith('Test')",
      "    ]",
      "    tests = [",
      "        {'name': node.name}",
      "        for container in test_containers",
      "        for node in container.body",
      "        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))",
      "        and node.name.startswith('test_')",
      "    ]",
      "    print(json.dumps({'tests': tests}, ensure_ascii=False))",
      "    sys.exit(0)",
      "",
      "if __name__ == '__main__':",
      "    main()"
    ]
checkCargoToml :: FilePath -> IO [String]
checkCargoToml packageName = do
  let cargoTomlPath = "packages" </> packageName </> "Cargo.toml"
      defaultNixPath = "packages" </> packageName </> "default.nix"
  maybeCargoTomlContents <- readTextFileIfExists cargoTomlPath
  case maybeCargoTomlContents of
    Nothing -> pure []
    Just cargoTomlContents ->
      case parseToml cargoTomlContents of
        Left parseError -> pure [cargoTomlPath ++ ": parse error: " ++ T.unpack parseError]
        Right cargoToml -> do
          let cargoPackageName = lookupTomlStringAt ["package", "name"] cargoToml
              cargoPackageVersion = lookupTomlStringAt ["package", "version"] cargoToml
              normalizedCargoToml = normalizeCargoTomlForBaselineComparison packageName cargoToml
              normalizedBaselineCargoToml = parseToml rustCargoTomlBaseline >>= Right . normalizeCargoTomlForBaselineComparison packageName
          maybeDefaultNixContents <- readTextFileIfExists defaultNixPath
          maybeDefaultNixVersion <- maybe (pure Nothing) extractDefaultNixPackageVersion maybeDefaultNixContents
          maybeDefaultNixRustFlags <- maybe (pure Nothing) extractDefaultNixRustFlags maybeDefaultNixContents
          pure $
            catMaybes
              [ case cargoPackageName of
                  Nothing ->
                    Just ("packages/" ++ packageName ++ "/Cargo.toml: missing [package].name")
                  Just actualPackageName ->
                    if actualPackageName == T.pack packageName
                      then Nothing
                      else
                        Just
                          ( "packages/"
                              ++ packageName
                              ++ "/Cargo.toml: [package].name must match directory name (expected \""
                              ++ packageName
                              ++ "\", got \""
                              ++ T.unpack actualPackageName
                              ++ "\")"
                          ),
                case (cargoPackageVersion, maybeDefaultNixVersion) of
                  (Just cargoVersion, Just defaultNixVersion)
                    | cargoVersion == defaultNixVersion -> Nothing
                    | otherwise ->
                        Just
                          ( "packages/"
                              ++ packageName
                              ++ "/default.nix: version must match Cargo.toml [package].version (expected \""
                              ++ T.unpack cargoVersion
                              ++ "\", got \""
                              ++ T.unpack defaultNixVersion
                              ++ "\")"
                          )
                  (Nothing, _) -> Just ("packages/" ++ packageName ++ "/Cargo.toml: missing [package].version")
                  (_, Nothing) -> Just ("packages/" ++ packageName ++ "/default.nix: missing a literal version"),
                if maybe False (T.isInfixOf "-F unsafe-code") maybeDefaultNixRustFlags
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/default.nix: RUSTFLAGS must forbid unsafe-code"),
                if Right normalizedCargoToml == normalizedBaselineCargoToml
                  then Nothing
                  else
                    Just
                      ( "packages/"
                          ++ packageName
                          ++ "/Cargo.toml: only dependency sections may differ from the internal Rust Cargo baseline"
                      )
              ]
parseToml :: T.Text -> Either T.Text TOML.Value
parseToml source = case TOML.decode source of
  Left parseError -> Left (TOML.renderTOMLError parseError)
  Right value -> Right value
lookupTomlValueAt :: [T.Text] -> TOML.Value -> Maybe TOML.Value
lookupTomlValueAt [] value = Just value
lookupTomlValueAt (key : remainingKeys) (TOML.Table table) = Map.lookup key table >>= lookupTomlValueAt remainingKeys
lookupTomlValueAt _ _ = Nothing
lookupTomlStringAt :: [T.Text] -> TOML.Value -> Maybe T.Text
lookupTomlStringAt path value = case lookupTomlValueAt path value of
  Just (TOML.String textValue) -> Just textValue
  _ -> Nothing
normalizeCargoTomlForBaselineComparison :: FilePath -> TOML.Value -> TOML.Value
normalizeCargoTomlForBaselineComparison packageName = normalize []
  where
    normalize path (TOML.Table table) =
      TOML.Table
        ( Map.mapWithKey
            (\key -> normalize (path ++ [key]))
            (foldr Map.delete table (ignoredKeys path))
        )
    normalize path (TOML.Array values) = TOML.Array (map (normalize path) values)
    normalize path (TOML.String _)
      | path == ["package", "name"] || path == ["bin", "name"] = TOML.String (T.pack packageName)
    normalize _ value = value
    ignoredKeys :: [T.Text] -> [T.Text]
    ignoredKeys ("target" : _) = ["dependencies", "dev-dependencies", "build-dependencies"]
    ignoredKeys path
      | null path = ["dependencies", "dev-dependencies", "build-dependencies"]
      | otherwise = []
checkCabalFile :: FilePath -> IO [String]
checkCabalFile packageName = do
  let cabalFilePath = "packages" </> packageName </> packageName <.> "cabal"
  maybeCabalContents <- readTextFileIfExists cabalFilePath
  case maybeCabalContents of
    Nothing -> pure []
    Just cabalContents ->
      case parseCabalFields cabalContents of
        Left parseError -> pure [cabalFilePath ++ ": parse error: " ++ parseError]
        Right cabalFields -> do
          let normalizedCabal = normalizeCabalForBaselineComparison packageName cabalFields
              normalizedBaselineCabal = normalizeCabalForBaselineComparison packageName <$> parseCabalFields haskellCabalBaseline
              cabalPackageName = lookupCabalField "name" cabalFields
          pure $
            catMaybes
              [ if cabalPackageName == Just (T.pack packageName)
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/" ++ packageName ++ ".cabal: name must match directory name"),
                if Right normalizedCabal == normalizedBaselineCabal
                  then Nothing
                  else
                    Just
                      ( "packages/"
                          ++ packageName
                          ++ "/"
                          ++ packageName
                          ++ ".cabal: only build-depends and package metadata fields synopsis/description may differ from the internal Haskell cabal baseline"
                      )
              ]
parseCabalFields :: T.Text -> Either String [Field ()]
parseCabalFields source = either (Left . show) (Right . map void) (readFields (TE.encodeUtf8 source))
normalizeCabalForBaselineComparison :: FilePath -> [Field ()] -> [Field ()]
normalizeCabalForBaselineComparison packageName = mapMaybe normalizeField
  where
    normalizeField (Field (Name _ fieldName) fieldLines)
      | fieldName `elem` map BS8.pack ["build-depends", "synopsis", "description"] = Nothing
      | fieldName == BS8.pack "name" = Just (Field (Name () fieldName) [FieldLine () (BS8.pack packageName)])
      | otherwise = Just (Field (Name () fieldName) fieldLines)
    normalizeField (Section (Name _ sectionName) sectionArguments nestedFields) =
      Just
        ( Section
            (Name () sectionName)
            (if sectionName == BS8.pack "executable" then [SecArgName () (BS8.pack packageName)] else sectionArguments)
            (mapMaybe normalizeField nestedFields)
        )
lookupCabalField :: T.Text -> [Field ()] -> Maybe T.Text
lookupCabalField requestedName fields = do
  Field _ fieldLines <- find matches fields
  let value = T.strip (TE.decodeUtf8 (BS8.intercalate (BS8.pack "\n") [line | FieldLine _ line <- fieldLines]))
  pure (stripCabalQuotedValue value)
  where
    matches :: Field () -> Bool
    matches (Field (Name _ fieldName) _) = fieldName == TE.encodeUtf8 requestedName
    matches _ = False
stripCabalQuotedValue :: T.Text -> T.Text
stripCabalQuotedValue quotedValue =
  fromMaybe quotedValue (T.stripPrefix "\"" quotedValue >>= T.stripSuffix "\"")
optionalTemplateFunctionParams :: PackageKind -> FilePath -> Set.Set T.Text
optionalTemplateFunctionParams packageKind templateBaselineNixPath =
  if packageKind == CPackage || takeBaseName (takeDirectory templateBaselineNixPath) == "python_template"
    then Set.singleton "inputs"
    else Set.empty
compareCheckTemplateWithBaseline :: FilePath -> T.Text -> IO [String]
compareCheckTemplateWithBaseline checkTemplatePath templateBaselineText = do
  checkTemplateSource <- TIO.readFile checkTemplatePath
  let strippedCheckSource = T.strip checkTemplateSource
      strippedBaselineSource = T.strip templateBaselineText
  pure $
    if strippedCheckSource == strippedBaselineSource
      then []
      else
        let checkDefaultNixLines = T.lines strippedCheckSource
            templateDefaultNixLines = T.lines strippedBaselineSource
            mismatchDetails =
              case firstMismatchedLine checkDefaultNixLines templateDefaultNixLines of
                Just (lineNumber, actualLine, expectedLine) ->
                  [ "  - changed line " ++ show lineNumber,
                    "    expected: " ++ truncateDiagnosticValue (T.unpack expectedLine),
                    "    actual:   " ++ truncateDiagnosticValue (T.unpack actualLine)
                  ]
                Nothing ->
                  [ "  - expected form: " ++ truncateDiagnosticValue (compactTextToSingleLine strippedBaselineSource),
                    "  - actual form:   " ++ truncateDiagnosticValue (compactTextToSingleLine strippedCheckSource)
                  ]
         in [ checkTemplatePath
                ++ ": differs from embedded check template\n"
                ++ intercalate "\n" mismatchDetails
            ]
validateCheckTemplate :: Map.Map FilePath PackageKind -> FilePath -> FilePath -> CheckTemplateSpec -> IO [String]
validateCheckTemplate packageKinds checkName checkTemplatePath checkTemplateSpec =
  case checkTemplateComparisonMode checkTemplateSpec of
    ExactCheckTemplate ->
      compareCheckTemplateWithBaseline checkTemplatePath (checkTemplateBaselineSource checkTemplateSpec)
    StructuralCPackageVm ->
      validateCPackageVmCheck packageKinds checkName checkTemplatePath
validateCPackageVmCheck :: Map.Map FilePath PackageKind -> FilePath -> FilePath -> IO [String]
validateCPackageVmCheck packageKinds checkName checkTemplatePath = do
  maybeCheckTemplateText <- readTextFileIfExists checkTemplatePath
  case maybeCheckTemplateText of
    Nothing -> pure []
    Just checkTemplateText ->
      pure (validateCPackageVmCheckSource (Map.lookup checkName packageKinds) checkName checkTemplatePath (T.unpack checkTemplateText))
validateCPackageVmCheckSource :: Maybe PackageKind -> FilePath -> FilePath -> String -> [String]
validateCPackageVmCheckSource packageKind checkName checkTemplatePath checkTemplateSource =
  let hasCanonicalNameBinding = "name = baseNameOf ./.;" `isInfixOf` checkTemplateSource
      hasRunNixOSTest = "pkgs.testers.runNixOSTest" `isInfixOf` checkTemplateSource
      hasMachineNode = "nodes.machine" `isInfixOf` checkTemplateSource
      hasTestScript = "testScript = ''" `isInfixOf` checkTemplateSource
      hasSameNamePackageReference =
        "inputs.self.packages.${pkgs.stdenv.system}.${name}" `isInfixOf` checkTemplateSource
          || ("inputs.self.packages.${pkgs.stdenv.system}." ++ checkName) `isInfixOf` checkTemplateSource
   in catMaybes
        [ if packageKind == Just CPackage
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks require a same-name C package under packages/"),
          if hasRunNixOSTest
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks must use pkgs.testers.runNixOSTest"),
          if hasCanonicalNameBinding
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks must bind name from ./."),
          if hasMachineNode
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks must define nodes.machine"),
          if hasTestScript
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks must define testScript"),
          if hasSameNamePackageReference
            then Nothing
            else Just (checkTemplatePath ++ ": generic C VM checks must install or override the same-name package from inputs.self.packages")
        ]
compareNixFileWithTemplate :: Set.Set T.Text -> FilePath -> FilePath -> Set.Set T.Text -> T.Text -> IO [String]
compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys templateBaselineSourceText = do
  subjectNixParseResult <- parseNixExprFromFile subjectNixPath
  templateBaselineParseResult <- parseNixExprFromText templateBaselineSourceText
  case (subjectNixParseResult, templateBaselineParseResult) of
    (Left parseError, _) ->
      pure [subjectNixPath ++ ": parse error: " ++ show parseError]
    (_, Left parseError) ->
      pure [templateBaselineNixPath ++ ": parse error: " ++ show parseError]
    (Right subjectNixExpr, Right templateBaselineExpr) ->
      let normalizedSubjectNixExpr = normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys subjectNixExpr
          normalizedTemplateBaselineExpr = normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys templateBaselineExpr
       in pure $
            formatNixTemplateDifferences
              subjectNixPath
              templateBaselineNixPath
              normalizedSubjectNixExpr
              normalizedTemplateBaselineExpr
parseNixExprFromText :: T.Text -> IO (Either String NExprLoc)
parseNixExprFromText nixSource = do
  temporaryDirectory <- getTemporaryDirectory
  (temporaryNixPath, temporaryNixHandle) <- openTempFile temporaryDirectory "check-repository-template-override.nix"
  TIO.hPutStr temporaryNixHandle nixSource
  hClose temporaryNixHandle
  parseNixExprFromFile temporaryNixPath
    `finally` removeFile temporaryNixPath
parseNixExprFromFile :: FilePath -> IO (Either String NExprLoc)
parseNixExprFromFile nixFilePath =
  either (Left . show) Right <$> parseNixFileLoc (Path nixFilePath)
inferTemplateSpec :: PackageKind -> String -> Maybe TemplateSpec
inferTemplateSpec packageKind nixSource =
  find (\templateSpec -> templateMatches templateSpec packageKind nixSource) templateSpecs
inferCheckTemplateSpec :: Map.Map FilePath PackageKind -> FilePath -> String -> Maybe CheckTemplateSpec
inferCheckTemplateSpec packageKinds checkName nixSource =
  find (\checkTemplateSpec -> checkTemplateMatches checkTemplateSpec packageKinds checkName nixSource) checkTemplateSpecs
normalizeNixExpr :: Set.Set T.Text -> Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeNixExpr = normalizeNixExprAt []
normalizeNixExprAt :: [T.Text] -> Set.Set T.Text -> Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeNixExprAt attributePath ignoredTopLevelFunctionParams allowedNixDifferenceKeys (Fix (Compose (AnnUnit nixExprSpan expressionFunctor))) =
  let rebuiltExpressionFunctor = case expressionFunctor of
        NSet isRecursive bindings -> NSet isRecursive (normalizeNixBindings attributePath allowedNixDifferenceKeys bindings)
        NLet bindings body -> NLet (normalizeNixBindings [] allowedNixDifferenceKeys bindings) (normalizeNixExprAt [] ignoredTopLevelFunctionParams allowedNixDifferenceKeys body)
        NAbs (ParamSet paramsEllipsis paramsAt params) body ->
          NAbs
            (ParamSet paramsEllipsis paramsAt (sortNixParams (filterIgnoredNixParams ignoredTopLevelFunctionParams params)))
            (normalizeNixExprAt [] Set.empty allowedNixDifferenceKeys body)
        NAbs (Param paramName) body -> NAbs (Param paramName) (normalizeNixExprAt [] Set.empty allowedNixDifferenceKeys body)
        otherNixExpr -> fmap (normalizeNixExprAt attributePath Set.empty allowedNixDifferenceKeys) otherNixExpr
   in Fix (Compose (AnnUnit nixExprSpan rebuiltExpressionFunctor))
filterIgnoredNixParams :: Set.Set T.Text -> [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
filterIgnoredNixParams ignoredTopLevelFunctionParams =
  filter (\(VarName paramName, _) -> Set.notMember paramName ignoredTopLevelFunctionParams)
sortNixParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortNixParams = sortOn (\(VarName paramName, _) -> paramName)
normalizeNixBindings :: [T.Text] -> Set.Set T.Text -> [Binding NExprLoc] -> [Binding NExprLoc]
normalizeNixBindings attributePath allowedNixDifferenceKeys bindings =
  [normalizeNixBinding attributePath allowedNixDifferenceKeys binding | binding <- bindings, not (isAllowedNixDifferenceBinding attributePath allowedNixDifferenceKeys binding)]
normalizeNixBinding :: [T.Text] -> Set.Set T.Text -> Binding NExprLoc -> Binding NExprLoc
normalizeNixBinding attributePath allowedNixDifferenceKeys = \case
  NamedVar keyPath bindingValue sourcePosition ->
    let bindingPath = attributePath ++ mapMaybe nixKeyNameText (NE.toList keyPath)
     in NamedVar keyPath (normalizeNixExprAt bindingPath Set.empty allowedNixDifferenceKeys bindingValue) sourcePosition
  Inherit maybeBoundNixExpr inheritedNames sourcePosition -> Inherit (normalizeNixExprAt attributePath Set.empty allowedNixDifferenceKeys <$> maybeBoundNixExpr) inheritedNames sourcePosition
isAllowedNixDifferenceBinding :: [T.Text] -> Set.Set T.Text -> Binding NExprLoc -> Bool
isAllowedNixDifferenceBinding attributePath allowedNixDifferenceKeys = \case
  NamedVar keyPath _ _ ->
    let bindingPath = attributePath ++ mapMaybe nixKeyNameText (NE.toList keyPath)
        dottedBindingPath = T.intercalate "." bindingPath
     in Set.member dottedBindingPath allowedNixDifferenceKeys
          || any (`Set.member` allowedNixDifferenceKeys) bindingPath
  Inherit _ inheritedNames _ ->
    all
      ( \(VarName inheritedName) ->
          let bindingPath = attributePath ++ [inheritedName]
              dottedBindingPath = T.intercalate "." bindingPath
           in Set.member dottedBindingPath allowedNixDifferenceKeys
                || any (`Set.member` allowedNixDifferenceKeys) bindingPath
      )
      inheritedNames
nixKeyNameText :: NKeyName NExprLoc -> Maybe T.Text
nixKeyNameText = \case
  StaticKey (VarName keyText) -> Just keyText
  DynamicKey (Plain (DoubleQuoted [Plain keyText])) -> Just keyText
  _ -> Nothing
renderNixExpr :: NExprLoc -> T.Text
renderNixExpr =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyNix
    . stripAnnotation
formatNixTemplateDifferences :: FilePath -> FilePath -> NExprLoc -> NExprLoc -> [String]
formatNixTemplateDifferences subjectNixPath templateBaselineNixPath subjectNixExpr templateBaselineExpr =
  let renderedPackageDefaultNix = renderNixExpr subjectNixExpr
      renderedTemplateDefaultNix = renderNixExpr templateBaselineExpr
   in if renderedPackageDefaultNix == renderedTemplateDefaultNix
        then []
        else
          let packageLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings subjectNixExpr)
              templateLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings templateBaselineExpr)
              packageFunctionParameterMap = fromMaybe Map.empty (extractTopLevelFunctionParameters subjectNixExpr)
              templateFunctionParameterMap = fromMaybe Map.empty (extractTopLevelFunctionParameters templateBaselineExpr)
              packagePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings subjectNixExpr)
              templatePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings templateBaselineExpr)
              functionParameterDifferenceLines =
                if Map.null packageFunctionParameterMap && Map.null templateFunctionParameterMap
                  then []
                  else formatBindingMapDifferences "parameter" packageFunctionParameterMap templateFunctionParameterMap
              letBindingDifferenceLines =
                if Map.null packageLetBindingMap && Map.null templateLetBindingMap
                  then []
                  else formatBindingMapDifferences "let key" packageLetBindingMap templateLetBindingMap
              primaryBindingDifferenceLines =
                if Map.null packagePrimaryBindingMap && Map.null templatePrimaryBindingMap
                  then []
                  else formatBindingMapDifferences "key" packagePrimaryBindingMap templatePrimaryBindingMap
              differenceDetailLines = functionParameterDifferenceLines ++ letBindingDifferenceLines ++ primaryBindingDifferenceLines
              renderedDifferenceDetailLines =
                if null differenceDetailLines
                  then
                    [ "  - expected normalized form: " ++ truncateDiagnosticValue (compactTextToSingleLine renderedTemplateDefaultNix),
                      "  - actual normalized form:   " ++ truncateDiagnosticValue (compactTextToSingleLine renderedPackageDefaultNix)
                    ]
                  else differenceDetailLines
           in [ subjectNixPath
                  ++ ": differs from template "
                  ++ templateBaselineNixPath
                  ++ " (excluding dependency keys)\n"
                  ++ intercalate "\n" renderedDifferenceDetailLines
              ]
formatBindingMapDifferences :: String -> Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> [String]
formatBindingMapDifferences keyLabel packageBindingMap templateBindingMap =
  let missingBindingKeys = Map.keys (Map.difference templateBindingMap packageBindingMap)
      unexpectedBindingKeys = Map.keys (Map.difference packageBindingMap templateBindingMap)
      changedBindings =
        Map.toAscList
          ( Map.filter
              (uncurry (/=))
              (Map.intersectionWith (,) templateBindingMap packageBindingMap)
          )
   in map (formatNixBindingDifferenceLine ("missing " ++ keyLabel)) missingBindingKeys
        ++ map (formatNixBindingDifferenceLine ("unexpected " ++ keyLabel)) unexpectedBindingKeys
        ++ map
          ( \(bindingKey, (expectedBindingValue, actualBindingValue)) ->
              "  - changed "
                ++ keyLabel
                ++ ": "
                ++ T.unpack bindingKey
                ++ "\n    expected: "
                ++ truncateDiagnosticValue (compactTextToSingleLine expectedBindingValue)
                ++ "\n    actual:   "
                ++ truncateDiagnosticValue (compactTextToSingleLine actualBindingValue)
          )
          changedBindings
formatNixBindingDifferenceLine :: String -> T.Text -> String
formatNixBindingDifferenceLine differenceKind bindingKey = "  - " ++ differenceKind ++ ": " ++ T.unpack bindingKey
compactTextToSingleLine :: T.Text -> String
compactTextToSingleLine = T.unpack . T.unwords . T.words
truncateDiagnosticValue :: String -> String
truncateDiagnosticValue textValue =
  let maxDiagnosticLength :: Int
      maxDiagnosticLength = 240
   in if length textValue <= maxDiagnosticLength
        then textValue
        else take maxDiagnosticLength textValue ++ "..."
firstMismatchedLine :: [T.Text] -> [T.Text] -> Maybe (Int, T.Text, T.Text)
firstMismatchedLine actualLines expectedLines =
  listToMaybe
    [ (lineNumber, actualLine, expectedLine)
    | (lineNumber, actualLine, expectedLine) <- zip3 [1 ..] actualLines expectedLines,
      actualLine /= expectedLine
    ]
extractTopLevelFunctionParameters :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractTopLevelFunctionParameters (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NAbs (ParamSet _ _ params) _ ->
      Just
        ( Map.fromList
            [ (paramName, maybe "required" (T.pack . compactTextToSingleLine . renderNixExpr) maybeDefaultValue)
            | (VarName paramName, maybeDefaultValue) <- params
            ]
        )
    NAbs (Param (VarName paramName)) _ -> Just (Map.singleton paramName "required")
    _ -> Nothing
extractPrimaryNixBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractPrimaryNixBindings nixExpression =
  Map.fromList . maximumBy (comparing length) . NE.toList
    <$> NE.nonEmpty (collectNixSetBindingGroups nixExpression)
extractOutermostLetBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractOutermostLetBindings (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NAbs _ body -> extractOutermostLetBindings body
    NLet bindings _ -> Just (Map.fromList (extractNamedNixBindings bindings))
    _ -> Nothing
collectNixSetBindingGroups :: NExprLoc -> [[(T.Text, T.Text)]]
collectNixSetBindingGroups (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NSet _ bindings ->
      let currentSetBindings = extractNamedNixBindings bindings
          nestedBindings = concatMap collectNixSetBindingGroupsFromBinding bindings
       in currentSetBindings : nestedBindings
    NLet bindings body ->
      let nestedFromBindings = concatMap collectNixSetBindingGroupsFromBinding bindings
       in nestedFromBindings ++ collectNixSetBindingGroups body
    NAbs _ body -> collectNixSetBindingGroups body
    otherNixExpr ->
      concatMap collectNixSetBindingGroups otherNixExpr
collectNixSetBindingGroupsFromBinding :: Binding NExprLoc -> [[(T.Text, T.Text)]]
collectNixSetBindingGroupsFromBinding (NamedVar _ bindingValue _) = collectNixSetBindingGroups bindingValue
collectNixSetBindingGroupsFromBinding (Inherit maybeBoundNixExpr _ _) = maybe [] collectNixSetBindingGroups maybeBoundNixExpr
extractNamedNixBindings :: [Binding NExprLoc] -> [(T.Text, T.Text)]
extractNamedNixBindings bindings =
  [ (bindingKey, T.pack (compactTextToSingleLine (renderNixExpr bindingValue)))
  | NamedVar keyPath bindingValue _ <- bindings,
    let bindingKey = T.intercalate "." (mapMaybe nixKeyNameText (NE.toList keyPath))
  ]
runPackageTests :: IO ()
runPackageTests = runPackageTestsWith hUnitPackageTests
runPackageTestsWith :: Test -> IO ()
runPackageTestsWith packageTests = do
  hUnitCounts <- runTestTT packageTests
  if errors hUnitCounts == 0 && failures hUnitCounts == 0
    then putStrLn "test ... ok"
    else exitFailure
hUnitPackageTests :: Test
hUnitPackageTests =
  TestList
    [ TestLabel "Discovers conventional Haskell tests as behavioral specifications." (TestCase haskellTestDiscoveryTest),
      TestLabel "Ignores non-test Haskell strings and comments during discovery." (TestCase haskellTestDiscoveryFalsePositiveTest),
      TestLabel "Uses Python test identifiers as behavioral specifications." (TestCase pythonTestDiscoveryTest),
      TestLabel "Discovers pytest test methods in conventional test classes." (TestCase pythonClassTestDiscoveryTest),
      TestLabel "Humanizes conventional test identifiers across frameworks." (TestCase testIdentifierSpecificationTest),
      TestLabel "Uses default.nix evidence only for markerless package classification." (TestCase packageDetectionTest),
      TestLabel "Ignores formatter-only Cargo inline-table spacing." (TestCase cargoTomlFormattingNormalizationTest),
      TestLabel "Requires Rust Nix and Cargo versions to match." (TestCase rustVersionSynchronizationTest),
      TestLabel "Requires allowlisted filesystem entry kinds." (TestCase entryKindStructureTest),
      TestLabel "Refuses to repair a non-regular root Git ignore entry." (TestCase rootGitignoreRepairSafetyTest),
      TestLabel "Rolls back scaffold files when generation fails." (TestCase scaffoldCreationRollbackTest),
      TestLabel "Uses standard Git ignore semantics for repository entries." (TestCase gitIgnoredRepositoryEntryTest),
      TestLabel "Renders only supplied repository whitelist paths." (TestCase minimalRootGitignoreRenderingTest),
      TestLabel "Treats parameter directories as opaque user data." (TestCase parameterDirectoryStructureTest),
      TestLabel "Renders stable text and JSON repository summaries." (TestCase repositorySummaryRenderingTest),
      TestLabel "Extracts local package dependencies." (TestCase localPackageDependencyExtractionTest),
      TestLabel "Reports concise Nix template parameter differences." (TestCase nixTemplateParameterDifferenceTest),
      TestLabel "Accepts python_template without inputs or shellHook." (TestCase pythonTemplateOptionalInputsAndShellHookTest),
      TestLabel "Documents help and invokes it consistently." (TestCase commandLineHelpEndToEndTest),
      TestLabel "Initializes flake repositories and rejects home initialization." (TestCase initializationEndToEndTest),
      TestLabel "Initializes packages and hosts from file and stdin status JSON." (TestCase statusImportEndToEndTest),
      TestLabel "Validates canonical project locations against origin." (TestCase initializationLocationEndToEndTest),
      TestLabel "Preserves existing initialization files and rejects inconsistent state." (TestCase initializationExistingFilesEndToEndTest),
      TestLabel "Disambiguates repository URLs and package additions." (TestCase addCommandParsingTest),
      TestLabel "Rejects unknown commands with usage on stderr." (TestCase invalidCommandEndToEndTest),
      TestLabel "Parses and validates canonical home repository resources." (TestCase homeRepositoryParsingTest),
      TestLabel "Detects, checks, and summarizes an empty home profile." (TestCase homeProfileEndToEndTest),
      TestLabel "Scaffolds a package and its check from a nested directory." (TestCase addPackageEndToEndTest),
      TestLabel "Removes a clean package, its check, and whitelist entries." (TestCase removePackageEndToEndTest),
      TestLabel "Removes generated checks after a package changes detected type." (TestCase removeChangedPackageTypeEndToEndTest),
      TestLabel "Refuses to remove packages containing local changes." (TestCase unsafeRemovePackageEndToEndTest),
      TestLabel "Protects root Git ignore changes during package mutations." (TestCase rootGitignoreMutationSafetyEndToEndTest),
      TestLabel "Rejects root whitelist drift." (TestCase rootGitignoreDriftEndToEndTest),
      TestLabel "Repairs the root whitelist from repository structure policy." (TestCase rootGitignoreStructurePolicyEndToEndTest),
      TestLabel "Repairs root whitelist drift in repositories that use a Gitfile." (TestCase gitFileRootGitignoreFixEndToEndTest),
      TestLabel "Scaffolds and checks every supported package kind." (TestCase allPackageKindsEndToEndTest),
      TestLabel "Rejects package creation when its path already exists." (TestCase existingPackageCollisionEndToEndTest),
      TestLabel "Reports generated package behavior in text and JSON status output." (TestCase statusEndToEndTest),
      TestLabel "Reports conventional tests for generated Haskell packages." (TestCase haskellStatusEndToEndTest),
      TestLabel "Rejects unlabeled HUnit cases in generated Haskell packages." (TestCase unlabeledHaskellPackageCheckEndToEndTest),
      TestLabel "Reports the phase and file when a package check fails." (TestCase corruptedPackageCheckEndToEndTest),
      TestLabel "Rejects unknown package options without creating partial output." (TestCase unknownAddOptionEndToEndTest),
      TestLabel "Rejects package names that violate the package convention." (TestCase invalidPackageNameEndToEndTest),
      TestLabel "Scaffolds package types without a combined check." (TestCase packageWithoutCheckEndToEndTest)
    ]
haskellTestDiscoveryTest :: IO ()
haskellTestDiscoveryTest =
  assertEqual
    "Standard HUnit labels and QuickCheck declarations form the Haskell test specification."
    ["Alpha behavior.", "Alpha.", "Beta \"quoted\" behavior.", "Beta."]
    ( discoverHaskellUnitTestNamesFromSource
        ( unlines
            [ "hUnitPackageTests =",
              "  TestList",
              "    [ TestLabel \"Beta \\\"quoted\\\" behavior.\" betaTest,",
              "      TestLabel",
              "        \"Alpha behavior.\"",
              "        alphaTest,",
              "      TestLabel \"Alpha behavior.\" duplicateTest",
              "    ]",
              "prop_beta :: Property",
              "prop_beta = property True",
              "prop_alpha = property True"
            ]
        )
    )
haskellTestDiscoveryFalsePositiveTest :: IO ()
haskellTestDiscoveryFalsePositiveTest =
  assertEqual
    "Comments, diagnostics, helper variables, character literals, and embedded source stay out of the specification."
    []
    ( discoverHaskellUnitTestNamesFromSource
        ( unlines
            [ "-- TestLabel \"Commented behavior.\" ignoredTest",
              "{- TestLabel \"Block-comment behavior.\" ignoredTest -}",
              "makeTest name = TestLabel name (TestCase action)",
              "assertEqual \"Failure diagnostic.\" expected actual",
              "embedded = \"TestLabel \\\"Embedded behavior.\\\" ignoredTest\"",
              "marker = 'x'",
              "value = prop_not_a_declaration"
            ]
        )
    )
pythonTestDiscoveryTest :: IO ()
pythonTestDiscoveryTest = do
  python3Path <- findExecutable "python3"
  forM_ python3Path $ \_ ->
    withTemporaryPackageRepository "python-test-inspection" $ \temporaryDirectory -> do
      let pythonSourcePath = temporaryDirectory </> "main.py"
      TIO.writeFile
        pythonSourcePath
        ( T.pack
            ( unlines
                [ "def test_undocumented_behavior() -> None:",
                  "    assert True",
                  "",
                  "async def test_multiline_definition(",
                  "    value: str,",
                  ") -> None:",
                  "    \"\"\"Handles a multiline definition.\"\"\"",
                  "    assert value",
                  "",
                  "class TestBehavior:",
                  "    def test_class_behavior(self) -> None:",
                  "        \"\"\"Handles a test class.\"\"\"",
                  "        assert True"
                ]
            )
        )
      discoveredTests <- inspectPythonTests pythonSourcePath
      assertEqual
        "Python AST inspection derives specifications from async, multiline, and class test identifiers."
        (Right ["Class behavior.", "Multiline definition.", "Undocumented behavior."])
        discoveredTests
pythonClassTestDiscoveryTest :: IO ()
pythonClassTestDiscoveryTest = do
  python3Path <- findExecutable "python3"
  forM_ python3Path $ \_ ->
    withTemporaryPackageRepository "python-class-test-inspection" $ \temporaryDirectory -> do
      let pythonSourcePath = temporaryDirectory </> "main.py"
      TIO.writeFile pythonSourcePath "class TestExample:\n  def test_behavior(self):\n    \"\"\"Reports class behavior.\"\"\"\n"
      discoveredTests <- inspectPythonTests pythonSourcePath
      assertEqual "Python AST inspection includes pytest test methods in Test classes." (Right ["Behavior."]) discoveredTests
testIdentifierSpecificationTest :: IO ()
testIdentifierSpecificationTest = do
  assertEqual
    "Framework prefixes, snake case, and camel case do not leak into specifications."
    ["Canonicalization is idempotent.", "Removing empty lines matches filtered sequence.", "Attribute sets canonicalize by key order.", "Parses URLs with CLI and preserves UTF-8.", "Rejects a non-regular top-level value."]
    ( map
        testSpecificationFromIdentifier
        [ "test_property_canonicalization_is_idempotent",
          "quickcheck_removing_empty_lines_matches_filtered_sequence",
          "prop_attributeSetsCanonicalizeByKeyOrder",
          "test_parses_urls_with_cli_and_preserves_utf8",
          "rejects_a_non_regular_top_level_value"
        ]
    )
  assertEqual
    "Status test sentences become conventional, stable language identifiers."
    ["test_open_api_contract_stays_explicit", "test_imported_behavior_must_fail_until_implemented", "test_imported_behavior_must_fail_until_implemented_2"]
    (statusTestIdentifiers "test" ["Open api contract stays explicit.", "Imported behavior must fail until implemented.", "Imported behavior must fail until implemented!"])
packageDetectionTest :: IO ()
packageDetectionTest =
  assertEqual
    "External Python source evidence refines markerless packages without hiding conflicting markers."
    [ DetectedPackageKind PythonPyPIPackage,
      AmbiguousPackageMarkers ("Main.hs" :| ["Cargo.lock+src/main.rs"])
    ]
    [ detectPackageFromEvidence [] (Just "{ buildPythonPackage = true; src = fetchPypi {}; }"),
      detectPackageFromEvidence (detectPackageMarkers ["Main.hs", "Cargo.lock", "src/main.rs"]) (Just "{ buildPythonPackage = true; src = fetchPypi {}; }")
    ]
cargoTomlFormattingNormalizationTest :: IO ()
cargoTomlFormattingNormalizationTest = case (parseToml rustCargoTomlBaseline, parseToml removeEmptyLinesCargoTomlFixture) of
  (Right baseline, Right fixture) ->
    assertEqual
      "Taplo's inline-table spacing does not change Cargo policy compliance."
      (normalizeCargoTomlForBaselineComparison "remove-empty-lines" baseline)
      (normalizeCargoTomlForBaselineComparison "remove-empty-lines" fixture)
  _ -> assertFailure "Cargo TOML fixtures must parse."
rustVersionSynchronizationTest :: IO ()
rustVersionSynchronizationTest =
  withTemporaryPackageRepository "rust-version-synchronization" $ \temporaryRepository -> do
    let packageRoot = temporaryRepository </> "packages/demo"
    createDirectoryIfMissing True packageRoot
    TIO.writeFile (packageRoot </> "Cargo.toml") (T.replace "remove-empty-lines" "demo" removeEmptyLinesCargoTomlFixture)
    TIO.writeFile (packageRoot </> "default.nix") "{ pkgs ? import <nixpkgs> { }, }: { version = \"0.0.0\"; }"
    issues <- withCurrentDirectory temporaryRepository (checkCargoToml "demo")
    assertBool
      "A Rust package must synchronize its Cargo and Nix versions."
      ( "packages/demo/default.nix: version must match Cargo.toml [package].version (expected \"0.1.0\", got \"0.0.0\")"
          `elem` issues
      )
entryKindStructureTest :: IO ()
entryKindStructureTest =
  withTemporaryPackageRepository "symbolic-link-structure" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    writeFile (temporaryRepository </> "README") ""
    createFileLink "README" (temporaryRepository </> "LICENSE")
    issues <- withCurrentDirectory temporaryRepository (snd <$> inspectRepositoryStructure)
    assertBool
      "entry-kind diagnostic"
      ("LICENSE: expected regular file, found symbolic link" `elem` issues)
rootGitignoreRepairSafetyTest :: IO ()
rootGitignoreRepairSafetyTest =
  withTemporaryPackageRepository "root-gitignore-repair-safety" $ \temporaryRepository -> do
    let targetPath = temporaryRepository </> "target"
    TIO.writeFile targetPath "outside content\n"
    createFileLink "target" (temporaryRepository </> ".gitignore")
    repairResult <- withCurrentDirectory temporaryRepository ensureRootGitignoreIsSafeToRepair
    assertBool "A symbolic-link .gitignore is rejected before repair." (either (const True) (const False) repairResult)
    targetContents <- TIO.readFile targetPath
    assertEqual "Rejecting repair leaves the symbolic-link target untouched." "outside content\n" targetContents
scaffoldCreationRollbackTest :: IO ()
scaffoldCreationRollbackTest =
  withTemporaryPackageRepository "scaffold-creation-rollback" $ \temporaryRepository ->
    withCurrentDirectory temporaryRepository $ do
      writeFile "checks" "not a directory"
      creationResult <-
        createScaffoldFiles
          ["packages/demo", "checks/demo"]
          [ ScaffoldFile "packages/demo/default.nix" "package scaffold",
            ScaffoldFile "checks/demo/default.nix" "check scaffold"
          ]
      assertBool "A scaffold write failure is returned as an error." (either (const True) (const False) creationResult)
      packageDirectoryExists <- doesPathExist "packages/demo"
      assertBool "A scaffold write failure removes files created earlier in the operation." (not packageDirectoryExists)
gitIgnoredRepositoryEntryTest :: IO ()
gitIgnoredRepositoryEntryTest =
  withTemporaryPackageRepository "git-ignored-repository-entry" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    TIO.writeFile
      (temporaryRepository </> ".gitignore")
      (T.unlines ["result", "ignored/", "tracked-artifact/", "LICENSE"])
    writeFile (temporaryRepository </> "README") ""
    createFileLink "README" (temporaryRepository </> "LICENSE")
    createFileLink "README" (temporaryRepository </> "result")
    createDirectoryIfMissing True (temporaryRepository </> "ignored")
    writeFile (temporaryRepository </> "ignored/artifact") ""
    createDirectoryIfMissing True (temporaryRepository </> "tracked-artifact")
    writeFile (temporaryRepository </> "tracked-artifact/unexpected.txt") ""
    writeFile (temporaryRepository </> "unexpected.txt") ""
    runGitFixtureCommand ["-C", temporaryRepository, "add", "-f", "--", ".gitignore", "README", "LICENSE", "tracked-artifact/unexpected.txt"]
    issues <- withCurrentDirectory temporaryRepository (snd <$> inspectRepositoryStructure)
    assertBool
      "A tracked path remains subject to entry-kind validation even when an ignore rule matches it."
      ("LICENSE: expected regular file, found symbolic link" `elem` issues)
    assertBool
      "A visible untracked path remains subject to repository structure policy."
      ("unexpected.txt: is not allowed" `elem` issues)
    assertBool
      "A tracked descendant remains subject to repository structure policy when its directory is ignored."
      ("tracked-artifact/unexpected.txt: is not allowed" `elem` issues)
    assertBool
      "Ignored untracked files, directories, and symlinks do not participate in repository structure policy."
      (not (any (\issue -> "result" `isPrefixOf` issue || "ignored" `isPrefixOf` issue) issues))
minimalRootGitignoreRenderingTest :: IO ()
minimalRootGitignoreRenderingTest =
  assertEqual
    "Whitelist rendering does not invent optional repository paths."
    (T.unlines ["*", "!/.gitignore", "!/flake.nix", "!/prm/", "!/prm/**"])
    (renderRootGitignore (whitelistPathsForExactFile "flake.nix" ++ whitelistPathsForTree "prm"))
parameterDirectoryStructureTest :: IO ()
parameterDirectoryStructureTest =
  withTemporaryPackageRepository "parameter-directory-structure" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    let parameterDirectories =
          [ temporaryRepository </> "prm",
            temporaryRepository </> "hosts/demo/prm",
            temporaryRepository </> "packages/demo/prm"
          ]
    createDirectoryIfMissing True (temporaryRepository </> "hosts/demo")
    createDirectoryIfMissing True (temporaryRepository </> "packages/demo")
    writeFile (temporaryRepository </> "hosts/demo/configuration.nix") ""
    writeFile (temporaryRepository </> "packages/demo/default.nix") ""
    forM_ parameterDirectories $ \parameterDirectory -> do
      createDirectoryIfMissing True (parameterDirectory </> "arbitrary/nested")
      writeFile (parameterDirectory </> "arbitrary/nested/user.data") ""
    issues <- withCurrentDirectory temporaryRepository (snd <$> inspectRepositoryStructure)
    assertEqual "Parameter directory contents do not participate in repository structure policy." [] issues
nixTemplateParameterDifferenceTest :: IO ()
nixTemplateParameterDifferenceTest = do
  actualParseResult <- parseNixExprFromText "{ pkgs ? import <nixpkgs> {} }: let python = pkgs.python3; in { pname = baseNameOf ./.; }"
  expectedParseResult <- parseNixExprFromText "{ inputs, pkgs ? import <nixpkgs> {} }: let python = pkgs.python3; in { pname = baseNameOf ./.; }"
  case (actualParseResult, expectedParseResult) of
    (Right actualExpression, Right expectedExpression) ->
      assertEqual
        "Only the missing function parameter is reported."
        ["packages/demo/default.nix: differs from template packages/example_template/default.nix (excluding dependency keys)\n  - missing parameter: inputs"]
        (formatNixTemplateDifferences "packages/demo/default.nix" "packages/example_template/default.nix" actualExpression expectedExpression)
    _ -> assertFailure "The Nix parameter-difference fixtures must parse."
pythonTemplateOptionalInputsAndShellHookTest :: IO ()
pythonTemplateOptionalInputsAndShellHookTest = do
  actualParseResult <- parseNixExprFromText "{ pkgs ? import <nixpkgs> {} }: pkgs.python3.pkgs.buildPythonPackage { pname = baseNameOf ./.; }"
  expectedParseResult <- parseNixExprFromText "{ inputs, pkgs ? import <nixpkgs> {} }: pkgs.python3.pkgs.buildPythonPackage { pname = baseNameOf ./.; shellHook = inputs.example; }"
  case (actualParseResult, expectedParseResult) of
    (Right actualExpression, Right expectedExpression) ->
      assertEqual
        "The python template's inputs parameter and shellHook binding may both be omitted."
        []
        ( formatNixTemplateDifferences
            "packages/demo/default.nix"
            "packages/python_template/default.nix"
            (normalizeNixExpr optionalFunctionParams (Set.singleton "shellHook") actualExpression)
            (normalizeNixExpr optionalFunctionParams (Set.singleton "shellHook") expectedExpression)
        )
    _ -> assertFailure "The optional python-template fixtures must parse."
  where
    optionalFunctionParams = optionalTemplateFunctionParams PythonPackage "packages/python_template/default.nix"
repositorySummaryRenderingTest :: IO ()
repositorySummaryRenderingTest = do
  let packageSummary =
        RepositoryPackageSummary
          { repositoryPackageName = "demo",
            repositoryPackageKind = PythonPackage,
            repositoryPackageDescription = Nothing,
            repositoryPackageDependencies = ["alpha", "demo-two"],
            repositoryPackageTestNames = ["Reports \"quoted\" behavior."],
            repositoryPackageTestStatus = RepositoryTestsConfigured
          }
      repositorySummary =
        RepositorySummary
          { repositorySummaryReadme = Just "Demo repository.\n\nIts intent is visible.",
            repositorySummaryPackages = [packageSummary],
            repositorySummaryHosts = ["default"]
          }
  assertEqual
    "Test status vocabulary distinguishes configured tests."
    ["not-configured", "configured"]
    ( map
        renderRepositoryPackageTestStatus
        [ RepositoryTestsNotConfigured,
          repositoryPackageTestStatus packageSummary
        ]
    )
  assertEqual
    "JSON rendering preserves the schema and escapes strings."
    (Right (repositoryStatusJSON [repositorySummary]))
    (Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack (renderRepositorySummariesJSON [repositorySummary]))))
  assertEqual
    "JSON string rendering escapes control characters."
    "\"line one\\nline two\\u0001\""
    (renderJSON ("line one\nline two\1" :: String))
  assertEqual
    "Nix string rendering prevents interpolation and preserves control characters."
    "(builtins.fromJSON \"\\\"\\${name}\\\\u0001\\\"\")"
    (renderNixString "${name}\1")
localPackageDependencyExtractionTest :: IO ()
localPackageDependencyExtractionTest =
  assertEqual
    "Only unique, sorted local package references are reported."
    ["alpha", "demo-two"]
    ( extractLocalPackageDependencies
        ( T.unwords
            [ "inputs.self.packages.${pkgs.stdenv.system}.demo-two",
              "inputs.self.packages.${pkgs.system}.ignored",
              "inputs.agenix-shell.lib",
              "inputs.self.packages.${pkgs.stdenv.system}.alpha",
              "inputs.self.packages.${pkgs.stdenv.system}.alpha"
            ]
        )
    )
commandLineHelpEndToEndTest :: IO ()
commandLineHelpEndToEndTest =
  withTemporaryPackageRepository "command-line-help" $ \temporaryDirectory -> do
    (helpExit, helpStdout, helpStderr) <- runEndToEndCommandIn temporaryDirectory ["-h"]
    assertEqual "The top-level help command succeeds." ExitSuccess helpExit
    assertBool
      "The top-level help command prints concise usage and commands to stdout."
      (all (`isInfixOf` helpStdout) ["Usage:", "init", "add", "rm", "check", "status"])
    assertEqual "The top-level help command leaves stderr empty." "" helpStderr
    (commandHelpExit, commandHelpStdout, commandHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["check", "--help"]
    assertEqual "A command-specific help request succeeds." ExitSuccess commandHelpExit
    assertBool "A command-specific help request prints specific help to stdout." ("Usage:" `isInfixOf` commandHelpStdout && "--fix" `isInfixOf` commandHelpStdout)
    assertEqual "A command-specific help request lists each help flag once." 1 (length (filter (== "  -h,--help                Show this help text") (lines commandHelpStdout)))
    assertEqual "A command-specific help request leaves stderr empty." "" commandHelpStderr
    (initHelpExit, initHelpStdout, initHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["init", "--help"]
    assertEqual "Init help succeeds." ExitSuccess initHelpExit
    assertBool "Init help documents its directory and status-import option." ("Usage:" `isInfixOf` initHelpStdout && "DIRECTORY" `isInfixOf` initHelpStdout && "--from-status FILE|-" `isInfixOf` initHelpStdout)
    assertEqual "Init help leaves stderr empty." "" initHelpStderr
initializationEndToEndTest :: IO ()
initializationEndToEndTest =
  withTemporaryPackageRepository "initialization-home" $ \temporaryHome ->
    withTemporaryPackageRepository "initialization-tools" $ \temporaryTools -> do
      environment <- initializationTestEnvironment temporaryHome temporaryTools
      (homeExit, homeStdout, homeStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", temporaryHome]
      assertEqual "Home initialization is rejected." (ExitFailure 1) homeExit
      assertEqual "Home initialization leaves stdout empty." "" homeStdout
      assertBool "Home initialization directs users to the documented Git setup." ("initialize the home repository with Git" `isInfixOf` homeStderr)
      homeGitMetadataExists <- doesPathExist (temporaryHome </> ".git")
      homeGitignoreExists <- doesPathExist (temporaryHome </> ".gitignore")
      assertBool "Rejected home initialization does not create Git metadata or a whitelist." (not homeGitMetadataExists && not homeGitignoreExists)
      initializeGitRepositoryFixture temporaryHome
      let project = temporaryHome </> "github.com/owner/demo"
      (projectExit, projectStdout, projectStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project]
      assertEqual "Project initialization creates a missing nested path and succeeds without origin." ExitSuccess projectExit
      assertBool "Project initialization preserves Git's normal success output." ("Initialized empty Git repository" `isInfixOf` projectStdout)
      assertBool "Project initialization makes network-dependent work visible." ("Locking flake inputs..." `isInfixOf` projectStderr)
      projectFlake <- TIO.readFile (project </> "flake.nix")
      projectLock <- TIO.readFile (project </> "flake.lock")
      projectGitignore <- TIO.readFile (project </> ".gitignore")
      assertEqual "Project initialization writes the minimal Blueprint flake." minimalProjectFlakeSource projectFlake
      assertEqual "The test Nix command writes a deterministic lock." "{}\n" projectLock
      assertEqual "Project initialization writes the canonical root whitelist." "*\n!/.gitignore\n!/flake.lock\n!/flake.nix\n" projectGitignore
      (projectStatusExit, projectStatusStdout, _projectStatusStderr) <- readGitProcess ["-C", project, "status", "--porcelain=v1"] ""
      assertEqual "The initialized project is a Git repository." ExitSuccess projectStatusExit
      assertEqual "Project initialization leaves all generated files unstaged." "?? .gitignore\n?? flake.lock\n?? flake.nix\n" projectStatusStdout
      (repeatExit, repeatStdout, repeatStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project]
      assertEqual "Repeated project initialization succeeds." ExitSuccess repeatExit
      assertBool "Repeated initialization preserves Git's reinitialization output." ("Reinitialized existing Git repository" `isInfixOf` repeatStdout)
      assertEqual "Repeated initialization does not repeat lock work." "" repeatStderr
      repeatedFiles <- mapM TIO.readFile [project </> "flake.nix", project </> "flake.lock", project </> ".gitignore"]
      assertEqual "Repeated initialization preserves generated files." [projectFlake, projectLock, projectGitignore] repeatedFiles
      let offlineProject = temporaryHome </> "--help"
      (offlineExit, offlineStdout, offlineStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", "--", offlineProject]
      assertEqual "Option termination permits a dash-prefixed directory." ExitSuccess offlineExit
      assertBool "Dash-prefixed initialization preserves Git's output." ("Initialized empty Git repository" `isInfixOf` offlineStdout)
      assertBool "Dash-prefixed project initialization reports lock generation." ("Locking flake inputs..." `isInfixOf` offlineStderr)
      offlineLockExists <- doesPathExist (offlineProject </> "flake.lock")
      assertBool "Project initialization consistently generates its lock." offlineLockExists
      let failedProject = temporaryHome </> "failed-lock"
          fakeNixPath = temporaryTools </> "nix"
      TIO.writeFile fakeNixPath "#!/bin/sh\nexit 2\n"
      (failedExit, _, _) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", failedProject]
      assertEqual "A failed lock phase propagates its exit status." (ExitFailure 2) failedExit
      failedProjectExists <- doesPathExist failedProject
      assertBool "A failed initialization removes a target created by the command." (not failedProjectExists)
statusImportEndToEndTest :: IO ()
statusImportEndToEndTest =
  withTemporaryPackageRepository "status-import-home" $ \temporaryHome ->
    withTemporaryPackageRepository "status-import-tools" $ \temporaryTools -> do
      environment <- initializationTestEnvironment temporaryHome temporaryTools
      let statusFile = temporaryTools </> "status.json"
          project = temporaryHome </> "projects/demo"
          stdinProject = temporaryHome </> "projects/stdin-demo"
          unsupportedProject = temporaryHome </> "projects/unsupported"
          statusSource =
            T.unlines
              [ "{",
                "  \"repositoryType\": \"flake\",",
                "  \"readme\": \"Imported repository README.\\n\",",
                "  \"resources\": [",
                "    {\"kind\": \"package\", \"name\": \"dependency\", \"type\": \"c\", \"description\": null, \"dependencies\": []},",
                "    {\"kind\": \"package\", \"name\": \"demo\", \"type\": \"python\", \"description\": \"Demo package\", \"dependencies\": [\"dependency\"], \"tests\": {\"status\": \"configured\", \"cases\": [{\"name\": \"Imported behavior must fail until implemented.\"}, {\"name\": \"Open api contract stays explicit.\"}]}},",
                "    {\"kind\": \"host\", \"name\": \"default\"}",
                "  ]",
                "}"
              ]
      TIO.writeFile
        statusFile
        statusSource
      (initExit, _initStdout, initStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project, "--from-status", statusFile]
      assertEqual "Status import initializes the target repository." ExitSuccess initExit
      assertBool "Status import reports lock generation." ("Locking flake inputs..." `isInfixOf` initStderr)
      packageExists <- doesFileExist (project </> "packages/demo/main.py")
      hostExists <- doesFileExist (project </> "hosts/default/configuration.nix")
      assertBool "Status import creates each package scaffold." packageExists
      assertBool "Status import creates each host scaffold." hostExists
      (statusExit, statusStdout, statusStderr) <- runEndToEndCommandWithEnvironment project environment ["status"]
      assertEqual "Imported repository status succeeds." ExitSuccess statusExit
      assertEqual "Imported repository status emits no diagnostics." "" statusStderr
      assertBool "Status omits location-specific roots." (not ("\"root\"" `isInfixOf` statusStdout))
      assertBool "Status import preserves the README." ("\"readme\":\"Imported repository README.\\n\"" `isInfixOf` statusStdout)
      assertBool "Status import preserves package dependencies." ("\"dependencies\":[\"dependency\"]" `isInfixOf` statusStdout)
      assertBool "Status import preserves absent package descriptions." ("\"name\":\"dependency\",\"type\":\"c\"" `isInfixOf` statusStdout && "\"description\":null" `isInfixOf` statusStdout)
      assertBool "Status import preserves test case names." ("Imported behavior must fail until implemented." `isInfixOf` statusStdout && "Open api contract stays explicit." `isInfixOf` statusStdout)
      importedPythonSource <- TIO.readFile (project </> "packages/demo/main.py")
      assertBool "Imported test placeholders fail instead of using the passing sample test." ("TODO: implement imported contract" `T.isInfixOf` importedPythonSource && "def test_imported_behavior_must_fail_until_implemented()" `T.isInfixOf` importedPythonSource && "def test_open_api_contract_stays_explicit()" `T.isInfixOf` importedPythonSource && not ("Prints the sample message from the executable." `T.isInfixOf` importedPythonSource))
      assertBool "Status includes the imported host." ("\"kind\":\"host\",\"name\":\"default\"" `isInfixOf` statusStdout)
      (stdinInitExit, _stdinInitStdout, stdinInitStderr) <- runEndToEndCommandWithEnvironmentAndInput "/tmp" environment ["init", stdinProject, "--from-status", "-"] (T.unpack statusSource)
      assertEqual "Standard-input status import initializes the explicit target." ExitSuccess stdinInitExit
      assertBool "Standard-input import reports lock generation." ("Locking flake inputs..." `isInfixOf` stdinInitStderr)
      stdinPackageExists <- doesFileExist (stdinProject </> "packages/demo/main.py")
      stdinHostExists <- doesFileExist (stdinProject </> "hosts/default/configuration.nix")
      stdinReadmeExists <- doesFileExist (stdinProject </> "README")
      assertBool "Standard-input import creates each package scaffold." stdinPackageExists
      assertBool "Standard-input import creates each host scaffold." stdinHostExists
      assertBool "Standard-input import creates the README." stdinReadmeExists
      TIO.writeFile statusFile "{\"repositoryType\":\"flake\",\"readme\":null,\"resources\":[{\"kind\":\"package\",\"name\":\"external\",\"type\":\"binary-release\",\"description\":null,\"dependencies\":[]}]}"
      (unsupportedExit, _unsupportedStdout, unsupportedStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", unsupportedProject, "--from-status", statusFile]
      assertEqual "Unsupported status resources fail before initialization." (ExitFailure 1) unsupportedExit
      assertBool "Unsupported status resources identify their type." ("cannot scaffold package type from status: binary-release" `isInfixOf` unsupportedStderr)
      unsupportedProjectExists <- doesPathExist unsupportedProject
      assertBool "Unsupported status resources leave no partial target." (not unsupportedProjectExists)
initializationLocationEndToEndTest :: IO ()
initializationLocationEndToEndTest =
  withTemporaryPackageRepository "initialization-location-home" $ \temporaryHome ->
    withTemporaryPackageRepository "initialization-location-tools" $ \temporaryTools -> do
      environment <- initializationTestEnvironment temporaryHome temporaryTools
      let canonicalProject = temporaryHome </> "github.com/owner/demo"
      createDirectoryIfMissing True canonicalProject
      initializeGitRepositoryFixture canonicalProject
      runGitFixtureCommand ["-C", canonicalProject, "remote", "add", "origin", "git@github.com:owner/demo.git"]
      (matchingExit, _, matchingStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", canonicalProject]
      assertEqual "A project at its origin-derived location initializes." ExitSuccess matchingExit
      assertBool "Matching location initialization reports lock generation." ("Locking flake inputs..." `isInfixOf` matchingStderr)
      let mismatchedProject = temporaryHome </> "github.com/example/other"
      createDirectoryIfMissing True mismatchedProject
      initializeGitRepositoryFixture mismatchedProject
      runGitFixtureCommand ["-C", mismatchedProject, "remote", "add", "origin", "git@github.com:owner/demo.git"]
      (mismatchExit, mismatchStdout, mismatchStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", mismatchedProject]
      assertEqual "A project at a noncanonical origin-derived location is rejected." (ExitFailure 1) mismatchExit
      assertEqual "Location rejection leaves stdout empty." "" mismatchStdout
      assertBool "Location rejection reports actual and expected paths." ("project location does not match origin" `isInfixOf` mismatchStderr && canonicalProject `isInfixOf` mismatchStderr)
      flakeWasCreated <- doesPathExist (mismatchedProject </> "flake.nix")
      assertBool "Location validation happens before project files are written." (not flakeWasCreated)
initializationExistingFilesEndToEndTest :: IO ()
initializationExistingFilesEndToEndTest =
  withTemporaryPackageRepository "initialization-existing-home" $ \temporaryHome ->
    withTemporaryPackageRepository "initialization-existing-tools" $ \temporaryTools -> do
      environment <- initializationTestEnvironment temporaryHome temporaryTools
      let project = temporaryHome </> "project"
      createDirectoryIfMissing True project
      TIO.writeFile (project </> "flake.lock") "existing-lock\n"
      (orphanLockExit, _, orphanLockStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project]
      assertEqual "A lock without a flake is completed non-destructively." ExitSuccess orphanLockExit
      assertBool "Completing an existing lock does not run the lock phase." (not ("Locking flake inputs..." `isInfixOf` orphanLockStderr))
      preservedLock <- TIO.readFile (project </> "flake.lock")
      assertEqual "An existing lock is preserved byte-for-byte." "existing-lock\n" preservedLock
      removeFile (project </> "flake.lock")
      let existingFlake :: T.Text
          existingFlake = "{ outputs = _: {}; }\n"
      TIO.writeFile (project </> "flake.nix") existingFlake
      (existingExit, _, existingStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project]
      assertEqual "Initialization completes an existing flake." ExitSuccess existingExit
      assertBool "Completing an existing flake reports lock generation." ("Locking flake inputs..." `isInfixOf` existingStderr)
      preservedFlake <- TIO.readFile (project </> "flake.nix")
      assertEqual "An existing flake is preserved byte-for-byte." existingFlake preservedFlake
      TIO.writeFile (project </> ".gitignore") "not canonical\n"
      (whitelistExit, _, whitelistStderr) <- runEndToEndCommandWithEnvironment "/tmp" environment ["init", project]
      assertEqual "Reinitialization preserves a noncanonical existing whitelist." ExitSuccess whitelistExit
      assertEqual "Preserving existing files is quiet." "" whitelistStderr
      preservedGitignore <- TIO.readFile (project </> ".gitignore")
      assertEqual "An existing whitelist is preserved byte-for-byte." "not canonical\n" preservedGitignore
addCommandParsingTest :: IO ()
addCommandParsingTest = do
  assertEqual "Init accepts exactly one local path." (Just (InitCommand (InitSpec "project" Nothing))) (parseCommandForTest ["init", "project"])
  assertEqual "Init accepts an explicit status source." (Just (InitCommand (InitSpec "project" (Just "-")))) (parseCommandForTest ["init", "project", "--from-status", "-"])
  assertEqual "Init without a path defaults to the current directory." (Just (InitCommand (InitSpec "." Nothing))) (parseCommandForTest ["init"])
  assertEqual "A single add argument is rejected." Nothing (parseCommandForTest ["add", "python"])
  assertEqual
    "The option terminator permits a description beginning with a dash."
    (Just (AddPackageCommand "python" "demo" (Just "--documented behavior")))
    (parseCommandForTest ["add", "python", "demo", "--", "--documented", "behavior"])
invalidCommandEndToEndTest :: IO ()
invalidCommandEndToEndTest =
  withTemporaryPackageRepository "invalid-command" $ \temporaryDirectory -> do
    (invalidCommandExit, invalidCommandStdout, invalidCommandStderr) <- runEndToEndCommandIn temporaryDirectory ["unknown-command"]
    assertEqual "An invalid command uses Git's usage exit status." usageExitCode invalidCommandExit
    assertEqual "An invalid command leaves stdout empty." "" invalidCommandStdout
    assertBool "An invalid command prints an error and usage to stderr." ("Invalid argument" `isInfixOf` invalidCommandStderr && "Usage:" `isInfixOf` invalidCommandStderr)
    (incompleteAddExit, incompleteAddStdout, incompleteAddStderr) <- runEndToEndCommandIn temporaryDirectory ["add", "python"]
    assertEqual "An incomplete add command uses Git's usage exit status." usageExitCode incompleteAddExit
    assertEqual "An incomplete add command leaves stdout empty." "" incompleteAddStdout
    assertBool "An incomplete add command prints usage on stderr." ("Missing:" `isInfixOf` incompleteAddStderr && "Usage:" `isInfixOf` incompleteAddStderr)
    (summaryExit, summaryStdout, summaryStderr) <- runEndToEndCommandIn temporaryDirectory ["summary"]
    assertEqual "The retired summary command uses Git's usage exit status." usageExitCode summaryExit
    assertEqual "The retired summary command leaves stdout empty." "" summaryStdout
    assertBool "The retired summary command prints an error and usage to stderr." ("Invalid argument" `isInfixOf` summaryStderr && "Usage:" `isInfixOf` summaryStderr)
    forM_ ["--version", "completion", "remove"] $ \retiredCommand -> do
      (retiredExit, retiredStdout, retiredStderr) <- runEndToEndCommandIn temporaryDirectory [retiredCommand]
      assertEqual (retiredCommand ++ " uses Git's usage exit status.") usageExitCode retiredExit
      assertEqual (retiredCommand ++ " leaves stdout empty.") "" retiredStdout
      assertBool (retiredCommand ++ " is reported as invalid.") (not (null retiredStderr) && "Usage:" `isInfixOf` retiredStderr)
homeRepositoryParsingTest :: IO ()
homeRepositoryParsingTest = do
  forM_
    [ "https://github.com/owner/demo.git",
      "http://github.com/owner/demo.git",
      "ssh://git@github.com/owner/demo.git",
      "ssh://somebody@github.com:2222/owner/demo.git",
      "git+ssh://git@github.com/owner/demo.git/",
      "git://github.com/owner/demo.git",
      "git@github.com:owner/demo.git"
    ]
    (assertEqual "Supported repository URLs share one canonical path." (Right "github.com/owner/demo") . canonicalHomeRepositoryPath)
  forM_
    [ "../demo.git",
      "file:///tmp/demo.git",
      "https://github.com/",
      "https://github.com/owner/../demo.git",
      "git@github.com:owner//demo.git"
    ]
    (assertBool "Unsafe or unsupported repository URLs are rejected." . either (const True) (const False) . canonicalHomeRepositoryPath)
  assertEqual
    "A complete .gitmodules record becomes a repository resource."
    (Right [HomeRepository "demo" "github.com/owner/demo" "https://github.com/owner/demo.git"])
    (parseHomeRepositoryConfig "submodule.demo.path\ngithub.com/owner/demo\0submodule.demo.url\nhttps://github.com/owner/demo.git\0")
  assertEqual
    "Standard optional submodule metadata is ignored while its path and URL are retained."
    (Right [HomeRepository "demo" "github.com/owner/demo" "https://github.com/owner/demo.git"])
    (parseHomeRepositoryConfig "submodule.demo.path\ngithub.com/owner/demo\0submodule.demo.url\nhttps://github.com/owner/demo.git\0submodule.demo.branch\nmain\0")
  assertBool
    "Duplicate .gitmodules fields are rejected."
    ( either
        (any ("must have exactly one path" `isInfixOf`))
        (const False)
        (parseHomeRepositoryConfig "submodule.demo.path\none/demo\0submodule.demo.path\ntwo/demo\0submodule.demo.url\nhttps://example.test/owner/demo.git\0")
    )
  withTemporaryPackageRepository "remote-alias-resolution" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    runGitFixtureCommand ["-C", temporaryRepository, "config", "url.git@github.com:.insteadOf", "gh:"]
    resolvedUrl <- resolveGitRemoteUrl temporaryRepository "gh:owner/demo.git"
    assertEqual "Git-configured URL aliases are resolved without contacting the remote." "git@github.com:owner/demo.git" resolvedUrl
homeProfileEndToEndTest :: IO ()
homeProfileEndToEndTest =
  withTemporaryPackageRepository "home-profile-end-to-end" $ \temporaryDirectory -> do
    initializeGitRepositoryFixture temporaryDirectory
    TIO.writeFile (temporaryDirectory </> ".gitignore") homeGitignoreSource
    gitignoreSource <- TIO.readFile (temporaryDirectory </> ".gitignore")
    assertEqual "Standard Git home setup writes the canonical whitelist." homeGitignoreSource gitignoreSource
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryDirectory ["check"]
    assertEqual "An empty home profile passes its check." ExitSuccess checkExit
    assertEqual "An empty home check emits no stdout." "" checkStdout
    assertEqual "An empty home check emits no stderr." "" checkStderr
    TIO.writeFile (temporaryDirectory </> ".gitignore") "*\n!/.custom/\n!/.gitmodules\n"
    (fixExit, fixStdout, fixStderr) <- runEndToEndCommandIn temporaryDirectory ["check", "--fix"]
    assertEqual "Fixing a user-edited home whitelist succeeds." ExitSuccess fixExit
    assertEqual "Fixing a home whitelist emits no stdout." "" fixStdout
    assertEqual "Fixing a home whitelist emits no stderr." "" fixStderr
    fixedGitignoreSource <- TIO.readFile (temporaryDirectory </> ".gitignore")
    assertEqual
      "Fixing a home whitelist preserves user entries and adds only missing structural entries."
      "*\n!/.custom/\n!/.gitmodules\n!/.gitignore\n"
      fixedGitignoreSource
    (statusExit, statusStdout, statusStderr) <- runEndToEndCommandIn temporaryDirectory ["status"]
    assertEqual "An empty home status succeeds." ExitSuccess statusExit
    assertEqual
      "An empty home status uses the common profile envelope."
      (Right (homeStatusJSON []))
      (Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack statusStdout)))
    assertEqual "An empty home status emits no stderr." "" statusStderr
addPackageEndToEndTest :: IO ()
addPackageEndToEndTest =
  withTemporaryPackageRepository "add-package-end-to-end" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    TIO.writeFile (temporaryRepository </> "flake.nix") "{}"
    TIO.writeFile (temporaryRepository </> "flake.lock") "{}"
    let nestedDirectory = temporaryRepository </> "packages"
    createDirectoryIfMissing True nestedDirectory
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn
        nestedDirectory
        ["add", "python", "demo", "Demo package"]
    assertEqual "Adding a package through the installed CLI succeeds." ExitSuccess addExit
    assertEqual "A successful add produces no stdout." "" addStdout
    assertEqual "A successful add leaves stderr empty." "" addStderr
    generatedFilesExist <-
      and
        <$> mapM
          doesFileExist
          [ temporaryRepository </> ".gitignore",
            temporaryRepository </> "packages/demo/default.nix",
            temporaryRepository </> "packages/demo/main.py",
            temporaryRepository </> "checks/demo_coverage/default.nix"
          ]
    assertBool "The installed CLI creates the package and its check on disk." generatedFilesExist
    localGitignoreExists <- doesFileExist (temporaryRepository </> "packages/demo/.gitignore")
    assertBool "The package has no local .gitignore." (not localGitignoreExists)
    rootGitignoreSource <- TIO.readFile (temporaryRepository </> ".gitignore")
    assertBool
      "The root whitelist includes the package and its check."
      ( "!/packages/demo/main.py" `T.isInfixOf` rootGitignoreSource
          && "!/checks/demo_coverage/default.nix" `T.isInfixOf` rootGitignoreSource
      )
    assertBool
      "The root whitelist omits optional paths that are absent."
      (not ("secrets" `T.isInfixOf` rootGitignoreSource) && not ("formatter.nix" `T.isInfixOf` rootGitignoreSource))
removePackageEndToEndTest :: IO ()
removePackageEndToEndTest =
  withGeneratedPythonPackageRepository "remove-package-end-to-end" $ \temporaryRepository -> do
    (dryRunExit, dryRunStdout, dryRunStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "--dry-run", "demo"]
    assertEqual "Dry-run removal succeeds." ExitSuccess dryRunExit
    assertBool "Dry-run removal reports package, check, and whitelist changes." (all (`isInfixOf` dryRunStdout) ["rm 'packages/demo'", "rm 'checks/demo_coverage'", "update '.gitignore'"])
    assertEqual "Dry-run removal leaves stderr empty." "" dryRunStderr
    packageStillExists <- doesPathExist (temporaryRepository </> "packages/demo")
    assertBool "Dry-run removal does not change the package." packageStillExists
    (removeExit, removeStdout, removeStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removing a clean package succeeds." ExitSuccess removeExit
    assertBool "A successful removal preserves Git's path output." ("rm 'packages/demo/main.py'" `isInfixOf` removeStdout)
    assertEqual "A successful removal leaves stderr empty." "" removeStderr
    packageExists <- doesPathExist (temporaryRepository </> "packages/demo")
    checkExists <- doesPathExist (temporaryRepository </> "checks/demo_coverage")
    assertBool "The package is removed from disk." (not packageExists)
    assertBool "The conventional check is removed from disk." (not checkExists)
    rootGitignoreSource <- TIO.readFile (temporaryRepository </> ".gitignore")
    assertBool "The removed package and check leave no whitelist entries." (not ("demo" `T.isInfixOf` rootGitignoreSource))
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "The repository remains canonical after removal." ExitSuccess checkExit
    assertEqual "The successful post-removal check leaves stdout empty." "" checkStdout
    assertEqual "The successful post-removal check leaves stderr empty." "" checkStderr
    (missingExit, missingStdout, missingStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removing an absent package fails." (ExitFailure 1) missingExit
    assertEqual "Missing-package removal leaves stdout empty." "" missingStdout
    assertBool "Missing-package removal identifies the package path." ("package does not exist: packages/demo" `isInfixOf` missingStderr)
removeChangedPackageTypeEndToEndTest :: IO ()
removeChangedPackageTypeEndToEndTest =
  withGeneratedPythonPackageRepository "remove-changed-package-type-end-to-end" $ \temporaryRepository -> do
    let pythonSourcePath = temporaryRepository </> "packages/demo/main.py"
        cSourcePath = temporaryRepository </> "packages/demo/main.c"
    renameFile pythonSourcePath cSourcePath
    runGitFixtureCommand ["-C", temporaryRepository, "add", "--update", "--", "packages/demo"]
    runGitFixtureCommand ["-C", temporaryRepository, "add", "--force", "--", "packages/demo/main.c"]
    runGitFixtureCommand ["-C", temporaryRepository, "commit", "--quiet", "-m", "Change generated package type"]
    (removeExit, removeStdout, removeStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removing a package with changed markers succeeds." ExitSuccess removeExit
    assertBool "Removal includes the package's former generated check." ("rm 'checks/demo_coverage/default.nix'" `isInfixOf` removeStdout)
    assertEqual "Removal of a changed package leaves stderr empty." "" removeStderr
    orphanedCheckExists <- doesPathExist (temporaryRepository </> "checks/demo_coverage")
    assertBool "Removal leaves no former generated check behind." (not orphanedCheckExists)
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "Removing a package with changed markers keeps the repository canonical." ExitSuccess checkExit
    assertEqual "The post-removal check leaves stdout empty." "" checkStdout
    assertEqual "The post-removal check leaves stderr empty." "" checkStderr
unsafeRemovePackageEndToEndTest :: IO ()
unsafeRemovePackageEndToEndTest = do
  assertUnsafeRemove "modified-remove-package" $ \temporaryRepository ->
    TIO.appendFile (temporaryRepository </> "packages/demo/main.py") "\n# local change\n"
  assertUnsafeRemove "staged-remove-package" $ \temporaryRepository -> do
    TIO.appendFile (temporaryRepository </> "packages/demo/main.py") "\n# staged change\n"
    runGitFixtureCommand ["-C", temporaryRepository, "add", "--", "packages/demo/main.py"]
  assertUnsafeRemove "untracked-remove-package" $ \temporaryRepository -> do
    createDirectoryIfMissing True (temporaryRepository </> "packages/demo/prm")
    TIO.writeFile (temporaryRepository </> "packages/demo/prm/local") "local"
  assertUnsafeRemove "ignored-remove-package" $ \temporaryRepository ->
    TIO.writeFile (temporaryRepository </> "packages/demo/cache.bin") "ignored"
  withGeneratedPythonPackageRepository "forced-remove-package" $ \temporaryRepository -> do
    TIO.appendFile (temporaryRepository </> "packages/demo/main.py") "\n# local change\n"
    (forceExit, forceStdout, forceStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "--force", "demo"]
    assertEqual "Forced removal permits local package changes." ExitSuccess forceExit
    assertBool "Forced removal preserves Git's path output." ("rm 'packages/demo/main.py'" `isInfixOf` forceStdout)
    assertEqual "Forced removal leaves stderr empty." "" forceStderr
rootGitignoreMutationSafetyEndToEndTest :: IO ()
rootGitignoreMutationSafetyEndToEndTest =
  withGeneratedPythonPackageRepository "root-gitignore-mutation-safety" $ \temporaryRepository -> do
    let rootGitignorePath = temporaryRepository </> ".gitignore"
    TIO.appendFile rootGitignorePath "# local change\n"
    dirtyRootGitignore <- TIO.readFile rootGitignorePath
    (dryRunExit, dryRunStdout, dryRunStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "--dry-run", "demo"]
    assertEqual "Dry-run removal rejects a changed root Git ignore file." (ExitFailure 1) dryRunExit
    assertEqual "A refused dry-run leaves stdout empty." "" dryRunStdout
    assertBool "The dry-run refusal identifies the changed root Git ignore file." ("root .gitignore is not clean" `isInfixOf` dryRunStderr)
    (removeExit, removeStdout, removeStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removal rejects a changed root Git ignore file." (ExitFailure 1) removeExit
    assertEqual "A refused removal leaves stdout empty." "" removeStdout
    assertBool "The refusal identifies the changed root Git ignore file." ("root .gitignore is not clean" `isInfixOf` removeStderr)
    rootGitignoreAfterRemove <- TIO.readFile rootGitignorePath
    assertEqual "A refused removal preserves root Git ignore content." dirtyRootGitignore rootGitignoreAfterRemove
    packageExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "A refused removal leaves the package intact." packageExists
    (addExit, addStdout, addStderr) <- runEndToEndCommandIn temporaryRepository ["add", "python", "other"]
    assertEqual "Addition rejects a changed root Git ignore file." (ExitFailure 1) addExit
    assertEqual "A refused addition leaves stdout empty." "" addStdout
    assertBool "The refusal identifies the changed root Git ignore file." ("root .gitignore is not clean" `isInfixOf` addStderr)
    rootGitignoreAfterAdd <- TIO.readFile rootGitignorePath
    assertEqual "A refused addition preserves root Git ignore content." dirtyRootGitignore rootGitignoreAfterAdd
    addedPackageExists <- doesDirectoryExist (temporaryRepository </> "packages/other")
    assertBool "A refused addition creates no package files." (not addedPackageExists)
assertUnsafeRemove :: String -> (FilePath -> IO ()) -> IO ()
assertUnsafeRemove fixtureName preparePackage =
  withGeneratedPythonPackageRepository fixtureName $ \temporaryRepository -> do
    preparePackage temporaryRepository
    (removeExit, removeStdout, removeStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removing an unsafe package fails." (ExitFailure 1) removeExit
    assertEqual "A refused removal leaves stdout empty." "" removeStdout
    assertBool "The refusal explains that the target is not clean." ("package or check is not clean" `isInfixOf` removeStderr)
    packageExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "A refused removal leaves the package intact." packageExists
rootGitignoreDriftEndToEndTest :: IO ()
rootGitignoreDriftEndToEndTest =
  withGeneratedPythonPackageRepository "root-gitignore-drift-end-to-end" $ \temporaryRepository -> do
    TIO.appendFile (temporaryRepository </> ".gitignore") "# drift\n"
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "A changed root whitelist fails the repository check." (ExitFailure 1) checkExit
    assertEqual "A whitelist failure leaves stdout empty." "" checkStdout
    assertBool
      "The check identifies the root whitelist mismatch."
      (".gitignore: does not match the canonical root whitelist" `isInfixOf` checkStderr)
    (fixExit, fixStdout, fixStderr) <- runEndToEndCommandIn temporaryRepository ["check", "--fix"]
    assertEqual "Fixing a changed root whitelist succeeds." ExitSuccess fixExit
    assertEqual "A successful fix leaves stdout empty." "" fixStdout
    assertEqual "A successful fix leaves stderr empty." "" fixStderr
    repairedRootGitignore <- TIO.readFile (temporaryRepository </> ".gitignore")
    assertBool "The repaired root whitelist removes the drift." (not ("# drift" `T.isInfixOf` repairedRootGitignore))
rootGitignoreStructurePolicyEndToEndTest :: IO ()
rootGitignoreStructurePolicyEndToEndTest =
  withGeneratedPythonPackageRepository "root-gitignore-structure-policy-end-to-end" $ \temporaryRepository -> do
    TIO.writeFile (temporaryRepository </> ".gitignore") ""
    TIO.writeFile (temporaryRepository </> "formatter.nix") "{}\n"
    createFileLink "flake.nix" (temporaryRepository </> "README")
    createFileLink "flake.nix" (temporaryRepository </> "result")
    (fixExit, fixStdout, fixStderr) <- runEndToEndCommandIn temporaryRepository ["check", "--fix"]
    assertEqual "Fixing an empty root whitelist from structure policy succeeds." ExitSuccess fixExit
    assertEqual "A successful structure-policy fix leaves stdout empty." "" fixStdout
    assertEqual "A successful structure-policy fix leaves stderr empty." "" fixStderr
    repairedRootGitignore <- TIO.readFile (temporaryRepository </> ".gitignore")
    assertBool "A valid untracked root file is whitelisted." ("!/formatter.nix\n" `T.isInfixOf` repairedRootGitignore)
    assertBool "An unrelated build result is not whitelisted." (not ("!/result\n" `T.isInfixOf` repairedRootGitignore))
    assertBool "A wrong-kind entry at an allowed path is not whitelisted." (not ("!/README\n" `T.isInfixOf` repairedRootGitignore))
    forM_ ["result", "README"] $ \ignoredPath -> do
      (ignoreExit, _ignoreStdout, ignoreStderr) <- readGitProcess ["-C", temporaryRepository, "check-ignore", "--quiet", "--", ignoredPath] ""
      assertEqual (ignoredPath ++ " is ignored after repair: " ++ ignoreStderr) ExitSuccess ignoreExit
gitFileRootGitignoreFixEndToEndTest :: IO ()
gitFileRootGitignoreFixEndToEndTest =
  withGeneratedPythonPackageRepository "gitfile-root-gitignore-fix-end-to-end" $ \temporaryRepository -> do
    let separateGitDirectory = temporaryRepository ++ "-git-directory"
        removeSeparateGitDirectory = do
          separateGitDirectoryExists <- doesPathExist separateGitDirectory
          when separateGitDirectoryExists (removePathForcibly separateGitDirectory)
        testGitFileRepository = do
          runGitFixtureCommand ["init", "--quiet", "--separate-git-dir=" ++ separateGitDirectory, temporaryRepository]
          gitMetadataIsFile <- doesFileExist (temporaryRepository </> ".git")
          assertBool "The fixture uses a root .git Gitfile." gitMetadataIsFile
          TIO.writeFile (temporaryRepository </> ".gitignore") ""
          (fixExit, fixStdout, fixStderr) <- runEndToEndCommandIn temporaryRepository ["check", "--fix"]
          assertEqual "Fixing an empty root whitelist in a Gitfile repository succeeds." ExitSuccess fixExit
          assertEqual "A successful Gitfile fix leaves stdout empty." "" fixStdout
          assertEqual "A successful Gitfile fix leaves stderr empty." "" fixStderr
          repairedRootGitignore <- TIO.readFile (temporaryRepository </> ".gitignore")
          assertBool "The repaired root whitelist excludes Git metadata." (not ("!/.git\n" `T.isInfixOf` repairedRootGitignore))
          (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
          assertEqual "The repaired Gitfile repository passes a subsequent check." ExitSuccess checkExit
          assertEqual "A successful Gitfile repository check leaves stdout empty." "" checkStdout
          assertEqual "A successful Gitfile repository check leaves stderr empty." "" checkStderr
    testGitFileRepository `finally` removeSeparateGitDirectory
allPackageKindsEndToEndTest :: IO ()
allPackageKindsEndToEndTest =
  withEmptyCanonicalRepository "all-package-kinds-end-to-end" $ \temporaryRepository -> do
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.name", "Canonicalization Tests"]
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.email", "canonicalization@example.test"]
    forM_
      [ ("haskell", "demo-haskell", "Main.hs", Just "demo-haskell-coverage"),
        ("rust", "demo-rust", "Cargo.lock", Just "demo-rust-coverage"),
        ("html", "demo_html", "index.html", Nothing),
        ("python", "demo_python", "main.py", Just "demo_python_coverage"),
        ("python-latex", "demo_python_latex", "main.py", Just "demo_python_latex_coverage"),
        ("python-pypi", "demo_pypi", "default.nix", Nothing),
        ("c", "demo_c", "main.c", Nothing),
        ("terraform", "demo_terraform", "main.tf", Nothing),
        ("latex", "demo_latex", "ms.tex", Nothing)
      ]
      $ \(packageKindName, packageName, markerFile, maybeCheckName) -> do
        (addExit, addStdout, addStderr) <-
          runEndToEndCommandIn
            temporaryRepository
            ["add", packageKindName, packageName, "E2E package"]
        assertEqual ("Scaffolding " ++ packageKindName ++ " succeeds.") ExitSuccess addExit
        assertEqual ("Scaffolding " ++ packageKindName ++ " leaves stdout empty.") "" addStdout
        assertEqual ("Scaffolding " ++ packageKindName ++ " leaves stderr empty.") "" addStderr
        markerExists <- doesFileExist (temporaryRepository </> "packages" </> packageName </> markerFile)
        assertBool ("Scaffolding " ++ packageKindName ++ " creates its marker file.") markerExists
        when (packageKindName == "rust") $ do
          generatedRustSource <- TIO.readFile (temporaryRepository </> "packages" </> packageName </> "src/main.rs")
          assertBool
            "Generated Rust executable tests isolate their working directory in a child process."
            ( "Command::new(executable).current_dir(root).output()?" `T.isInfixOf` generatedRustSource
                && not ("env::set_current_dir" `T.isInfixOf` generatedRustSource)
            )
        forM_ maybeCheckName $ \checkName -> do
          checkExists <- doesFileExist (temporaryRepository </> "checks" </> checkName </> "default.nix")
          assertBool ("Scaffolding " ++ packageKindName ++ " creates its combined check.") checkExists
        runGitFixtureCommand ["-C", temporaryRepository, "commit", "--quiet", "-m", "Add " ++ packageName]
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "The repository containing every supported scaffold passes its check." ExitSuccess checkExit
    assertEqual "The successful scaffold-matrix check leaves stdout empty." "" checkStdout
    assertEqual "The successful scaffold-matrix check leaves stderr empty." "" checkStderr
existingPackageCollisionEndToEndTest :: IO ()
existingPackageCollisionEndToEndTest =
  withGeneratedHaskellPackageRepository "existing-package-collision" $ \temporaryRepository -> do
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn
        temporaryRepository
        ["add", "haskell", "demo"]
    assertEqual "Creating an existing package fails." (ExitFailure 1) addExit
    assertEqual "The collision produces no stdout." "" addStdout
    assertBool
      "The collision reports the package path."
      ("path already exists: packages/demo" `isInfixOf` addStderr)
statusEndToEndTest :: IO ()
statusEndToEndTest =
  withGeneratedPythonPackageRepository "status-end-to-end" $ \temporaryRepository -> do
    let repositoryReadme :: String
        repositoryReadme = "Demo repository README.\n"
    TIO.writeFile (temporaryRepository </> "README") (T.pack repositoryReadme)
    (fixExit, fixStdout, fixStderr) <- runEndToEndCommandIn temporaryRepository ["check", "--fix"]
    assertEqual "Fixing the whitelist discovers the valid untracked README." ExitSuccess fixExit
    assertEqual "A successful README whitelist fix leaves stdout empty." "" fixStdout
    assertEqual "A successful README whitelist fix leaves stderr empty." "" fixStderr
    (statusExit, statusStdout, statusStderr) <- runEndToEndCommandIn temporaryRepository ["status"]
    assertEqual "JSON status succeeds for the generated repository." ExitSuccess statusExit
    assertEqual
      "JSON status exactly reports generated metadata, checks, and discovered tests."
      ( renderRepositorySummariesJSON
          [RepositorySummary (Just repositoryReadme) [expectedGeneratedPythonPackageSummary] []]
      )
      statusStdout
    assertEqual "Status does not evaluate coverage checks." "" statusStderr
    (emptyExit, emptyStdout, emptyStderr) <- runEndToEndCommandIn temporaryRepository []
    assertEqual "An omitted subcommand uses Git's usage exit status." usageExitCode emptyExit
    assertEqual "An omitted subcommand leaves stdout empty." "" emptyStdout
    assertBool "An omitted subcommand prints an error and usage to stderr." ("Missing:" `isInfixOf` emptyStderr && "Usage:" `isInfixOf` emptyStderr)
haskellStatusEndToEndTest :: IO ()
haskellStatusEndToEndTest =
  withGeneratedHaskellPackageRepository "haskell-status-end-to-end" $ \temporaryRepository -> do
    (statusExit, statusStdout, statusStderr) <- runEndToEndCommandIn temporaryRepository ["status"]
    assertEqual "A generated Haskell package status succeeds." ExitSuccess statusExit
    assertEqual
      "The status reports its conventional HUnit label."
      ( renderRepositorySummariesJSON
          [RepositorySummary Nothing [expectedGeneratedHaskellPackageSummary] []]
      )
      statusStdout
    assertEqual "Status does not evaluate coverage checks." "" statusStderr
unlabeledHaskellPackageCheckEndToEndTest :: IO ()
unlabeledHaskellPackageCheckEndToEndTest =
  withGeneratedHaskellPackageRepository "unlabeled-haskell-check-end-to-end" $ \temporaryRepository -> do
    let mainPath = temporaryRepository </> "packages/demo/Main.hs"
    mainSource <- TIO.readFile mainPath
    TIO.writeFile mainPath (T.replace "TestLabel \"Renders the sample message.\" $ " "" mainSource)
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "An unlabeled HUnit package fails its repository check." (ExitFailure 1) checkExit
    assertEqual "An unlabeled HUnit package produces no stdout." "" checkStdout
    assertBool
      "The check explains the literal TestLabel convention."
      ("HUnit tests must use literal TestLabel descriptions" `isInfixOf` checkStderr)
corruptedPackageCheckEndToEndTest :: IO ()
corruptedPackageCheckEndToEndTest =
  withGeneratedPythonPackageRepository "corrupted-check-end-to-end" $ \temporaryRepository -> do
    TIO.writeFile (temporaryRepository </> "packages/demo/default.nix") "not valid nix template"
    (failedCheckExit, failedCheckStdout, failedCheckStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "Checking a corrupted package fails." (ExitFailure 1) failedCheckExit
    assertEqual "A failed check leaves stdout empty." "" failedCheckStdout
    assertBool
      "A failed check reports its phase and affected file."
      ( "git canonicalization check failed at phase: file-compliance" `isInfixOf` failedCheckStderr
          && "packages/demo/default.nix:" `isInfixOf` failedCheckStderr
      )
unknownAddOptionEndToEndTest :: IO ()
unknownAddOptionEndToEndTest =
  withEmptyCanonicalRepository "unknown-add-option-end-to-end" $ \temporaryRepository -> do
    (unknownOptionExit, unknownOptionStdout, unknownOptionStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "python", "demo", "--unknown"]
    assertEqual "An unknown add option uses Git's usage exit status." usageExitCode unknownOptionExit
    assertEqual "An unknown add option leaves stdout empty." "" unknownOptionStdout
    assertBool
      "An unknown add option prints an error and add usage to stderr."
      ("Invalid option" `isInfixOf` unknownOptionStderr && "Usage:" `isInfixOf` unknownOptionStderr && "add" `isInfixOf` unknownOptionStderr)
    packageDirectoryExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "An unknown option does not leave a partial package directory." (not packageDirectoryExists)
invalidPackageNameEndToEndTest :: IO ()
invalidPackageNameEndToEndTest =
  withEmptyCanonicalRepository "invalid-package-name-end-to-end" $ \temporaryRepository -> do
    (invalidNameExit, invalidNameStdout, invalidNameStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "python", "demo-python"]
    assertEqual "An invalid package name fails." (ExitFailure 1) invalidNameExit
    assertEqual "An invalid package name leaves stdout empty." "" invalidNameStdout
    assertBool "An invalid package name reports its convention." ("must use snake_case" `isInfixOf` invalidNameStderr)
    packageDirectoryExists <- doesDirectoryExist (temporaryRepository </> "packages/demo-python")
    assertBool "An invalid name does not leave a partial package directory." (not packageDirectoryExists)
packageWithoutCheckEndToEndTest :: IO ()
packageWithoutCheckEndToEndTest =
  withEmptyCanonicalRepository "package-without-check-end-to-end" $ \temporaryRepository -> do
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "html", "demo"]
    assertEqual "Adding a package type without a combined check succeeds." ExitSuccess addExit
    assertEqual "Adding the package leaves stdout empty." "" addStdout
    assertEqual "Adding the package leaves stderr empty." "" addStderr
    packageDirectoryExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "The package is created." packageDirectoryExists
    checkDirectoryExists <- doesPathExist (temporaryRepository </> "checks/demo")
    assertBool "No unsupported check is invented." (not checkDirectoryExists)
withEmptyCanonicalRepository :: String -> (FilePath -> IO a) -> IO a
withEmptyCanonicalRepository temporaryName action =
  withTemporaryPackageRepository temporaryName $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    TIO.writeFile (temporaryRepository </> "flake.nix") "{}"
    TIO.writeFile (temporaryRepository </> "flake.lock") "{}"
    action temporaryRepository
withGeneratedPythonPackageRepository :: String -> (FilePath -> IO a) -> IO a
withGeneratedPythonPackageRepository temporaryName action =
  withEmptyCanonicalRepository temporaryName $ \temporaryRepository -> do
    let nestedDirectory = temporaryRepository </> "packages"
    createDirectoryIfMissing True nestedDirectory
    (addExit, _addStdout, addStderr) <-
      runEndToEndCommandIn
        nestedDirectory
        ["add", "python", "demo", "Demo package"]
    when (addExit /= ExitSuccess) $
      assertFailure ("Failed to generate the Python package fixture: " ++ addStderr)
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.name", "Canonicalization Tests"]
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.email", "canonicalization@example.test"]
    runGitFixtureCommand ["-C", temporaryRepository, "commit", "--quiet", "-m", "Add generated package"]
    action temporaryRepository
withGeneratedHaskellPackageRepository :: String -> (FilePath -> IO a) -> IO a
withGeneratedHaskellPackageRepository temporaryName action =
  withEmptyCanonicalRepository temporaryName $ \temporaryRepository -> do
    let nestedDirectory = temporaryRepository </> "packages"
    createDirectoryIfMissing True nestedDirectory
    (addExit, _addStdout, addStderr) <-
      runEndToEndCommandIn
        nestedDirectory
        ["add", "haskell", "demo", "Demo package"]
    when (addExit /= ExitSuccess) $
      assertFailure ("Failed to generate the Haskell package fixture: " ++ addStderr)
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.name", "Canonicalization Tests"]
    runGitFixtureCommand ["-C", temporaryRepository, "config", "user.email", "canonicalization@example.test"]
    runGitFixtureCommand ["-C", temporaryRepository, "commit", "--quiet", "-m", "Add generated package"]
    action temporaryRepository
expectedGeneratedPythonPackageSummary :: RepositoryPackageSummary
expectedGeneratedPythonPackageSummary =
  RepositoryPackageSummary
    { repositoryPackageName = "demo",
      repositoryPackageKind = PythonPackage,
      repositoryPackageDescription = Just "Demo package",
      repositoryPackageDependencies = [],
      repositoryPackageTestNames =
        ["Main prints sample message."],
      repositoryPackageTestStatus = RepositoryTestsConfigured
    }
expectedGeneratedHaskellPackageSummary :: RepositoryPackageSummary
expectedGeneratedHaskellPackageSummary =
  RepositoryPackageSummary
    { repositoryPackageName = "demo",
      repositoryPackageKind = HaskellPackage,
      repositoryPackageDescription = Nothing,
      repositoryPackageDependencies = [],
      repositoryPackageTestNames = ["Renders the sample message."],
      repositoryPackageTestStatus = RepositoryTestsConfigured
    }
runEndToEndCommandIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runEndToEndCommandIn workingDirectory arguments =
  withCurrentDirectory workingDirectory $
    readProcessWithExitCode "git-canonicalization" arguments ""
runEndToEndCommandWithEnvironment :: FilePath -> [(String, String)] -> [String] -> IO (ExitCode, String, String)
runEndToEndCommandWithEnvironment workingDirectory environment arguments =
  runEndToEndCommandWithEnvironmentAndInput workingDirectory environment arguments ""
runEndToEndCommandWithEnvironmentAndInput :: FilePath -> [(String, String)] -> [String] -> String -> IO (ExitCode, String, String)
runEndToEndCommandWithEnvironmentAndInput workingDirectory environment arguments input =
  withCurrentDirectory workingDirectory $
    readCreateProcessWithExitCode (proc "git-canonicalization" arguments) {env = Just environment} input
initializationTestEnvironment :: FilePath -> FilePath -> IO [(String, String)]
initializationTestEnvironment home toolsDirectory = do
  let fakeNixPath = toolsDirectory </> "nix"
  TIO.writeFile
    fakeNixPath
    ( T.unlines
        [ "#!/bin/sh",
          "if [ \"$1\" = flake ] && [ \"$2\" = lock ] && [ \"$3\" = path:. ]; then",
          "  printf '{}\\n' > flake.lock",
          "  exit 0",
          "fi",
          "exit 2"
        ]
    )
  Posix.setFileMode fakeNixPath 0o755
  environment <- getEnvironment
  let existingPath = fromMaybe "" (lookup "PATH" environment)
  environmentWithOverrides
    [ ("HOME", home),
      ("PATH", toolsDirectory ++ ":" ++ existingPath),
      ("GIT_CANONICALIZATION_NIX", fakeNixPath)
    ]
environmentWithOverrides :: [(String, String)] -> IO [(String, String)]
environmentWithOverrides replacements = do
  environment <- getEnvironment
  pure (replacements ++ filter (\(name, _) -> name `notElem` map fst replacements) environment)
initializeGitRepositoryFixture :: FilePath -> IO ()
initializeGitRepositoryFixture repositoryPath =
  findExecutable "git" >>= \case
    Nothing -> assertFailure "git is required for command-line end-to-end tests"
    Just _ -> do
      (gitInitExit, _gitInitStdout, gitInitStderr) <- readGitProcess ["init", "--quiet", repositoryPath] ""
      when (gitInitExit /= ExitSuccess) $
        assertFailure ("Failed to initialize Git repository fixture: " ++ gitInitStderr)
runGitFixtureCommand :: [String] -> IO ()
runGitFixtureCommand arguments = do
  (gitExit, _gitStdout, gitStderr) <- readGitProcess arguments ""
  when (gitExit /= ExitSuccess) $
    assertFailure ("Git fixture command failed: git " ++ unwords arguments ++ if null gitStderr then "" else ": " ++ gitStderr)
withTemporaryPackageRepository :: String -> (FilePath -> IO a) -> IO a
withTemporaryPackageRepository tempDirName action = do
  temporaryDirectory <- getTemporaryDirectory
  temporaryPath <- mkdtemp (temporaryDirectory </> tempDirName ++ ".XXXXXX")
  action temporaryPath `finally` removePathForcibly temporaryPath
pythonMainSource :: T.Text
pythonMainSource =
  T.unlines
    [ "#!/usr/bin/env python3",
      "\"\"\"Provide a minimal executable Python package.\"\"\"",
      "",
      "from __future__ import annotations",
      "",
      "import os",
      "import subprocess",
      "",
      "SAMPLE_MESSAGE = \"Hello World Python\"",
      "",
      "",
      "def main() -> None:",
      "    \"\"\"Print the package's sample message.\"\"\"",
      "    print(SAMPLE_MESSAGE)  # noqa: T201",
      "",
      "",
      "def test_main_prints_sample_message() -> None:",
      "    \"\"\"Prints the sample message from the executable.\"\"\"",
      "    completed = subprocess.run(  # noqa: S603",
      "        [os.environ[\"PACKAGE_E2E_EXECUTABLE\"]],",
      "        check=False,",
      "        capture_output=True,",
      "        text=True,",
      "    )",
      "    if (",
      "        completed.returncode != 0",
      "        or completed.stdout != f\"{SAMPLE_MESSAGE}\\n\"",
      "        or completed.stderr",
      "    ):",
      "        message = \"executable output should match the sample message\"",
      "        raise AssertionError(message)",
      "",
      "",
      "if __name__ == \"__main__\":",
      "    main()"
    ]
pythonLaTeXMainSource :: T.Text
pythonLaTeXMainSource =
  T.unlines
    [ "#!/usr/bin/env python3",
      "\"\"\"Generate Python-produced artifacts for a LaTeX build.\"\"\"",
      "",
      "from __future__ import annotations",
      "",
      "import math",
      "import os",
      "import subprocess",
      "import tempfile",
      "from dataclasses import dataclass",
      "from pathlib import Path",
      "from typing import TYPE_CHECKING",
      "",
      "import matplotlib as mpl",
      "",
      "mpl.use(\"Agg\")",
      "",
      "import matplotlib.pyplot as plt",
      "",
      "if TYPE_CHECKING:",
      "    from collections.abc import Iterable",
      "LATEX_ESCAPES = {",
      "    \"\\\\\": r\"\\textbackslash{}\",",
      "    \"&\": r\"\\&\",",
      "    \"%\": r\"\\%\",",
      "    \"$\": r\"\\$\",",
      "    \"#\": r\"\\#\",",
      "    \"_\": r\"\\_\",",
      "    \"{\": r\"\\{\",",
      "    \"}\": r\"\\}\",",
      "    \"~\": r\"\\textasciitilde{}\",",
      "    \"^\": r\"\\textasciicircum{}\",",
      "}",
      "",
      "",
      "@dataclass(frozen=True, init=False, slots=True)",
      "class Samples:",
      "    \"\"\"An immutable, non-empty sequence with finite summary statistics.\"\"\"",
      "",
      "    values: tuple[float, ...]",
      "",
      "    def __init__(self, values: Iterable[float]) -> None:",
      "        \"\"\"Freeze values after validating every summary operation.\"\"\"",
      "        normalized = tuple(values)",
      "        if not normalized:",
      "            message = \"samples must not be empty\"",
      "            raise ValueError(message)",
      "        if not all(map(math.isfinite, normalized)):",
      "            message = \"samples must be finite\"",
      "            raise ValueError(message)",
      "        try:",
      "            total = math.fsum(normalized)",
      "        except OverflowError as error:",
      "            message = \"sample total must be finite\"",
      "            raise ValueError(message) from error",
      "        if not math.isfinite(total):",
      "            message = \"sample total must be finite\"",
      "            raise ValueError(message)",
      "        object.__setattr__(self, \"values\", normalized)",
      "",
      "    def summary(self) -> tuple[tuple[str, float], ...]:",
      "        \"\"\"Return summary rows in display order.\"\"\"",
      "        total = math.fsum(self.values)",
      "        return (",
      "            (\"count\", float(len(self.values))),",
      "            (\"total\", total),",
      "            (\"mean\", total / len(self.values)),",
      "            (\"min\", min(self.values)),",
      "            (\"max\", max(self.values)),",
      "        )",
      "",
      "",
      "DEFAULT_SAMPLES = Samples((2.0, 3.5, 5.0, 9.5, 12.0))",
      "",
      "",
      "def latex_escape(text: str) -> str:",
      "    \"\"\"Escape LaTeX-special characters.\"\"\"",
      "    return \"\".join(LATEX_ESCAPES.get(character, character) for character in text)",
      "",
      "",
      "def render_table(samples: Samples) -> str:",
      "    \"\"\"Render summary statistics as a LaTeX tabular.\"\"\"",
      "    lines = [",
      "        \"\\\\begin{tabular}{lr}\",",
      "        \"\\\\toprule\",",
      "        \"Metric & Value \\\\\\\\\",",
      "        \"\\\\midrule\",",
      "        *(",
      "            f\"{latex_escape(metric)} & {value:.2f} \\\\\\\\\"",
      "            for metric, value in samples.summary()",
      "        ),",
      "        \"\\\\bottomrule\",",
      "        \"\\\\end{tabular}\",",
      "        \"\",",
      "    ]",
      "    return \"\\n\".join(lines)",
      "",
      "",
      "def create_figure(path: Path, samples: Samples) -> None:",
      "    \"\"\"Create a deterministic figure for the LaTeX document.\"\"\"",
      "    figure, axis = plt.subplots(figsize=(5, 3))",
      "    axis.plot(",
      "        range(1, len(samples.values) + 1),",
      "        samples.values,",
      "        color=\"#1f77b4\",",
      "        linewidth=2.5,",
      "        marker=\"o\",",
      "    )",
      "    axis.set_xlabel(\"Sample index\")",
      "    axis.set_ylabel(\"Value\")",
      "    axis.set_title(\"Python-generated figure\")",
      "    axis.grid(alpha=0.3)",
      "    figure.tight_layout()",
      "    figure.savefig(path, dpi=200)",
      "    plt.close(figure)",
      "",
      "",
      "def create_workspace_artifacts(",
      "    workspace: Path,",
      "    samples: Samples = DEFAULT_SAMPLES,",
      ") -> None:",
      "    \"\"\"Generate all artifacts required by the LaTeX document.\"\"\"",
      "    workspace.mkdir(parents=True, exist_ok=True)",
      "    create_figure(workspace / \"figure.png\", samples)",
      "    (workspace / \"table.tex\").write_text(render_table(samples), encoding=\"utf-8\")",
      "",
      "",
      "def main() -> None:",
      "    \"\"\"Generate the LaTeX build workspace artifacts.\"\"\"",
      "    create_workspace_artifacts(Path.cwd() / \"tmp\")",
      "",
      "",
      "def test_samples_rejects_invalid_values() -> None:",
      "    \"\"\"Rejects empty, non-finite, and overflowing sample sequences.\"\"\"",
      "    for values in ((), (math.nan,), (math.inf,), (1e308, 1e308)):",
      "        try:",
      "            Samples(values)",
      "        except ValueError:",
      "            continue",
      "        message = \"invalid samples must be rejected\"",
      "        raise AssertionError(message)",
      "",
      "",
      "def test_latex_escape_handles_special_characters() -> None:",
      "    \"\"\"Escapes LaTeX-special characters in generated text.\"\"\"",
      "    escaped = latex_escape(r\"value_#1 & 50%\")",
      "    if escaped != r\"value\\_\\#1 \\& 50\\%\":",
      "        message = \"LaTeX-special characters should be escaped\"",
      "        raise AssertionError(message)",
      "",
      "",
      "def test_main_creates_latex_workspace_artifacts() -> None:",
      "    \"\"\"Creates the LaTeX workspace artifacts from the executable.\"\"\"",
      "    with tempfile.TemporaryDirectory() as temporary_directory:",
      "        working_directory = Path(temporary_directory)",
      "        completed = subprocess.run(  # noqa: S603",
      "            [os.environ[\"PACKAGE_E2E_EXECUTABLE\"]],",
      "            cwd=working_directory,",
      "            check=False,",
      "            capture_output=True,",
      "            text=True,",
      "        )",
      "        if completed.returncode != 0 or completed.stdout or completed.stderr:",
      "            message = \"the executable should succeed without console output\"",
      "            raise AssertionError(message)",
      "        workspace = working_directory / \"tmp\"",
      "        figure_path = workspace / \"figure.png\"",
      "        table_path = workspace / \"table.tex\"",
      "        if (",
      "            not figure_path.is_file()",
      "            or figure_path.stat().st_size == 0",
      "            or not table_path.is_file()",
      "        ):",
      "            message = \"workspace artifacts should be complete\"",
      "            raise AssertionError(message)",
      "        table = table_path.read_text(encoding=\"utf-8\")",
      "        expected_fragments = (",
      "            \"\\\\begin{tabular}{lr}\",",
      "            \"Metric & Value \\\\\\\\\",",
      "            \"count & 5.00 \\\\\\\\\",",
      "            \"total & 32.00 \\\\\\\\\",",
      "            \"mean & 6.40 \\\\\\\\\",",
      "            \"min & 2.00 \\\\\\\\\",",
      "            \"max & 12.00 \\\\\\\\\",",
      "            \"\\\\end{tabular}\",",
      "        )",
      "        if not all(fragment in table for fragment in expected_fragments):",
      "            message = \"generated table should contain the expected summary\"",
      "            raise AssertionError(message)",
      "",
      "",
      "if __name__ == \"__main__\":",
      "    main()"
    ]
haskellMainSource :: T.Text
haskellMainSource =
  T.unlines
    [ "{-# LANGUAGE Trustworthy #-}",
      "{-# OPTIONS_GHC -Wno-unsafe #-}",
      "module Main (main, runPackageTests) where",
      "import System.Exit (exitFailure)",
      "import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)",
      "",
      "renderMessage :: String",
      "renderMessage = \"Hello World Haskell\"",
      "",
      "runPackageTests :: IO ()",
      "runPackageTests = runPackageTestsWith hUnitPackageTests",
      "runPackageTestsWith :: Test -> IO ()",
      "runPackageTestsWith packageTests = do",
      "  counts <- runTestTT packageTests",
      "  if errors counts == 0 && failures counts == 0",
      "    then putStrLn \"test ... ok\"",
      "    else exitFailure",
      "",
      "hUnitPackageTests :: Test",
      "hUnitPackageTests =",
      "  TestList",
      "    [ TestLabel \"Renders the sample message.\" $ TestCase $ do",
      "        assertEqual \"sample message\" \"Hello World Haskell\" renderMessage",
      "    ]",
      "",
      "main :: IO ()",
      "main = putStrLn renderMessage"
    ]
rustMainSource :: T.Text
rustMainSource =
  T.unlines
    [ "#![allow(clippy::multiple_crate_versions)]",
      "use anyhow::{Context as _, Result};",
      "use ignore::WalkBuilder;",
      "use std::env;",
      "use std::fs;",
      "use std::path::Path;",
      "fn main() -> Result<()> {",
      "    let mut paths = env::args_os().skip(1);",
      "    if let Some(path) = paths.next() {",
      "        process_path(Path::new(&path))?;",
      "        for path in paths {",
      "            process_path(Path::new(&path))?;",
      "        }",
      "    } else {",
      "        process_path(Path::new(\".\"))?;",
      "    }",
      "    Ok(())",
      "}",
      "fn process_path(path: &Path) -> Result<()> {",
      "    for result in WalkBuilder::new(path).require_git(false).build() {",
      "        let entry = result.with_context(|| format!(\"Failed to walk path: {}\", path.display()))?;",
      "        if entry.file_type().is_some_and(|file_type| file_type.is_file()) {",
      "            process_file(entry.path())?;",
      "        }",
      "    }",
      "    Ok(())",
      "}",
      "fn process_file(path: &Path) -> Result<()> {",
      "    let path_display = path.display();",
      "    let contents = fs::read(path).with_context(|| format!(\"Failed to read file: {path_display}\"))?;",
      "    if content_inspector::inspect(&contents).is_binary() {",
      "        return Ok(());",
      "    }",
      "    let updated_contents = remove_empty_lines(&contents);",
      "    if updated_contents != contents {",
      "        fs::write(path, updated_contents).with_context(|| format!(\"Failed to write file: {path_display}\"))?;",
      "    }",
      "    Ok(())",
      "}",
      "fn remove_empty_lines(contents: &[u8]) -> Vec<u8> {",
      "    let mut updated_contents = Vec::with_capacity(contents.len());",
      "    for line in contents.split_inclusive(|byte| *byte == b'\\n') {",
      "        let without_newline = line.strip_suffix(b\"\\n\").unwrap_or(line);",
      "        let line_contents = without_newline.strip_suffix(b\"\\r\").unwrap_or(without_newline);",
      "        if !core::str::from_utf8(line_contents).is_ok_and(|text| text.trim().is_empty()) {",
      "            updated_contents.extend_from_slice(line);",
      "        }",
      "    }",
      "    updated_contents",
      "}",
      "#[cfg(test)]",
      "mod tests {",
      "    use super::*;",
      "    use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};",
      "    use std::process::Command;",
      "    use tempfile::tempdir;",
      "    #[derive(Clone, Debug)]",
      "    struct LogicalLine(String);",
      "    impl Arbitrary for LogicalLine {",
      "        fn arbitrary(g: &mut Gen) -> Self {",
      "            let line = String::arbitrary(g)",
      "                .chars()",
      "                .filter(|character| *character != '\\n' && *character != '\\r')",
      "                .collect();",
      "            Self(line)",
      "        }",
      "    }",
      "    fn render_lines(lines: &[LogicalLine]) -> Vec<u8> {",
      "        let mut rendered = Vec::new();",
      "        for line in lines {",
      "            rendered.extend_from_slice(line.0.as_bytes());",
      "            rendered.push(b'\\n');",
      "        }",
      "        rendered",
      "    }",
      "    fn expected_non_empty_lines(lines: &[LogicalLine]) -> Vec<u8> {",
      "        let mut rendered = Vec::new();",
      "        for line in lines {",
      "            if !line.0.trim().is_empty() {",
      "                rendered.extend_from_slice(line.0.as_bytes());",
      "                rendered.push(b'\\n');",
      "            }",
      "        }",
      "        rendered",
      "    }",
      "    #[test]",
      "    fn respects_gitignore_and_skips_binary_files() -> Result<()> {",
      "        let directory = tempdir()?;",
      "        let root = directory.path();",
      "        let gitignore_path = root.join(\".gitignore\");",
      "        fs::write(&gitignore_path, \"ignored.txt\\n\")?;",
      "        let ignored_path = root.join(\"ignored.txt\");",
      "        fs::write(&ignored_path, \"should be ignored\\n\\n\")?;",
      "        let binary_path = root.join(\"binary.bin\");",
      "        fs::write(&binary_path, [0, 15, 255, 0, 1, 2, 3])?;",
      "        process_path(root)?;",
      "        let content_ignored = fs::read_to_string(&ignored_path)?;",
      "        assert_eq!(content_ignored, \"should be ignored\\n\\n\");",
      "        let content_binary = fs::read(&binary_path)?;",
      "        assert_eq!(content_binary, vec![0, 15, 255, 0, 1, 2, 3]);",
      "        Ok(())",
      "    }",
      "    #[test]",
      "    fn processes_current_directory_when_no_arguments() -> Result<()> {",
      "        let Some(executable) = env::var_os(\"PACKAGE_E2E_EXECUTABLE\") else {",
      "            return Ok(());",
      "        };",
      "        let directory = tempdir()?;",
      "        let root = directory.path();",
      "        let file_path = root.join(\"test.txt\");",
      "        fs::write(&file_path, \"line1\\n\\nline2\\n   \\nline3\\n\")?;",
      "        let output = Command::new(executable).current_dir(root).output()?;",
      "        assert!(output.status.success());",
      "        assert!(output.stdout.is_empty());",
      "        assert!(output.stderr.is_empty());",
      "        let content = fs::read_to_string(&file_path)?;",
      "        assert_eq!(content, \"line1\\nline2\\nline3\\n\");",
      "        Ok(())",
      "    }",
      "    #[test]",
      "    fn quickcheck_removing_empty_lines_matches_filtered_sequence() {",
      "        #[expect(clippy::needless_pass_by_value)]",
      "        fn property(lines: Vec<LogicalLine>) -> TestResult {",
      "            let input = render_lines(&lines);",
      "            let actual = remove_empty_lines(&input);",
      "            TestResult::from_bool(actual == expected_non_empty_lines(&lines))",
      "        }",
      "        QuickCheck::new()",
      "            .tests(100)",
      "            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);",
      "    }",
      "}"
    ]
htmlIndexSource :: T.Text
htmlIndexSource =
  T.unlines
    [ "<!doctype html>",
      "<html lang=\"en\">",
      "  <head>",
      "    <meta charset=\"utf-8\" />",
      "    <title>Hello World</title>",
      "    <link href=\"style.css\" rel=\"stylesheet\" />",
      "  </head>",
      "  <body>",
      "    <h1>Hello World!</h1>",
      "    <script src=\"script.js\"></script>",
      "  </body>",
      "</html>"
    ]
htmlScriptSource :: T.Text
htmlScriptSource = T.unlines ["console.log(\"Hello World!\");"]
htmlStyleSource :: T.Text
htmlStyleSource =
  T.unlines
    [ "body {",
      "  font-family: sans-serif;",
      "  text-align: center;",
      "  padding-top: 50px;",
      "}"
    ]
cMainSource :: T.Text
cMainSource =
  T.unlines
    [ "#include <stdio.h>",
      "int main(void) {",
      "  puts(\"Hello World\");",
      "  return 0;",
      "}"
    ]
pythonLaTeXMsTexSource :: T.Text
pythonLaTeXMsTexSource =
  T.unlines
    [ "\\documentclass{article}",
      "\\usepackage{booktabs}",
      "\\usepackage{graphicx}",
      "\\usepackage{url}",
      "\\begin{document}",
      "\\section*{Python and \\LaTeX{} template}",
      "This manuscript consumes artifacts generated by Python and compiled in \\texttt{tmp/}. The figure below is rendered as \\texttt{tmp/figure.png}.",
      "\\begin{figure}[ht]",
      "  \\centering",
      "  \\includegraphics[width=0.75\\linewidth]{figure.png}",
      "  \\caption{A deterministic figure generated by \\texttt{main.py}.}",
      "\\end{figure}",
      "The table below is rendered directly by \\texttt{main.py} and stored as \\texttt{tmp/table.tex}.",
      "\\begin{table}[ht]",
      "  \\centering",
      "  \\input{table.tex}",
      "  \\caption{A table imported from a Python-generated \\LaTeX{} fragment.}",
      "\\end{table}",
      "The same module contains its runtime entrypoint and focused \\texttt{python -m pytest} checks, and still cites \\cite{nixos}.",
      "\\bibliographystyle{plain}",
      "\\bibliography{ms}",
      "\\end{document}"
    ]
pythonLaTeXMsBibSource :: T.Text
pythonLaTeXMsBibSource =
  T.unlines
    [ "@misc{nixos,",
      "  author = {NixOS contributors},",
      "  title = {NixOS},",
      "  url = {https://nixos.org/},",
      "}"
    ]
latexMsTexSource :: T.Text
latexMsTexSource =
  T.unlines
    [ "\\documentclass{article}",
      "\\usepackage{url}",
      "\\begin{document}",
      "Hello World! \\cite{nixos}",
      "\\bibliographystyle{plain}",
      "\\bibliography{ms}",
      "\\end{document}"
    ]
latexMsBibSource :: T.Text
latexMsBibSource =
  T.unlines
    [ "@misc{nixos,",
      "  title = {NixOS},",
      "  howpublished = {\\url{https://nixos.org}},",
      "  year = {2026}",
      "}"
    ]
removeEmptyLinesCargoTomlFixture :: T.Text
removeEmptyLinesCargoTomlFixture =
  T.unlines
    [ "[dependencies]",
      "ignore = \"0.4\"",
      "anyhow = \"1.0\"",
      "content_inspector = \"0.2\"",
      "tempfile = \"3.8\"",
      "",
      "",
      "[package]",
      "name = \"remove-empty-lines\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      ""
    ]
rustCargoTomlBaseline :: T.Text
rustCargoTomlBaseline =
  T.unlines
    [ "[dependencies]",
      "colored = \"2.1.0\"",
      "",
      "",
      "[package]",
      "name = \"rust-template\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      ""
    ]
haskellCabalBaseline :: T.Text
haskellCabalBaseline =
  T.unlines
    [ "name:          haskell-template",
      "version:       0.0.0",
      "synopsis:      Canonical Haskell package template",
      "cabal-version: >=1.10",
      "build-type:    Simple",
      "executable haskell-template",
      "  main-is:       Main.hs",
      "  build-depends:",
      "      aeson",
      "    , base",
      "    , bytestring",
      "    , HUnit",
      ""
    ]
haskellTemplateBaselineNixSource :: T.Text
haskellTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  executableHaskellDepends = [",
      "    pkgs.haskellPackages.HUnit",
      "    pkgs.haskellPackages.aeson",
      "    pkgs.haskellPackages.base",
      "    pkgs.haskellPackages.bytestring",
      "  ];",
      "  ghc = pkgs.haskellPackages.ghcWithPackages (_: executableHaskellDepends);",
      "in",
      "pkgs.stdenv.mkDerivation rec {",
      "  buildPhase = ''",
      "    runHook preBuild",
      "    ${ghc}/bin/ghc -O2 -Weverything -Werror -threaded -i. -o \"$pname\" Main.hs",
      "    runHook postBuild",
      "  '';",
      "  installPhase = ''",
      "    runHook preInstall",
      "    install -Dm755 \"$pname\" \"$out/bin/$pname\"",
      "    runHook postInstall",
      "  '';",
      "  nativeBuildInputs = [",
      "    ghc",
      "    pkgs.makeWrapper",
      "  ];",
      "  meta.mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  postInstall = ''",
      "    wrapProgram \"$out/bin/${pname}\" --run \"rm -f tmp/${pname}.tix\" --set-default HPCTIXFILE tmp/${pname}.tix",
      "    ${ghcForTests}/bin/ghc -i. -e 'Main.runPackageTests' Main.hs",
      "  '';",
      "  src = ./.;",
      "  version = \"0.0.0\";",
      "}",
      ""
    ]
rustTemplateBaselineNixSource :: T.Text
rustTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  pname = baseNameOf ./.;",
      "  version = \"0.1.0\";",
      "  cargoTomlFormat = pkgs.formats.toml { };",
      "  cargoToml = cargoTomlFormat.generate \"Cargo.toml\" {",
      "    package = { name = pname; inherit version; edition = \"2021\"; };",
      "    dependencies = { };",
      "  };",
      "in",
      "pkgs.rustPlatform.buildRustPackage {",
      "  cargoLock.lockFile = ./Cargo.lock;",
      "  doInstallCheck = pkgs.stdenv.isLinux;",
      "  env = {",
      "    RUSTDOCFLAGS = \"-D warnings\";",
      "    RUSTFLAGS = \"-D warnings -F unsafe-code\";",
      "  };",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    test -x \"$out/bin/${pname}\"",
      "    workspace=\"$PWD/installcheck\"",
      "    mkdir -p \"$workspace\"",
      "    \"$out/bin/${pname}\" \"$workspace\"",
      "    runHook postInstallCheck",
      "  '';",
      "  meta.mainProgram = pname;",
      "  pname = pname;",
      "  version = version;",
      "  postPatch = ''",
      "    cp ${cargoToml} Cargo.toml",
      "  '';",
      "  src = ./.;",
      "  strictDeps = true;",
      "}",
      ""
    ]
htmlTemplateBaselineNixSource :: T.Text
htmlTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.writeShellApplication rec {",
      "  meta.description = \"An HTML, CSS, and JavaScript template package.\";",
      "  name = baseNameOf ./.;",
      "  runtimeInputs = [",
      "    pkgs.http-server",
      "  ];",
      "  text = ''",
      "    exec ${pkgs.http-server}/bin/http-server ${./.} \"$@\"",
      "  '';",
      "}",
      ""
    ]
cTemplateBaselineNixSource :: T.Text
cTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.stdenv.mkDerivation rec {",
      "  buildPhase = ''",
      "    cc -o ${pname} main.c -std=c89 \\",
      "    -O3 \\",
      "    -Waggregate-return \\",
      "    -Waggressive-loop-optimizations \\",
      "    -Wall \\",
      "    -Walloc-zero \\",
      "    -Walloca \\",
      "    -Warith-conversion \\",
      "    -Warray-bounds=2 \\",
      "    -Wattribute-alias \\",
      "    -Wattributes \\",
      "    -Wbad-function-cast \\",
      "    -Wbidi-chars=any \\",
      "    -Wbuiltin-declaration-mismatch \\",
      "    -Wbuiltin-macro-redefined \\",
      "    -Wc90-c99-compat \\",
      "    -Wc99-c11-compat \\",
      "    -Wcast-align \\",
      "    -Wcast-align=strict \\",
      "    -Wcast-qual \\",
      "    -Wconversion \\",
      "    -Wcoverage-mismatch \\",
      "    -Wcpp \\",
      "    -Wdate-time \\",
      "    -Wdeclaration-after-statement \\",
      "    -Wdeprecated \\",
      "    -Wdeprecated-declarations \\",
      "    -Wdesignated-init \\",
      "    -Wdisabled-optimization \\",
      "    -Wdiscarded-array-qualifiers \\",
      "    -Wdiscarded-qualifiers \\",
      "    -Wdiv-by-zero \\",
      "    -Wdouble-promotion \\",
      "    -Wduplicated-branches \\",
      "    -Wduplicated-cond \\",
      "    -Werror \\",
      "    -Wextra \\",
      "    -Wfloat-equal \\",
      "    -Wformat=2 \\",
      "    -Wformat-overflow=2 \\",
      "    -Wformat-signedness \\",
      "    -Wformat-truncation=2 \\",
      "    -Wfree-nonheap-object \\",
      "    -Whsa \\",
      "    -Wif-not-aligned \\",
      "    -Wignored-attributes \\",
      "    -Wimport \\",
      "    -Wincompatible-pointer-types \\",
      "    -Winline \\",
      "    -Wint-conversion \\",
      "    -Wint-to-pointer-cast \\",
      "    -Winvalid-memory-model \\",
      "    -Winvalid-pch \\",
      "    -Wjump-misses-init \\",
      "    -Wlogical-op \\",
      "    -Wlto-type-mismatch \\",
      "    -Wmissing-declarations \\",
      "    -Wmissing-include-dirs \\",
      "    -Wmissing-prototypes \\",
      "    -Wmultichar \\",
      "    -Wnested-externs \\",
      "    -Wnull-dereference \\",
      "    -Wodr \\",
      "    -Wold-style-definition \\",
      "    -Woverflow \\",
      "    -Woverride-init-side-effects \\",
      "    -Wpacked \\",
      "    -Wpacked-bitfield-compat \\",
      "    -Wpedantic \\",
      "    -Wpointer-compare \\",
      "    -Wpointer-to-int-cast \\",
      "    -Wpragmas \\",
      "    -Wreturn-local-addr \\",
      "    -Wscalar-storage-order \\",
      "    -Wshadow \\",
      "    -Wshift-count-negative \\",
      "    -Wshift-count-overflow \\",
      "    -Wshift-negative-value \\",
      "    -Wsizeof-array-argument \\",
      "    -Wstack-protector \\",
      "    -Wstrict-aliasing \\",
      "    -Wstrict-overflow \\",
      "    -Wstrict-prototypes \\",
      "    -Wstringop-overflow=4 \\",
      "    -Wsuggest-attribute=const \\",
      "    -Wsuggest-attribute=format \\",
      "    -Wsuggest-attribute=malloc \\",
      "    -Wsuggest-attribute=noreturn \\",
      "    -Wsuggest-attribute=pure \\",
      "    -Wsuggest-attribute=returns_nonnull \\",
      "    -Wsuggest-final-methods \\",
      "    -Wsuggest-final-types \\",
      "    -Wswitch-bool \\",
      "    -Wswitch-default \\",
      "    -Wswitch-enum \\",
      "    -Wswitch-unreachable \\",
      "    -Wsync-nand \\",
      "    -Wtraditional-conversion \\",
      "    -Wtrampolines \\",
      "    -Wundef \\",
      "    -Wunreachable-code \\",
      "    -Wunsafe-loop-optimizations \\",
      "    -Wunsuffixed-float-constants \\",
      "    -Wunused-macros \\",
      "    -Wunused-result \\",
      "    -Wvarargs \\",
      "    -Wvector-operation-performance \\",
      "    -Wvla \\",
      "    -Wwrite-strings \\",
      "    -fanalyzer \\",
      "    -fstrict-flex-arrays=3 \\",
      "    -fstack-protector-strong \\",
      "    -fstack-clash-protection \\",
      "    -D_FORTIFY_SOURCE=3 \\",
      "    -Wl,-z,relro,-z,now \\",
      "    -Wl,-z,noexecstack",
      "  '';",
      "  checkPhase = ''",
      "    clang-tidy main.c -- -std=c89 -I${pkgs.stdenv.cc.libc.dev}/include -I${pkgs.lib.getDev pkgs.stdenv.cc.cc}/include",
      "    cppcheck --enable=all --error-exitcode=1 --inconclusive --force --std=c89 --suppress=missingIncludeSystem .",
      "    ./${pname}",
      "  '';",
      "  doCheck = pkgs.stdenv.isLinux;",
      "  installPhase = ''",
      "    install -Dm755 ${pname} \"$out/bin/${pname}\"",
      "  '';",
      "  meta = {",
      "    description = \"A C template package.\";",
      "    mainProgram = pname;",
      "  };",
      "  nativeCheckInputs = [",
      "    pkgs.clang-tools",
      "    pkgs.cppcheck",
      "  ];",
      "  pname = baseNameOf ./.;",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}",
      ""
    ]
latexTemplateBaselineNixSource :: T.Text
latexTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.stdenv.mkDerivation rec {",
      "  buildPhase = ''",
      "    latexmk -pdf ms.tex",
      "  '';",
      "  installPhase = ''",
      "    install -Dm644 ms.pdf \"$out/ms.pdf\"",
      "  '';",
      "  meta.description = \"A LaTeX template package.\";",
      "  nativeBuildInputs = [",
      "    pkgs.texliveFull",
      "  ];",
      "  pname = baseNameOf ./.;",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}",
      ""
    ]
deployHostTemplateBaselineNixSource :: T.Text
deployHostTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.writeShellApplication rec {",
      "  meta.description = \"A Terraform template package for deploying a host.\";",
      "  name = baseNameOf ./.;",
      "  runtimeInputs = [",
      "    pkgs.jq",
      "    pkgs.openssh",
      "    (pkgs.opentofu.withPlugins (p: [",
      "      p.hashicorp_external",
      "      p.hashicorp_local",
      "      p.hashicorp_null",
      "      p.hetznercloud_hcloud",
      "    ]))",
      "  ];",
      "  text = ''",
      "    # shellcheck disable=SC1091",
      "    source ${",
      "      pkgs.lib.getExe (",
      "        inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {",
      "          secrets.secrets.file = ../../secrets/secrets.age;",
      "        }",
      "      )",
      "    }",
      "    # shellcheck disable=SC2086,SC2163,SC2154",
      "    export $secrets",
      "    workdir=$(mktemp -d)",
      "    cp -R ${../..}/. \"$workdir/\"",
      "    chmod -R u+w \"$workdir\"",
      "    rm -rf \"$workdir/packages/${name}/.terraform\" \"$workdir/packages/${name}/.terraform.lock.hcl\"",
      "    tofu -chdir=\"$workdir/packages/${name}\" init -reconfigure",
      "    tofu -chdir=\"$workdir/packages/${name}\" apply",
      "  '';",
      "}",
      ""
    ]
pythonPyPITemplateBaselineNixSource :: T.Text
pythonPyPITemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python3;",
      "in",
      "python.pkgs.buildPythonPackage rec {",
      "  format = \"wheel\";",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = [];",
      "  pythonImportsCheck = [",
      "    pname",
      "  ];",
      "  src = python.pkgs.fetchPypi rec {",
      "    inherit",
      "      format",
      "      pname",
      "      version",
      "      ;",
      "    dist = python;",
      "    python = \"py3\";",
      "    sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\";",
      "  };",
      "  version = \"0.0.0\";",
      "}"
    ]
pythonPyPIApplicationTemplateBaselineNixSource :: T.Text
pythonPyPIApplicationTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python3;",
      "in",
      "python.pkgs.buildPythonApplication rec {",
      "  format = \"wheel\";",
      "  meta.mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = [];",
      "  pythonImportsCheck = [",
      "    pname",
      "  ];",
      "  src = python.pkgs.fetchPypi rec {",
      "    inherit",
      "      format",
      "      pname",
      "      version",
      "      ;",
      "    dist = python;",
      "    python = \"py3\";",
      "    sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\";",
      "  };",
      "  version = \"0.0.0\";",
      "}"
    ]
binaryReleaseTemplateBaselineNixSource :: T.Text
binaryReleaseTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.stdenv.mkDerivation rec {",
      "  doInstallCheck = pkgs.stdenv.isLinux;",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    test -x \"$out/bin/${pname}\"",
      "    set -o pipefail",
      "    \"$out/bin/${pname}\" --help 2>&1 | grep -F \"${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    runHook preInstall",
      "    install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname} \"$out/bin/${pname}\"",
      "    runHook postInstall",
      "  '';",
      "  meta.mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  sourceRoot = \".\";",
      "  src = pkgs.fetchurl {",
      "    sha256 = \"4yOzM6f8Rdsw2YxsqSpIhHCNuRZRf8j3AAcK2T5VZlU=\";",
      "    url = \"https://github.com/asamarts/${pname}/releases/download/v${version}/${pname}-v${version}-x86_64-unknown-linux-musl.tar.gz\";",
      "  };",
      "  strictDeps = true;",
      "  version = \"0.9.23\";",
      "}"
    ]
pythonLaTeXTemplateBaselineNixSource :: T.Text
pythonLaTeXTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python3;",
      "  pythonDeps = [",
      "    python.pkgs.matplotlib",
      "  ];",
      "  pythonEnv = python.withPackages (_: pythonDeps);",
      "in",
      "python.pkgs.buildPythonPackage rec {",
      "  buildPhase = ''",
      "    mkdir -p tmp",
      "    ${pythonEnv}/bin/python3 main.py",
      "    cp ms.{tex,bib} tmp/",
      "    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex",
      "  '';",
      "  installPhase = ''",
      "    datadir=\"$out/share/${pname}\"",
      "    install -Dm755 main.py \"$out/bin/${pname}\"",
      "    install -Dm644 main.py ms.tex ms.bib -t \"$datadir\"",
      "    install -Dm644 tmp/ms.pdf \"$out/ms.pdf\"",
      "    install -Dm644 main.py \"$out/${python.sitePackages}/${pname}.py\"",
      "  '';",
      "  meta = {",
      "    description = \"A Python and LaTeX template package.\";",
      "    mainProgram = pname;",
      "  };",
      "  nativeBuildInputs = [ pkgs.texliveFull ];",
      "  passthru.python = python;",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = pythonDeps;",
      "  pyproject = false;",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}"
    ]
haskellCoverageCheckBaselineNixSource :: T.Text
haskellCoverageCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = baseNameOf ./.;",
      "  packageDrv = import (../.. + \"/packages/${packageName}/default.nix\") {",
      "    inherit pkgs;",
      "  };",
      "  packageName = pkgs.lib.removeSuffix \"-coverage\" checkName;",
      "  testGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);",
      "in",
      "pkgs.runCommand checkName",
      "  {",
      "    nativeBuildInputs = [",
      "      packageDrv",
      "      pkgs.git",
      "      testGhc",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    packageName=\"${packageName}\"",
      "    mkdir -p \"$out/html\" \"$workspace/coverage\" \"$workspace/hpc\"",
      "    cd \"$workspace\"",
      "    cat > \"$workspace/TestMain.hs\" <<EOF",
      "    module TestMain (main) where",
      "    import qualified Main as PackageMain",
      "    main :: IO ()",
      "    main = PackageMain.runPackageTests",
      "    EOF",
      "    \"${testGhc}/bin/ghc\" \\",
      "      -fhpc \\",
      "      -hpcdir \"$workspace/hpc\" \\",
      "      -O2 \\",
      "      -main-is TestMain.main \\",
      "      -i\"$src\" \\",
      "      -outputdir \"$workspace\" \\",
      "      -odir \"$workspace\" \\",
      "      -hidir \"$workspace\" \\",
      "      -o \"$workspace/$packageName\" \\",
      "      \"$workspace/TestMain.hs\" \\",
      "      \"$src/Main.hs\"",
      "    PACKAGE_E2E_EXECUTABLE=\"${packageDrv}/bin/${packageName}\" HPCTIXFILE=\"$workspace/coverage/$packageName.tix\" \\",
      "      \"$workspace/$packageName\"",
      "    hpc markup \"$workspace/coverage/${packageName}.tix\" --hpcdir=\"$workspace/hpc\" --destdir=\"$out/html\"",
      "  ''"
    ]
pythonCoverageCheckBaselineNixSource :: T.Text
pythonCoverageCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"_coverage\" checkName;",
      "  pythonEnv = packageDrv.python.withPackages (",
      "    _:",
      "    packageDrv.propagatedBuildInputs",
      "    ++ [",
      "      packageDrv.python.pkgs.pytest",
      "      packageDrv.python.pkgs.pytest-cov",
      "    ]",
      "  );",
      "in",
      "pkgs.runCommand checkName",
      "  {",
      "    nativeBuildInputs = [",
      "      pythonEnv",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$(mktemp -d)\"",
      "    mkdir -p \"$out/html\"",
      "    PACKAGE_E2E_EXECUTABLE=\"${packageDrv}/bin/${packageName}\" python -m pytest -p no:cacheprovider --cov=\"$src\" --cov-report \"html:$out/html\" \"$src/main.py\"",
      "  ''"
    ]
rustCoverageCheckBaselineNixSource :: T.Text
rustCoverageCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  inherit (packageDrv) cargoDeps;",
      "  checkName = baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"-coverage\" checkName;",
      "in",
      "pkgs.runCommand checkName",
      "  {",
      "    nativeBuildInputs = packageDrv.passthru.rustCheckNativeBuildInputs ++ [",
      "      pkgs.cargo-llvm-cov",
      "      pkgs.cargo-nextest",
      "      pkgs.llvmPackages.llvm",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm \"llvm-cov\"}'",
      "    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm \"llvm-profdata\"}'",
      "    workspace=\"$PWD/workspace\"",
      "    cp -R --no-preserve=mode \"$src\" \"$workspace\"",
      "    cp \"${packageDrv.passthru.cargoToml}\" \"$workspace/Cargo.toml\"",
      "    ln -s \"${cargoDeps}\" \"$workspace/cargo-vendor-dir\"",
      "    install -Dm644 \"${cargoDeps}/.cargo/config.toml\" \"$workspace/.cargo/config.toml\"",
      "    substituteInPlace \"$workspace/.cargo/config.toml\" \\",
      "      --replace-warn \"@vendor@\" \"${cargoDeps}\"",
      "    mkdir -p \"$out\"",
      "    export PACKAGE_E2E_EXECUTABLE=\"${packageDrv}/bin/${packageName}\"",
      "    cd \"$workspace\"",
      "    eval \"$(cargo llvm-cov show-env --sh)\"",
      "    cargo llvm-cov clean --profraw-only",
      "    cargo nextest run",
      "  ''"
    ]
cTemplateCheckBaselineNixSource :: T.Text
cTemplateCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  name = baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${name};",
      "in",
      "pkgs.testers.runNixOSTest {",
      "  inherit name;",
      "  nodes.machine = _: {",
      "    environment.systemPackages = [",
      "      packageDrv",
      "    ]",
      "    ++ (packageDrv.runtimeInputs or [ ]);",
      "  };",
      "  testScript = ''",
      "    machine.succeed(\"${name}\")",
      "  '';",
      "}"
    ]
hostDefaultCheckBaselineNixSource :: T.Text
hostDefaultCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "pkgs.runCommand \"host_default\"",
      "  {",
      "    nativeBuildInputs = [",
      "      inputs.self.nixosConfigurations.default.config.system.build.vm",
      "    ];",
      "  }",
      "  ''",
      "    touch \"$out\"",
      "  ''"
    ]
defaultVmWithDiskoCheckBaselineNixSource :: T.Text
defaultVmWithDiskoCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  host = pkgs.lib.removeSuffix \"VmWithDisko\" (baseNameOf ./.);",
      "in",
      "pkgs.runCommand (baseNameOf ./.)",
      "  {",
      "    buildInputs = [",
      "      inputs.self.nixosConfigurations.${host}.config.system.build.vmWithDisko",
      "    ];",
      "  }",
      "  ''",
      "    touch \"$out\"",
      "  ''"
    ]
uncommentTemplateBaselineNixSource :: T.Text
uncommentTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "pkgs.stdenv.mkDerivation rec {",
      "  doInstallCheck = pkgs.stdenv.isLinux;",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    test -x \"$out/bin/${pname}\"",
      "    set -o pipefail",
      "    \"$out/bin/${pname}\" --help 2>&1 | grep -F \"${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    runHook preInstall",
      "    install -Dm755 ${pname} \"$out/bin/${pname}\"",
      "    runHook postInstall",
      "  '';",
      "  meta = {",
      "    description = \"A fast Rust-based CLI tool for removing comments from source code.\";",
      "    mainProgram = pname;",
      "  };",
      "  nativeBuildInputs = [",
      "    pkgs.autoPatchelfHook",
      "  ];",
      "  pname = baseNameOf ./.;",
      "  sourceRoot = \".\";",
      "  src = pkgs.fetchurl {",
      "    sha256 = \"6jUmVZ5SIKRgaF6V6gy2aFu4ZgcKbhl1O7g16UcnIQQ=\";",
      "    url = \"https://github.com/Goldziher/${pname}/releases/download/v${version}/${pname}-x86_64-unknown-linux-gnu.tar.gz\";",
      "  };",
      "  strictDeps = true;",
      "  version = \"3.0.2\";",
      "}"
    ]
