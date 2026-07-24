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
module Main (main, runPackageTests, runPackageTestsWithTimings) where
import Control.Applicative ((<|>))
import Control.Exception (IOException, finally, try)
import Control.Monad (filterM, forM, forM_, when)
import Data.Char (isAlphaNum, isAsciiLower, isDigit, isLower, isSpace, isUpper, toLower, toUpper)
import Data.Either (partitionEithers)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, mapAccumL, maximumBy, partition, sort, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import Nix.Expr.Types
  ( Antiquoted (Plain),
    Binding (Inherit, NamedVar),
    NExprF (NAbs, NLet, NSet),
    NKeyName (DynamicKey, StaticKey),
    NString (DoubleQuoted),
    Params (Param, ParamSet),
    VarName (VarName),
  )
import Nix.Expr.Types.Annotated (AnnUnit (AnnUnit), NExprLoc, stripAnnotation)
import Nix.Parser (parseNixFileLoc)
import Nix.Pretty (prettyNix)
import Nix.Utils (Path (Path))
import Numeric (showFFloat)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly, withCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath ((<.>), (</>))
import System.FilePath.Posix (makeRelative, splitDirectories, takeBaseName, takeDirectory, takeFileName)
import System.IO (hClose, hPutStr, hPutStrLn, openTempFile, stderr)
import System.Posix.Process (executeFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertBool, assertEqual, assertFailure, runTestTT)
import Text.Read (readMaybe)
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
      "postInstall",
      "meta",
      "meta.description",
      "propagatedBuildInputs",
      "runtimeInputs",
      "version"
    ]
type TemplateSpec :: Type
data TemplateSpec = TemplateSpec
  { templateName :: FilePath,
    templateMatches :: FilePath -> String -> IO Bool,
    templateAllowedDifferenceKeys :: Set.Set T.Text,
    templateBaselineSource :: T.Text
  }
type CheckTemplateSpec :: Type
data CheckTemplateSpec = CheckTemplateSpec
  { checkTemplateName :: FilePath,
    checkTemplateMatches :: FilePath -> String -> IO Bool,
    checkTemplateBaselineSource :: T.Text,
    checkTemplateComparisonMode :: CheckTemplateComparisonMode
  }
type CheckTemplateComparisonMode :: Type
data CheckTemplateComparisonMode
  = ExactCheckTemplate
  | StructuralCPackageVm
templateSpecs :: [TemplateSpec]
templateSpecs =
  [ TemplateSpec
      { templateName = "haskell_package_baseline",
        templateMatches = \_ nixSource -> pure ("haskellPackages.mkDerivation" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = haskellTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        templateMatches = \_ nixSource -> pure ("rustPlatform.buildRustPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = rustTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "html_template",
        templateMatches = \_ nixSource -> pure ("writeShellApplication" `isInfixOf` nixSource && "http-server" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "text" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = htmlTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_latex_template",
        templateMatches = matchesPythonLatexTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["meta", "propagatedBuildInputs", "pythonDeps", "version"],
        templateBaselineSource = pythonLatexTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_application_template",
        templateMatches = matchesPythonPyPIApplicationTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        templateBaselineSource = pythonPyPIApplicationTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_template",
        templateMatches = matchesPythonPyPITemplate,
        templateAllowedDifferenceKeys = Set.fromList ["format", "installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        templateBaselineSource = pythonPyPITemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "binary_release_template",
        templateMatches = matchesBinaryReleaseTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "src", "version"],
        templateBaselineSource = binaryReleaseTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_template",
        templateMatches = \_ nixSource -> pure ("buildPythonPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"],
        templateBaselineSource = pythonTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "deploy_host_template",
        templateMatches = \_ nixSource ->
          pure
            ( "writeShellApplication" `isInfixOf` nixSource
                && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource)
            ),
        templateAllowedDifferenceKeys = Set.insert "meta.description" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = deployHostTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "latex_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "latexmk -pdf ms.tex" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
        templateBaselineSource = latexTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "c_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "cc -o ${pname} main.c -std=c89" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]),
        templateBaselineSource = cTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "uncomment_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "autoPatchelfHook" `isInfixOf` nixSource
                && "Goldziher" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["pname", "src"]),
        templateBaselineSource = uncommentTemplateBaselineNixSource
      }
  ]
matchesPythonLatexTemplate :: FilePath -> String -> IO Bool
matchesPythonLatexTemplate packageName nixSource
  | "buildPythonPackage" `isInfixOf` nixSource = do
      let packageDirectory = "packages" </> packageName
      hasManuscriptTexFile <- doesFileExist (packageDirectory </> "ms.tex")
      hasRefsBibFile <- doesFileExist (packageDirectory </> "refs.bib")
      hasFiguresDirectory <- doesDirectoryExist (packageDirectory </> "figures")
      pure (hasManuscriptTexFile || hasRefsBibFile || hasFiguresDirectory)
  | otherwise = pure False
matchesPythonPyPITemplateLike :: String -> FilePath -> String -> IO Bool
matchesPythonPyPITemplateLike buildFunction _ nixSource =
  pure
    ( buildFunction `isInfixOf` nixSource
        && not ("src = ./.;" `isInfixOf` nixSource)
        && ("fetchPypi" `isInfixOf` nixSource || "fetchurl" `isInfixOf` nixSource)
    )
matchesPythonPyPIApplicationTemplate :: FilePath -> String -> IO Bool
matchesPythonPyPIApplicationTemplate = matchesPythonPyPITemplateLike "buildPythonApplication"
matchesPythonPyPITemplate :: FilePath -> String -> IO Bool
matchesPythonPyPITemplate = matchesPythonPyPITemplateLike "buildPythonPackage"
matchesBinaryReleaseTemplate :: FilePath -> String -> IO Bool
matchesBinaryReleaseTemplate _ nixSource =
  pure
    ( "stdenv.mkDerivation" `isInfixOf` nixSource
        && "src = pkgs.fetchurl" `isInfixOf` nixSource
        && "sourceRoot = \".\";" `isInfixOf` nixSource
        && "install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname}" `isInfixOf` nixSource
    )
matchesCheckNameSuffixAndSourceContains :: String -> [String] -> FilePath -> String -> IO Bool
matchesCheckNameSuffixAndSourceContains suffix requiredNeedles checkName nixSource =
  pure
    ( suffix `isSuffixOf` checkName
        && all (`isInfixOf` nixSource) requiredNeedles
    )
checkTemplateSpecs :: [CheckTemplateSpec]
checkTemplateSpecs =
  [ CheckTemplateSpec
      { checkTemplateName = "haskell_coverage_check",
        checkTemplateMatches = matchesHaskellCoverageCheck,
        checkTemplateBaselineSource = haskellCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "haskell_profile_check",
        checkTemplateMatches = matchesHaskellProfileCheck,
        checkTemplateBaselineSource = haskellProfileCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "python_coverage_check",
        checkTemplateMatches = matchesPythonCoverageCheck,
        checkTemplateBaselineSource = pythonCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_coverage_check",
        checkTemplateMatches = matchesRustCoverageCheck,
        checkTemplateBaselineSource = rustCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_profile_check",
        checkTemplateMatches = matchesRustProfileCheck,
        checkTemplateBaselineSource = rustProfileCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "c_package_vm_check",
        checkTemplateMatches = matchesCPackageVmCheck,
        checkTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkTemplateComparisonMode = StructuralCPackageVm
      },
    CheckTemplateSpec
      { checkTemplateName = "default_vm_with_disko_check",
        checkTemplateMatches = matchesDefaultVmWithDiskoCheck,
        checkTemplateBaselineSource = defaultVmWithDiskoCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "host_default_check",
        checkTemplateMatches = \checkName _ -> pure (checkName == "host_default"),
        checkTemplateBaselineSource = hostDefaultCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      }
  ]
matchesHaskellCoverageCheck :: FilePath -> String -> IO Bool
matchesHaskellCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["ghcWithPackages", "-fhpc"]
matchesHaskellProfileCheck :: FilePath -> String -> IO Bool
matchesHaskellProfileCheck = matchesCheckNameSuffixAndSourceContains "-profile" ["ghcWithPackages", "-fprof-auto"]
matchesPythonCoverageCheck :: FilePath -> String -> IO Bool
matchesPythonCoverageCheck = matchesCheckNameSuffixAndSourceContains "_coverage" ["--cov=\"$src\""]
matchesRustCoverageCheck :: FilePath -> String -> IO Bool
matchesRustCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["cargo llvm-cov"]
matchesRustProfileCheck :: FilePath -> String -> IO Bool
matchesRustProfileCheck = matchesCheckNameSuffixAndSourceContains "-profile" ["pkgs.perf", "perf record"]
matchesCPackageVmCheck :: FilePath -> String -> IO Bool
matchesCPackageVmCheck checkName nixSource = do
  packageKind <- detectPackageKindForPackage checkName
  pure (isCPackageVmCheckShape packageKind nixSource)
matchesDefaultVmWithDiskoCheck :: FilePath -> String -> IO Bool
matchesDefaultVmWithDiskoCheck = matchesCheckNameSuffixAndSourceContains "VmWithDisko" ["pkgs.runCommand", "config.system.build.vmWithDisko"]
isCPackageVmCheckShape :: PackageKind -> String -> Bool
isCPackageVmCheckShape packageKind nixSource =
  packageKind == CPackage
    && "pkgs.testers.runNixOSTest" `isInfixOf` nixSource
    && "nodes.machine" `isInfixOf` nixSource
    && "testScript = ''" `isInfixOf` nixSource
type RepositoryCheckKind :: Type
data RepositoryCheckKind
  = RepositoryCoverageCheck
  | RepositoryProfileCheck
  deriving stock (Eq, Ord, Show)
type CanonicalizationSettings :: Type
newtype CanonicalizationSettings = CanonicalizationSettings
  { canonicalizationPythonPackageAttribute :: String
  }
defaultCanonicalizationSettings :: CanonicalizationSettings
defaultCanonicalizationSettings =
  CanonicalizationSettings
    { canonicalizationPythonPackageAttribute = defaultPythonPackageAttribute
    }
type Command :: Type
data Command
  = CheckCommand
  | SummaryCommand Bool
  | AddCommand String FilePath (Maybe String) (Set.Set RepositoryCheckKind)
  | AddChecksCommand FilePath (Set.Set RepositoryCheckKind)
type CommandParseResult :: Type
data CommandParseResult
  = ParsedCommand Command
  | MainHelp
  | InvalidCommand ExitCode (Maybe String)
main :: IO ()
main = getArgs >>= runCli defaultCanonicalizationSettings
runCli :: CanonicalizationSettings -> [String] -> IO ()
runCli canonicalizationSettings commandLineArgs =
  case parseCommand commandLineArgs of
    MainHelp -> printMainHelpAndExit
    InvalidCommand exitCode maybeCommand ->
      hPutStr stderr (usageTextForCommand maybeCommand) >> exitWith exitCode
    ParsedCommand CheckCommand -> checkRepositoryLocation canonicalizationSettings "."
    ParsedCommand (SummaryCommand jsonOutput) ->
      summarizeRepositoryLocation
        canonicalizationSettings
        (if jsonOutput then renderRepositorySummariesJson else renderRepositorySummariesText)
        "."
    ParsedCommand (AddCommand packageKindName packageName packageDescription requestedCheckKinds) ->
      runInGitRepositoryRoot "." $
        case parseSupportedAddPackageKind packageKindName of
          Nothing -> do
            hPutStrLn stderr ("error: unsupported package type: " ++ packageKindName)
            hPutStrLn stderr ("hint: supported package types: " ++ intercalate ", " (map fst supportedAddPackageKinds))
            exitFailure
          Just scaffoldPackageKind -> do
            addResult <- addPackageToCurrentRepositoryWith canonicalizationSettings scaffoldPackageKind packageName packageDescription requestedCheckKinds
            stageGeneratedPathsOrExit addResult
    ParsedCommand (AddChecksCommand packageName requestedCheckKinds) ->
      runInGitRepositoryRoot "." $ do
        addResult <- addChecksToExistingPackage Nothing packageName Nothing requestedCheckKinds
        stageGeneratedPathsOrExit addResult
stageGeneratedPathsOrExit :: Either String [FilePath] -> IO a
stageGeneratedPathsOrExit = \case
  Left addError -> do
    hPutStrLn stderr ("error: " ++ addError)
    exitFailure
  Right generatedPaths -> delegateToGit (["add", "--"] ++ generatedPaths)
parseCommand :: [String] -> CommandParseResult
parseCommand commandLineArgs =
  case commandLineArgs of
    [] -> InvalidCommand (ExitFailure 1) Nothing
    [argument] | argument `elem` ["-h", "--help"] -> MainHelp
    [_, argument] | argument `elem` ["-h", "--help"] -> InvalidCommand (ExitFailure 1) Nothing
    ["check"] -> ParsedCommand CheckCommand
    "check" : _ -> InvalidCommand usageExitCode (Just "check")
    ["summary"] -> ParsedCommand (SummaryCommand False)
    ["summary", "--json"] -> ParsedCommand (SummaryCommand True)
    _ ->
      case parseAddChecksArgs commandLineArgs of
        Just (packageName, requestedCheckKinds) ->
          ParsedCommand (AddChecksCommand packageName requestedCheckKinds)
        Nothing ->
          case parseAddPackageArgs commandLineArgs of
            Just (packageKindName, packageName, packageDescription, requestedCheckKinds) ->
              ParsedCommand (AddCommand packageKindName packageName packageDescription requestedCheckKinds)
            Nothing -> InvalidCommand usageExitCode (listToMaybe commandLineArgs)
printMainHelpAndExit :: IO a
printMainHelpAndExit = do
  putStr mainHelpText
  exitSuccess
usageExitCode :: ExitCode
usageExitCode = ExitFailure 129
mainUsageText :: String
mainUsageText =
  unlines
    [ "usage: git repository-canonicalization add [<options>] <package-type> <package-name> [<description>...]",
      "   or: git repository-canonicalization add [<options>] <existing-package-name>",
      "   or: git repository-canonicalization check",
      "   or: git repository-canonicalization summary [--json]"
    ]
mainHelpText :: String
mainHelpText =
  unlines
    [ "GIT-REPOSITORY-CANONICALIZATION(1)",
      "",
      "NAME",
      "    git-repository-canonicalization - Check, summarize, and scaffold canonical repositories",
      "",
      "SYNOPSIS",
      "    git repository-canonicalization add [<options>] <package-type> <package-name> [<description>...]",
      "    git repository-canonicalization add [<options>] <existing-package-name>",
      "    git repository-canonicalization check",
      "    git repository-canonicalization summary [--json]",
      "",
      "DESCRIPTION",
      "    Checks and summarizes the nearest Git repository and scaffolds its",
      "    packages and checks. Use 'git -C <location>' to select a repository.",
      "",
      "COMMANDS",
      "    add [<options>] <package-type> <package-name> [<description>...]",
      "    add [<options>] <existing-package-name>",
      "        Add a package and its requested checks, or infer the type of an",
      "        existing package and add requested checks to it. Generated files",
      "        are staged with git add.",
      "",
      "        --coverage            add a coverage check",
      "        --profile             add a profiling check",
      "",
      "    check",
      "        Check that the repository follows the canonical package and check",
      "        conventions.",
      "",
      "    summary [--json]",
      "        Show a summary of the repository's packages and checks.",
      "",
      "        With --json, output the repository summary as JSON.",
      ""
    ]
usageTextForCommand :: Maybe String -> String
usageTextForCommand = \case
  Just "add" ->
    unlines
      [ "usage: git repository-canonicalization add [<options>] <package-type> <package-name> [<description>...]",
        "   or: git repository-canonicalization add [<options>] <existing-package-name>",
        "",
        "Add a package and its requested checks. For an existing package, omit",
        "the package type and description; its type is inferred.",
        "Generated files are staged with git add.",
        "",
        "    --coverage            add a coverage check",
        "    --profile             add a profiling check",
        ""
      ]
  Just "summary" ->
    unlines
      [ "usage: git repository-canonicalization summary [--json]",
        "",
        "Summarize the nearest Git repository. Use 'git -C <location>' to select it.",
        "",
        "    --json                output the repository summary as JSON",
        ""
      ]
  Just "check" ->
    unlines
      [ "usage: git repository-canonicalization check",
        "",
        "Check the nearest Git repository. Use 'git -C <location>' to select it.",
        ""
      ]
  _ -> mainUsageText
parseAddChecksArgs :: [String] -> Maybe (FilePath, Set.Set RepositoryCheckKind)
parseAddChecksArgs ("add" : packageName : flagArguments)
  | all ("--" `isPrefixOf`) flagArguments =
      (packageName,) <$> parseRepositoryCheckFlags flagArguments
parseAddChecksArgs _ = Nothing
parseAddPackageArgs :: [String] -> Maybe (String, FilePath, Maybe String, Set.Set RepositoryCheckKind)
parseAddPackageArgs ("add" : packageKindName : packageName : remainingArguments) = do
  let (flagArguments, packageDescriptionArguments) =
        partition ("--" `isPrefixOf`) remainingArguments
      packageDescription = unwords . NE.toList <$> NE.nonEmpty packageDescriptionArguments
  requestedCheckKinds <- parseRepositoryCheckFlags flagArguments
  pure (packageKindName, packageName, packageDescription, requestedCheckKinds)
parseAddPackageArgs _ = Nothing
parseRepositoryCheckFlags :: [String] -> Maybe (Set.Set RepositoryCheckKind)
parseRepositoryCheckFlags flagArguments =
  Set.fromList
    <$> mapM
      ( \repositoryCheckFlag ->
          lookup
            repositoryCheckFlag
            [ ("--coverage", RepositoryCoverageCheck),
              ("--profile", RepositoryProfileCheck)
            ]
      )
      flagArguments
runInGitRepositoryRoot :: FilePath -> IO a -> IO a
runInGitRepositoryRoot repositoryDirectory action = do
  canonicalRepositoryRoot <- discoverGitRepositoryRoot repositoryDirectory
  withCurrentDirectory canonicalRepositoryRoot action
discoverGitRepositoryRoot :: FilePath -> IO FilePath
discoverGitRepositoryRoot repositoryDirectory = do
  repositoryRootStdout <-
    captureGitOrExit ["-C", repositoryDirectory, "rev-parse", "--path-format=absolute", "--show-toplevel"]
  pure (T.unpack (T.strip (T.pack repositoryRootStdout)))
captureGitOrExit :: [String] -> IO String
captureGitOrExit gitArguments = do
  (gitExit, gitStdout, gitStderr) <- readProcessWithExitCode "git" gitArguments ""
  if gitExit == ExitSuccess
    then pure gitStdout
    else do
      putStr gitStdout
      hPutStr stderr gitStderr
      exitWith gitExit
delegateToGit :: [String] -> IO a
delegateToGit gitArguments = executeFile "git" True gitArguments Nothing
checkRepositoryLocation :: CanonicalizationSettings -> FilePath -> IO ()
checkRepositoryLocation canonicalizationSettings location = do
  repositoryRoot <- discoverGitRepositoryRoot location
  withCurrentDirectory repositoryRoot $
    collectRepositoryComplianceWith canonicalizationSettings >>= \case
      Left repositoryComplianceFailure -> do
        reportCheckRepositoryFailure repositoryComplianceFailure
        exitFailure
      Right _ -> pure ()
requiredRepositoryRootFiles :: [FilePath]
requiredRepositoryRootFiles = ["flake.nix", "flake.lock"]
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
collectRepositoryComplianceWith :: CanonicalizationSettings -> IO (Either RepositoryComplianceFailure RepositoryComplianceSuccess)
collectRepositoryComplianceWith canonicalizationSettings = do
  requiredRootFileIssues <- checkRequiredRepositoryRootFiles
  case requiredRootFileIssues of
    [] -> collectRepositoryContentComplianceWith canonicalizationSettings
    firstIssue : remainingIssues ->
      pure (Left (RepositoryComplianceFailure RequiredRootFilesPhase (firstIssue :| remainingIssues)))
collectRepositoryContentComplianceWith :: CanonicalizationSettings -> IO (Either RepositoryComplianceFailure RepositoryComplianceSuccess)
collectRepositoryContentComplianceWith canonicalizationSettings = do
  repositoryStructureIssues <- checkRepositoryStructure
  case repositoryStructureIssues of
    [] -> do
      packageNames <- listSubdirectoryNames "packages"
      packageComplianceIssues <- concat <$> forM packageNames (checkPackageWith canonicalizationSettings)
      checkNames <- listSubdirectoryNames "checks"
      checkComplianceIssues <- concat <$> forM checkNames checkTemplateWith
      case packageComplianceIssues ++ checkComplianceIssues of
        [] ->
          pure
            ( Right
                RepositoryComplianceSuccess
                  { repositoryCompliancePackageNames = packageNames,
                    repositoryComplianceCheckNames = checkNames
                  }
            )
        firstIssue : remainingIssues ->
          pure (Left (RepositoryComplianceFailure FileCompliancePhase (firstIssue :| remainingIssues)))
    firstIssue : remainingIssues ->
      pure (Left (RepositoryComplianceFailure DirectoryStructurePhase (firstIssue :| remainingIssues)))
reportCheckRepositoryFailure :: RepositoryComplianceFailure -> IO ()
reportCheckRepositoryFailure (RepositoryComplianceFailure checkPhase checkPhaseIssues) = do
  let checkPhaseName = renderRepositoryCheckPhase checkPhase
  hPutStrLn stderr ("error: git repository-canonicalization check failed at phase: " ++ checkPhaseName)
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
type RepositoryPackageChecksSummary :: Type
data RepositoryPackageChecksSummary = RepositoryPackageChecksSummary
  { repositoryPackageCoverage :: Maybe RepositoryPackageCoverageSummary,
    repositoryPackageProfile :: Maybe RepositoryPackageProfileSummary
  }
  deriving stock (Eq, Show)
type RepositoryPackageCoverageSummary :: Type
data RepositoryPackageCoverageSummary
  = RepositoryCoverageNotRun
  | RepositoryCoverageUnavailable (Map.Map String Duration)
  | RepositoryCoverageMeasured CoverageMeasurement (Map.Map String Duration)
  deriving stock (Eq, Show)
type CoverageMeasurement :: Type
data CoverageMeasurement = CoverageMeasurement CoverageMetric Integer Integer
  deriving stock (Eq, Show)
type CoverageMetric :: Type
data CoverageMetric
  = ExpressionCoverage
  | StatementCoverage
  | LineCoverage
  deriving stock (Eq, Show)
type Duration :: Type
newtype Duration = Duration Double
  deriving stock (Eq, Ord, Show)
type RepositoryPackageProfileSummary :: Type
data RepositoryPackageProfileSummary
  = RepositoryProfileNotRun
  | RepositoryProfileUnavailable
  | RepositoryProfileMeasured Duration (Map.Map String Duration)
  deriving stock (Eq, Show)
type RepositoryComplianceSuccess :: Type
data RepositoryComplianceSuccess = RepositoryComplianceSuccess
  { repositoryCompliancePackageNames :: [FilePath],
    repositoryComplianceCheckNames :: [FilePath]
  }
  deriving stock (Eq, Show)
type RepositoryPackageSummary :: Type
data RepositoryPackageSummary = RepositoryPackageSummary
  { repositoryPackageName :: FilePath,
    repositoryPackageKind :: PackageKind,
    repositoryPackageDescription :: Maybe String,
    repositoryPackageTestNames :: [String],
    repositoryPackageChecks :: RepositoryPackageChecksSummary
  }
  deriving stock (Eq, Show)
type RepositorySummary :: Type
data RepositorySummary = RepositorySummary
  { repositorySummaryPath :: FilePath,
    repositorySummaryPackages :: [RepositoryPackageSummary]
  }
  deriving stock (Eq, Show)
summarizeRepositoryLocation :: CanonicalizationSettings -> ([RepositorySummary] -> String) -> FilePath -> IO ()
summarizeRepositoryLocation canonicalizationSettings render location = do
  repositoryRoot <- discoverGitRepositoryRoot location
  repositoryPath <- canonicalizePath repositoryRoot
  repositorySummary <- summarizeRepositoryAt canonicalizationSettings repositoryPath repositoryRoot
  putStr (render [repositorySummary])
summarizeRepositoryAt :: CanonicalizationSettings -> FilePath -> FilePath -> IO RepositorySummary
summarizeRepositoryAt canonicalizationSettings repositoryPath repositoryRoot =
  withCurrentDirectory repositoryRoot $
    collectRepositoryComplianceWith canonicalizationSettings >>= \case
      Left repositoryComplianceFailure -> do
        hPutStrLn stderr ("error: repository: " ++ repositoryPath)
        reportCheckRepositoryFailure repositoryComplianceFailure
        exitFailure
      Right (RepositoryComplianceSuccess packageNames checkNames) -> do
        let repositoryCheckNames = Set.fromList checkNames
            resultCheckNames = filter (\checkName -> any (`isSuffixOf` checkName) ["-coverage", "_coverage", "-profile", "_profile"]) checkNames
        checkOutputPaths <- resolveRepositoryCheckOutputPaths resultCheckNames
        packageSummaries <- forM packageNames (summarizeRepositoryPackage checkOutputPaths repositoryCheckNames)
        pure
          RepositorySummary
            { repositorySummaryPath = repositoryPath,
              repositorySummaryPackages = packageSummaries
            }
renderRepositorySummariesText :: [RepositorySummary] -> String
renderRepositorySummariesText repositorySummaries =
  intercalate
    "\n"
    [ "repository: "
        ++ repositorySummaryPath repositorySummary
        ++ "\n"
        ++ renderRepositoryPackageSummariesText (repositorySummaryPackages repositorySummary)
    | repositorySummary <- repositorySummaries
    ]
renderRepositorySummariesJson :: [RepositorySummary] -> String
renderRepositorySummariesJson repositorySummaries =
  unlines
    [ "{",
      "  \"repositories\": [",
      intercalate ",\n" (map renderRepositorySummaryJson repositorySummaries),
      "  ]",
      "}"
    ]
renderRepositorySummaryJson :: RepositorySummary -> String
renderRepositorySummaryJson repositorySummary =
  intercalate
    "\n"
    [ "    {",
      "      \"path\": " ++ renderJsonString (repositorySummaryPath repositorySummary) ++ ",",
      "      \"packages\": [",
      intercalate ",\n" (map (indentText 4 . renderRepositoryPackageSummaryJson) (repositorySummaryPackages repositorySummary)),
      "      ]",
      "    }"
    ]
indentText :: Int -> String -> String
indentText indentation = intercalate "\n" . map (replicate indentation ' ' ++) . lines
renderRepositoryPackageSummariesText :: [RepositoryPackageSummary] -> String
renderRepositoryPackageSummariesText packageSummaries =
  intercalate
    "\n"
    [ unlines
        ( [ renderRepositoryPackageFieldName "packageName" ++ " " ++ repositoryPackageName packageSummary,
            renderRepositoryPackageFieldName "packageType" ++ " " ++ renderPackageKind (repositoryPackageKind packageSummary),
            renderRepositoryPackageFieldName "description" ++ " " ++ fromMaybe "(none)" (repositoryPackageDescription packageSummary),
            renderRepositoryPackageFieldName "checks"
          ]
            ++ case enabledRepositoryPackageCheckNames packageSummary of
              [] -> [repositoryPackageValueIndent ++ "(none)"]
              checkNames -> [repositoryPackageValueIndent ++ checkName | checkName <- checkNames]
            ++ [renderRepositoryPackageFieldName "tests"]
            ++ case repositoryPackageTestNames packageSummary of
              [] -> [repositoryPackageValueIndent ++ "(none)"]
              testNames -> [repositoryPackageValueIndent ++ renderRepositoryPackageTestText packageSummary testName | testName <- testNames]
        )
    | packageSummary <- packageSummaries
    ]
    ++ if null packageSummaries then "" else "\n"
renderRepositoryPackageFieldName :: String -> String
renderRepositoryPackageFieldName fieldName =
  replicate (length ("description" :: String) - length fieldName) ' ' ++ fieldName ++ ":"
repositoryPackageValueIndent :: String
repositoryPackageValueIndent = replicate (length ("description: " :: String)) ' '
renderRepositoryPackageSummaryJson :: RepositoryPackageSummary -> String
renderRepositoryPackageSummaryJson packageSummary =
  intercalate
    "\n"
    [ "    {",
      "      \"name\": " ++ renderJsonString (repositoryPackageName packageSummary) ++ ",",
      "      \"packageType\": " ++ renderJsonString (renderPackageKind (repositoryPackageKind packageSummary)) ++ ",",
      "      \"description\": " ++ maybe "null" renderJsonString (repositoryPackageDescription packageSummary) ++ ",",
      "      \"checks\": " ++ renderRepositoryPackageChecksJson (repositoryPackageChecks packageSummary) ++ ",",
      "      \"coverage\": " ++ renderRepositoryPackageCoverageJson (repositoryPackageCoverage (repositoryPackageChecks packageSummary)) ++ ",",
      "      \"profile\": " ++ renderRepositoryPackageProfileJson (repositoryPackageProfile (repositoryPackageChecks packageSummary)) ++ ",",
      "      \"testDurationsSeconds\": " ++ renderRepositoryPackageTestDurationsJson (repositoryPackageTestDurations packageSummary) ++ ",",
      "      \"tests\": " ++ "[" ++ intercalate ", " (map renderJsonString (repositoryPackageTestNames packageSummary)) ++ "]",
      "    }"
    ]
renderRepositoryPackageChecksJson :: RepositoryPackageChecksSummary -> String
renderRepositoryPackageChecksJson packageChecks =
  "{ "
    ++ intercalate
      ", "
      [ "\"" ++ checkName ++ "\": " ++ if isEnabled then "true" else "false"
      | (checkName, isEnabled) <- repositoryPackageCheckEntries packageChecks
      ]
    ++ " }"
repositoryPackageCheckEntries :: RepositoryPackageChecksSummary -> [(String, Bool)]
repositoryPackageCheckEntries packageChecks =
  [ ("coverage", isJust (repositoryPackageCoverage packageChecks)),
    ("profile", isJust (repositoryPackageProfile packageChecks))
  ]
enabledRepositoryPackageCheckNames :: RepositoryPackageSummary -> [String]
enabledRepositoryPackageCheckNames packageSummary =
  ["coverage " ++ renderRepositoryPackageCoverageAnnotation coverage | coverage <- maybeToList (repositoryPackageCoverage packageChecks)]
    ++ ["profile " ++ renderRepositoryPackageProfileAnnotation profile | profile <- maybeToList (repositoryPackageProfile packageChecks)]
  where
    packageChecks = repositoryPackageChecks packageSummary
renderRepositoryPackageCoverageAnnotation :: RepositoryPackageCoverageSummary -> String
renderRepositoryPackageCoverageAnnotation = \case
  RepositoryCoverageNotRun -> "(not run)"
  RepositoryCoverageUnavailable _ -> "(unavailable)"
  RepositoryCoverageMeasured (CoverageMeasurement metric covered total) _ ->
    "(" ++ renderCoverageMetric metric ++ " " ++ show covered ++ "/" ++ show total ++ ", " ++ renderCoveragePercent covered total ++ "%)"
renderRepositoryPackageCoverageJson :: Maybe RepositoryPackageCoverageSummary -> String
renderRepositoryPackageCoverageJson = \case
  Nothing -> "{ \"status\": \"not-configured\" }"
  Just RepositoryCoverageNotRun -> "{ \"status\": \"not-run\" }"
  Just (RepositoryCoverageUnavailable _) -> "{ \"status\": \"unavailable\" }"
  Just (RepositoryCoverageMeasured (CoverageMeasurement metric covered total) _) ->
    "{ \"status\": \"measured\", \"metric\": "
      ++ renderJsonString (renderCoverageMetric metric)
      ++ ", \"covered\": "
      ++ show covered
      ++ ", \"total\": "
      ++ show total
      ++ ", \"percent\": "
      ++ renderCoveragePercent covered total
      ++ " }"
renderCoverageMetric :: CoverageMetric -> String
renderCoverageMetric = \case
  ExpressionCoverage -> "expressions"
  StatementCoverage -> "statements"
  LineCoverage -> "lines"
renderCoveragePercent :: Integer -> Integer -> String
renderCoveragePercent covered total =
  showFFloat (Just 1) (100 * fromIntegral covered / fromIntegral total :: Double) ""
renderRepositoryPackageProfileAnnotation :: RepositoryPackageProfileSummary -> String
renderRepositoryPackageProfileAnnotation = \case
  RepositoryProfileNotRun -> "(not run)"
  RepositoryProfileUnavailable -> "(unavailable)"
  RepositoryProfileMeasured totalSeconds _ -> "(all tests: " ++ renderProfileSeconds totalSeconds ++ "s)"
renderRepositoryPackageProfileJson :: Maybe RepositoryPackageProfileSummary -> String
renderRepositoryPackageProfileJson = \case
  Nothing -> "{ \"status\": \"not-configured\" }"
  Just RepositoryProfileNotRun -> "{ \"status\": \"not-run\" }"
  Just RepositoryProfileUnavailable -> "{ \"status\": \"unavailable\" }"
  Just (RepositoryProfileMeasured totalSeconds _) -> "{ \"status\": \"measured\", \"totalSeconds\": " ++ renderProfileSeconds totalSeconds ++ " }"
renderRepositoryPackageTestText :: RepositoryPackageSummary -> String -> String
renderRepositoryPackageTestText packageSummary testName =
  case Map.lookup testName testDurations of
    Just seconds ->
      let renderedSeconds = renderProfileSeconds seconds
          durationWidth = maximum (map (length . renderProfileSeconds) (Map.elems testDurations))
       in "(" ++ replicate (durationWidth - length renderedSeconds) ' ' ++ renderedSeconds ++ "s) " ++ testName
    Nothing -> testName
  where
    testDurations = repositoryPackageTestDurations packageSummary
repositoryPackageTestDurations :: RepositoryPackageSummary -> Map.Map String Duration
repositoryPackageTestDurations packageSummary =
  case repositoryPackageProfile packageChecks of
    Just (RepositoryProfileMeasured _ profileTestDurations) -> profileTestDurations
    _ ->
      case repositoryPackageCoverage packageChecks of
        Just (RepositoryCoverageUnavailable coverageTestDurations) -> coverageTestDurations
        Just (RepositoryCoverageMeasured _ coverageTestDurations) -> coverageTestDurations
        Just RepositoryCoverageNotRun -> Map.empty
        Nothing -> Map.empty
  where
    packageChecks = repositoryPackageChecks packageSummary
renderRepositoryPackageTestDurationsJson :: Map.Map String Duration -> String
renderRepositoryPackageTestDurationsJson testDurations
  | not (Map.null testDurations) =
      "{ "
        ++ intercalate ", " [renderJsonString testName ++ ": " ++ renderProfileSeconds seconds | (testName, seconds) <- Map.toAscList testDurations]
        ++ " }"
  | otherwise = "{}"
renderProfileSeconds :: Duration -> String
renderProfileSeconds (Duration seconds) = showFFloat (Just 3) seconds ""
summarizeRepositoryPackage :: Maybe (Map.Map FilePath FilePath) -> Set.Set FilePath -> FilePath -> IO RepositoryPackageSummary
summarizeRepositoryPackage checkOutputPaths repositoryCheckNames packageName = do
  packageKind <- detectPackageKindForPackage packageName
  let packageRoot = "packages" </> packageName
  repositoryPackageDescriptionValue <-
    case packageKind of
      HaskellPackage -> do
        maybeCabalContents <- readTextFileIfExists (packageRoot </> (packageName <.> "cabal"))
        pure (maybeCabalContents >>= extractHaskellPackageDescription)
      RustPackage -> do
        maybeCargoTomlContents <- readTextFileIfExists (packageRoot </> "Cargo.toml")
        pure (maybeCargoTomlContents >>= extractRustPackageDescription)
      _
        | packageKind `elem` [PythonPackage, PythonLatexPackage, PythonPyPIPackage] -> do
            maybePyprojectTomlContents <- readTextFileIfExists (packageRoot </> "pyproject.toml")
            maybeDefaultNixContents <- readTextFileIfExists (packageRoot </> "default.nix")
            let maybePyprojectDescription = maybePyprojectTomlContents >>= extractPythonPackageDescriptionFromPyprojectToml
                maybeDefaultNixDescription = maybeDefaultNixContents >>= extractDefaultNixPackageDescription
            pure (maybePyprojectDescription <|> maybeDefaultNixDescription)
      _ -> do
        maybeDefaultNixContents <- readTextFileIfExists (packageRoot </> "default.nix")
        pure (maybeDefaultNixContents >>= extractDefaultNixPackageDescription)
  repositoryPackageTestNamesValue <-
    case packageKind of
      HaskellPackage -> do
        maybeMainHaskellSourceText <- readTextFileIfExists (packageRoot </> "Main.hs")
        pure (maybe [] (discoverHaskellUnitTestNamesFromSource . T.unpack) maybeMainHaskellSourceText)
      RustPackage -> do
        maybeMainRustSourceText <- readTextFileIfExists (packageRoot </> "src/main.rs")
        pure (maybe [] (discoverRustUnitTestNamesFromSource . T.unpack) maybeMainRustSourceText)
      _
        | packageKind `elem` [PythonPackage, PythonLatexPackage] -> do
            maybeMainPythonSourceText <- readTextFileIfExists (packageRoot </> "main.py")
            pure (maybe [] (discoverPythonUnitTestNamesFromSource . T.unpack) maybeMainPythonSourceText)
      _ -> pure []
  let configuredRepositoryCheckName checkKind =
        repositoryCheckNameForKind packageKind packageName checkKind
          >>= \checkName ->
            if checkName `Set.member` repositoryCheckNames
              then Just checkName
              else Nothing
      coverageCheckName = configuredRepositoryCheckName RepositoryCoverageCheck
      profileCheckName =
        case configuredRepositoryCheckName RepositoryProfileCheck of
          Just checkName -> Just checkName
          Nothing
            | packageKind `elem` [PythonPackage, PythonLatexPackage] -> coverageCheckName
          Nothing -> Nothing
  repositoryPackageCoverageValue <-
    traverse (summarizeRepositoryPackageCoverage checkOutputPaths) coverageCheckName
  repositoryPackageProfileValue <-
    traverse (summarizeRepositoryPackageProfile checkOutputPaths) profileCheckName
  let repositoryPackageChecksValue =
        RepositoryPackageChecksSummary
          { repositoryPackageCoverage = repositoryPackageCoverageValue,
            repositoryPackageProfile = repositoryPackageProfileValue
          }
  pure
    RepositoryPackageSummary
      { repositoryPackageName = packageName,
        repositoryPackageKind = packageKind,
        repositoryPackageDescription = repositoryPackageDescriptionValue,
        repositoryPackageTestNames = repositoryPackageTestNamesValue,
        repositoryPackageChecks = repositoryPackageChecksValue
      }
resolveRepositoryCheckOutputPaths :: [FilePath] -> IO (Maybe (Map.Map FilePath FilePath))
resolveRepositoryCheckOutputPaths [] = pure (Just Map.empty)
resolveRepositoryCheckOutputPaths resultCheckNames = do
  repositoryRoot <- getCurrentDirectory
  let nixExpression =
        unlines
          [ "let",
            "  flake = builtins.getFlake " ++ renderJsonString (localGitFlakeReference repositoryRoot) ++ ";",
            "  checks = flake.checks.${builtins.currentSystem};",
            "  checkNames = [ " ++ unwords (map renderJsonString resultCheckNames) ++ " ];",
            "in",
            "builtins.concatStringsSep \"\\n\" (map (checkName: \"${checkName}\\t${checks.${checkName}.outPath}\") checkNames)"
          ]
  commandResult <- try (readProcessWithExitCode "nix" ["eval", "--raw", "--impure", "--expr", nixExpression] "")
  pure $
    case commandResult of
      Right (ExitSuccess, outputPathsText, _) -> parseRepositoryCheckOutputPaths resultCheckNames (T.pack outputPathsText)
      Right _ -> Nothing
      Left (_ :: IOException) -> Nothing
localGitFlakeReference :: FilePath -> String
localGitFlakeReference repositoryRoot = "git+file://" ++ repositoryRoot
parseRepositoryCheckOutputPaths :: [FilePath] -> T.Text -> Maybe (Map.Map FilePath FilePath)
parseRepositoryCheckOutputPaths expectedCheckNames outputPathsText = do
  entries <- mapM parseEntry (T.lines outputPathsText)
  let outputPaths = Map.fromList entries
  if Set.fromList (Map.keys outputPaths) == Set.fromList expectedCheckNames && Map.size outputPaths == length expectedCheckNames
    then Just outputPaths
    else Nothing
  where
    parseEntry outputPathLine =
      case T.splitOn "\t" outputPathLine of
        [checkName, outputPath] | not (T.null checkName) && not (T.null outputPath) -> Just (T.unpack checkName, T.unpack outputPath)
        _ -> Nothing
summarizeRepositoryPackageCoverage :: Maybe (Map.Map FilePath FilePath) -> FilePath -> IO RepositoryPackageCoverageSummary
summarizeRepositoryPackageCoverage Nothing _ =
  pure (RepositoryCoverageUnavailable Map.empty)
summarizeRepositoryPackageCoverage (Just coverageOutputPaths) coverageCheckName =
  case Map.lookup coverageCheckName coverageOutputPaths of
    Nothing -> pure (RepositoryCoverageUnavailable Map.empty)
    Just outputPath -> do
      outputExists <- doesDirectoryExist outputPath
      if not outputExists
        then pure RepositoryCoverageNotRun
        else do
          maybeCoverageText <- readTextFileIfExists (outputPath </> "coverage-summary.tsv")
          maybeTimingsText <- readTextFileIfExists (outputPath </> "test-timings.tsv")
          let testDurations = fromMaybe Map.empty (maybeTimingsText >>= parseRepositoryTestTimings)
          pure $
            case maybeCoverageText >>= parseRepositoryCoverageSummary of
              Nothing -> RepositoryCoverageUnavailable testDurations
              Just coverageMeasurement -> RepositoryCoverageMeasured coverageMeasurement testDurations
parseRepositoryCoverageSummary :: T.Text -> Maybe CoverageMeasurement
parseRepositoryCoverageSummary coverageText =
  case T.splitOn "\t" (T.strip coverageText) of
    ["coverage-v1", metricText, coveredText, totalText] -> do
      metric <-
        lookup
          metricText
          [ ("expressions", ExpressionCoverage),
            ("statements", StatementCoverage),
            ("lines", LineCoverage)
          ]
      covered <- readMaybe (T.unpack coveredText)
      total <- readMaybe (T.unpack totalText)
      if covered >= 0 && total > 0 && covered <= total
        then Just (CoverageMeasurement metric covered total)
        else Nothing
    _ -> Nothing
summarizeRepositoryPackageProfile :: Maybe (Map.Map FilePath FilePath) -> FilePath -> IO RepositoryPackageProfileSummary
summarizeRepositoryPackageProfile Nothing _ = pure RepositoryProfileUnavailable
summarizeRepositoryPackageProfile (Just checkOutputPaths) profileCheckName =
  case Map.lookup profileCheckName checkOutputPaths of
    Nothing -> pure RepositoryProfileUnavailable
    Just outputPath -> do
      outputExists <- doesDirectoryExist outputPath
      if not outputExists
        then pure RepositoryProfileNotRun
        else do
          maybeProfileText <- readTextFileIfExists (outputPath </> "profile-summary.tsv")
          pure (maybe RepositoryProfileUnavailable (fromMaybe RepositoryProfileUnavailable . parseRepositoryProfileSummary) maybeProfileText)
parseRepositoryProfileSummary :: T.Text -> Maybe RepositoryPackageProfileSummary
parseRepositoryProfileSummary profileText =
  case T.lines profileText of
    headerLine : testLines -> do
      totalSeconds <- case T.splitOn "\t" headerLine of
        ["profile-v1", "total-seconds", secondsText] -> readDuration secondsText
        _ -> Nothing
      testDurations <- parseRepositoryTestTimingLines testLines
      Just (RepositoryProfileMeasured totalSeconds testDurations)
    [] -> Nothing
parseRepositoryTestTimings :: T.Text -> Maybe (Map.Map String Duration)
parseRepositoryTestTimings = parseRepositoryTestTimingLines . T.lines
parseRepositoryTestTimingLines :: [T.Text] -> Maybe (Map.Map String Duration)
parseRepositoryTestTimingLines testLines = do
  testDurations <- Map.fromList <$> mapM parseTestLine testLines
  if Map.size testDurations == length testLines
    then Just testDurations
    else Nothing
  where
    parseTestLine :: T.Text -> Maybe (String, Duration)
    parseTestLine testLine =
      case T.splitOn "\t" testLine of
        ["test", secondsText, testName] | not (T.null testName) -> do
          seconds <- readDuration secondsText
          pure (T.unpack testName, seconds)
        _ -> Nothing
readDuration :: T.Text -> Maybe Duration
readDuration secondsText = do
  seconds <- readMaybe (T.unpack secondsText)
  durationFromSeconds seconds
durationFromSeconds :: Double -> Maybe Duration
durationFromSeconds seconds =
  if seconds >= 0 && not (isNaN seconds) && not (isInfinite seconds)
    then Just (Duration seconds)
    else Nothing
extractHaskellPackageDescription :: T.Text -> Maybe String
extractHaskellPackageDescription cabalContents =
  (T.unpack <$> lookupCabalField "description" cabalContents)
    <|> (T.unpack <$> lookupCabalField "synopsis" cabalContents)
extractRustPackageDescription :: T.Text -> Maybe String
extractRustPackageDescription cargoTomlContents =
  let packageSection = extractTomlSection "package" cargoTomlContents
   in T.unpack <$> lookupTomlString "description" packageSection
extractPythonPackageDescriptionFromPyprojectToml :: T.Text -> Maybe String
extractPythonPackageDescriptionFromPyprojectToml pyprojectTomlContents =
  T.unpack <$> lookupTomlString "description" (extractTomlSection "project" pyprojectTomlContents)
extractDefaultNixPackageDescription :: T.Text -> Maybe String
extractDefaultNixPackageDescription defaultNixContents =
  go False (T.lines defaultNixContents)
  where
    go _ [] = Nothing
    go insideMetaBlock (sourceLine : remainingLines) =
      case extractQuotedNixAssignmentValue "meta.description =" sourceLine
        <|> if insideMetaBlock then extractQuotedNixAssignmentValue "description =" sourceLine else Nothing of
        Just description -> Just (T.unpack description)
        Nothing
          | insideMetaBlock && "};" `T.isPrefixOf` T.strip sourceLine -> go False remainingLines
          | not insideMetaBlock && "meta = {" `T.isPrefixOf` T.strip sourceLine -> go True remainingLines
          | otherwise -> go insideMetaBlock remainingLines
extractQuotedNixAssignmentValue :: T.Text -> T.Text -> Maybe T.Text
extractQuotedNixAssignmentValue assignmentPrefix sourceLine = do
  quotedValue <- T.stripPrefix assignmentPrefix (T.strip sourceLine)
  valueWithoutSemicolon <- T.stripSuffix ";" (T.strip quotedValue)
  valueWithoutPrefix <- T.stripPrefix "\"" valueWithoutSemicolon
  T.stripSuffix "\"" valueWithoutPrefix
renderJsonString :: String -> String
renderJsonString value =
  "\"" ++ concatMap escapeCharacter value ++ "\""
  where
    escapeCharacter character =
      case character of
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> [character]
renderPackageKind :: PackageKind -> String
renderPackageKind packageKind =
  case packageKind of
    HaskellPackage -> "haskell"
    RustPackage -> "rust"
    HtmlPackage -> "html"
    PythonLatexPackage -> "python-latex"
    PythonPackage -> "python"
    PythonPyPIPackage -> "python-pypi"
    CPackage -> "c"
    TerraformPackage -> "terraform"
    LatexPackage -> "latex"
    BinaryReleasePackage -> "binary-release"
type ScaffoldPackageKind :: Type
data ScaffoldPackageKind
  = HaskellScaffold
  | RustScaffold
  | HtmlScaffold
  | PythonLatexScaffold
  | PythonScaffold
  | CScaffold
  | LatexScaffold
  deriving stock (Eq)
type PackageSpec :: Type
data PackageSpec = PackageSpec
  { packageSpecCliName :: String,
    packageSpecScaffoldKind :: ScaffoldPackageKind,
    packageSpecPackageKind :: PackageKind
  }
packageSpecs :: [PackageSpec]
packageSpecs =
  [ PackageSpec "haskell" HaskellScaffold HaskellPackage,
    PackageSpec "rust" RustScaffold RustPackage,
    PackageSpec "html" HtmlScaffold HtmlPackage,
    PackageSpec "python" PythonScaffold PythonPackage,
    PackageSpec "python-latex" PythonLatexScaffold PythonLatexPackage,
    PackageSpec "c" CScaffold CPackage,
    PackageSpec "latex" LatexScaffold LatexPackage
  ]
supportedAddPackageKinds :: [(String, ScaffoldPackageKind)]
supportedAddPackageKinds =
  [ (packageSpecCliName packageSpec, packageSpecScaffoldKind packageSpec)
  | packageSpec <- packageSpecs
  ]
parseSupportedAddPackageKind :: String -> Maybe ScaffoldPackageKind
parseSupportedAddPackageKind packageKindName = lookup packageKindName supportedAddPackageKinds
packageKindForScaffold :: ScaffoldPackageKind -> Maybe PackageKind
packageKindForScaffold scaffoldPackageKind =
  packageSpecPackageKind
    <$> find ((== scaffoldPackageKind) . packageSpecScaffoldKind) packageSpecs
validatePackageNameForKind :: PackageKind -> FilePath -> Maybe String
validatePackageNameForKind packageKind packageName =
  let (conventionName, separator) = packageNameConventionForKind packageKind
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
packageNameConventionForKind :: PackageKind -> (String, Char)
packageNameConventionForKind packageKind =
  case packageKind of
    HaskellPackage -> ("kebab-case", '-')
    RustPackage -> ("kebab-case", '-')
    BinaryReleasePackage -> ("kebab-case", '-')
    HtmlPackage -> ("snake_case", '_')
    PythonLatexPackage -> ("snake_case", '_')
    PythonPackage -> ("snake_case", '_')
    PythonPyPIPackage -> ("snake_case", '_')
    CPackage -> ("snake_case", '_')
    TerraformPackage -> ("snake_case", '_')
    LatexPackage -> ("snake_case", '_')
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
  { scaffoldFileRelativePath :: FilePath,
    scaffoldFileContents :: T.Text
  }
type RepositoryScaffoldFile :: Type
data RepositoryScaffoldFile = RepositoryScaffoldFile
  { repositoryScaffoldFilePath :: FilePath,
    repositoryScaffoldFileContents :: T.Text
  }
type RepositoryCheckSpec :: Type
data RepositoryCheckSpec = RepositoryCheckSpec
  { repositoryCheckNameSuffix :: String,
    repositoryCheckSource :: T.Text
  }
addPackageToCurrentRepositoryWith :: CanonicalizationSettings -> ScaffoldPackageKind -> FilePath -> Maybe String -> Set.Set RepositoryCheckKind -> IO (Either String [FilePath])
addPackageToCurrentRepositoryWith canonicalizationSettings scaffoldPackageKind packageName packageDescription requestedCheckKinds =
  case packageKindForScaffold scaffoldPackageKind of
    Nothing -> pure (Left "internal error: missing scaffold package specification")
    Just packageKind -> case validatePackageNameForKind packageKind packageName of
      Just validationError -> pure (Left validationError)
      Nothing -> do
        let packageRootDirectory = "packages" </> packageName
        packageRootExists <- doesPathExist packageRootDirectory
        if packageRootExists
          then
            addChecksToExistingPackage
              (Just packageKind)
              packageName
              packageDescription
              requestedCheckKinds
          else case validateRepositoryCheckSelection packageKind requestedCheckKinds of
            Left validationError -> pure (Left validationError)
            Right requestedCheckSpecs -> do
              let packageScaffoldFiles =
                    [ RepositoryScaffoldFile
                        (packageRootDirectory </> scaffoldFileRelativePath scaffoldFile)
                        (scaffoldFileContents scaffoldFile)
                    | scaffoldFile <- renderScaffoldFilesWith canonicalizationSettings scaffoldPackageKind packageName packageDescription
                    ]
                  checkScaffoldFiles = renderRepositoryCheckScaffoldFiles packageName requestedCheckSpecs
              createRepositoryScaffoldFiles (packageScaffoldFiles ++ checkScaffoldFiles)
addChecksToExistingPackage :: Maybe PackageKind -> FilePath -> Maybe String -> Set.Set RepositoryCheckKind -> IO (Either String [FilePath])
addChecksToExistingPackage maybeRequestedPackageKind packageName packageDescription requestedCheckKinds = do
  let packageRootDirectory = "packages" </> packageName
  packageRootExists <- doesPathExist packageRootDirectory
  packageRootIsDirectory <- doesDirectoryExist packageRootDirectory
  if not packageRootExists
    then pure (Left ("package does not exist: " ++ packageRootDirectory))
    else
      if not packageRootIsDirectory
        then pure (Left ("path already exists: " ++ packageRootDirectory))
        else case packageDescription of
          Just _ ->
            pure (Left ("cannot set a description when adding checks to existing package: " ++ packageRootDirectory))
          Nothing ->
            if Set.null requestedCheckKinds
              then pure (Left ("package already exists; specify at least one check option: " ++ packageRootDirectory))
              else do
                detectedPackageKind <- detectPackageKindForPackage packageName
                case maybeRequestedPackageKind of
                  Just requestedPackageKind
                    | detectedPackageKind /= requestedPackageKind ->
                        pure
                          ( Left
                              ( packageRootDirectory
                                  ++ " is a "
                                  ++ renderPackageKind detectedPackageKind
                                  ++ " package, not "
                                  ++ renderPackageKind requestedPackageKind
                              )
                          )
                  _ ->
                    case validatePackageNameForKind detectedPackageKind packageName of
                      Just validationError -> pure (Left validationError)
                      Nothing ->
                        case validateRepositoryCheckSelection detectedPackageKind requestedCheckKinds of
                          Left validationError -> pure (Left validationError)
                          Right requestedCheckSpecs ->
                            createRepositoryScaffoldFiles (renderRepositoryCheckScaffoldFiles packageName requestedCheckSpecs)
createRepositoryScaffoldFiles :: [RepositoryScaffoldFile] -> IO (Either String [FilePath])
createRepositoryScaffoldFiles scaffoldFiles = do
  let scaffoldPaths = map repositoryScaffoldFilePath scaffoldFiles
  existingPaths <- filterM doesPathExist scaffoldPaths
  case existingPaths of
    existingPath : _ -> pure (Left ("path already exists: " ++ existingPath))
    [] -> do
      forM_ scaffoldFiles $ \repositoryScaffoldFile -> do
        let absolutePath = repositoryScaffoldFilePath repositoryScaffoldFile
        createDirectoryIfMissing True (takeDirectory absolutePath)
        TIO.writeFile absolutePath (repositoryScaffoldFileContents repositoryScaffoldFile)
      pure (Right scaffoldPaths)
validateRepositoryCheckSelection :: PackageKind -> Set.Set RepositoryCheckKind -> Either String [RepositoryCheckSpec]
validateRepositoryCheckSelection packageKind requestedCheckKinds =
  let (unsupportedCheckKinds, requestedCheckSpecs) =
        partitionEithers
          [ maybe (Left requestedCheckKind) Right (repositoryCheckSpecForKind packageKind requestedCheckKind)
          | requestedCheckKind <- Set.toList requestedCheckKinds
          ]
   in case NE.nonEmpty unsupportedCheckKinds of
        Nothing -> Right requestedCheckSpecs
        Just unsupportedCheckKindsNonEmpty ->
          Left
            ( "unsupported checks for package type "
                ++ renderPackageKind packageKind
                ++ ": "
                ++ intercalate
                  ", "
                  [ case repositoryCheckKind of
                      RepositoryCoverageCheck -> "--coverage"
                      RepositoryProfileCheck -> "--profile"
                  | repositoryCheckKind <- NE.toList unsupportedCheckKindsNonEmpty
                  ]
            )
renderRepositoryCheckScaffoldFiles :: FilePath -> [RepositoryCheckSpec] -> [RepositoryScaffoldFile]
renderRepositoryCheckScaffoldFiles packageName requestedCheckSpecs =
  [ RepositoryScaffoldFile
      ("checks" </> (packageName ++ repositoryCheckNameSuffix checkSpec) </> "default.nix")
      (repositoryCheckSource checkSpec)
  | checkSpec <- requestedCheckSpecs
  ]
repositoryCheckNameForKind :: PackageKind -> FilePath -> RepositoryCheckKind -> Maybe FilePath
repositoryCheckNameForKind packageKind packageName repositoryCheckKind =
  (\checkSpec -> packageName ++ repositoryCheckNameSuffix checkSpec)
    <$> repositoryCheckSpecForKind packageKind repositoryCheckKind
repositoryCheckSpecForKind :: PackageKind -> RepositoryCheckKind -> Maybe RepositoryCheckSpec
repositoryCheckSpecForKind packageKind repositoryCheckKind =
  lookup (packageKind, repositoryCheckKind) repositoryCheckSpecs
repositoryCheckSpecs :: [((PackageKind, RepositoryCheckKind), RepositoryCheckSpec)]
repositoryCheckSpecs =
  [ spec HaskellPackage RepositoryCoverageCheck "-coverage" haskellCoverageCheckBaselineNixSource,
    spec HaskellPackage RepositoryProfileCheck "-profile" haskellProfileCheckBaselineNixSource,
    spec RustPackage RepositoryCoverageCheck "-coverage" rustCoverageCheckBaselineNixSource,
    spec RustPackage RepositoryProfileCheck "-profile" rustProfileCheckBaselineNixSource,
    spec PythonPackage RepositoryCoverageCheck "_coverage" pythonCoverageCheckBaselineNixSource,
    spec PythonLatexPackage RepositoryCoverageCheck "_coverage" pythonCoverageCheckBaselineNixSource
  ]
  where
    spec :: PackageKind -> RepositoryCheckKind -> String -> T.Text -> ((PackageKind, RepositoryCheckKind), RepositoryCheckSpec)
    spec packageKind checkKind suffix source =
      ((packageKind, checkKind), RepositoryCheckSpec suffix source)
renderScaffoldFilesWith :: CanonicalizationSettings -> ScaffoldPackageKind -> FilePath -> Maybe String -> [ScaffoldFile]
renderScaffoldFilesWith canonicalizationSettings scaffoldPackageKind packageName packageDescription =
  case scaffoldPackageKind of
    HaskellScaffold ->
      [ ScaffoldFile ".gitignore" haskellGitignoreSource,
        ScaffoldFile "default.nix" haskellTemplateBaselineNixSource,
        ScaffoldFile "Main.hs" haskellMainSource,
        ScaffoldFile (packageName <.> "cabal") (renderScaffoldHaskellCabal packageName packageDescription)
      ]
    RustScaffold ->
      [ ScaffoldFile ".gitignore" rustGitignoreSource,
        ScaffoldFile "default.nix" rustTemplateBaselineNixSource,
        ScaffoldFile "Cargo.toml" (renderScaffoldCargoToml packageName packageDescription),
        ScaffoldFile "src/main.rs" rustMainSource
      ]
    HtmlScaffold ->
      [ ScaffoldFile ".gitignore" htmlGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultHtmlTemplateDescription packageDescription htmlTemplateBaselineNixSource),
        ScaffoldFile "index.html" htmlIndexSource,
        ScaffoldFile "script.js" htmlScriptSource,
        ScaffoldFile "style.css" htmlStyleSource
      ]
    PythonLatexScaffold ->
      [ ScaffoldFile ".gitignore" pythonLatexGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultPythonLatexTemplateDescription packageDescription pythonLatexTemplateBaselineNixSource),
        ScaffoldFile "main.py" pythonLatexMainSource,
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
    PythonScaffold ->
      [ ScaffoldFile ".gitignore" pythonGitignoreSource,
        ScaffoldFile "default.nix" (renderPythonTemplateBaselineNixSourceWith (scaffoldDescription defaultPythonTemplateDescription packageDescription) (canonicalizationPythonPackageAttribute canonicalizationSettings)),
        ScaffoldFile "main.py" pythonMainSource
      ]
    CScaffold ->
      [ ScaffoldFile ".gitignore" cGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultCTemplateDescription packageDescription cTemplateBaselineNixSource),
        ScaffoldFile "main.c" cMainSource
      ]
    LatexScaffold ->
      [ ScaffoldFile ".gitignore" latexGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultLatexTemplateDescription packageDescription latexTemplateBaselineNixSource),
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
defaultPythonTemplateDescription :: String
defaultPythonTemplateDescription = "A Python template package."
defaultHaskellScaffoldDescription :: String
defaultHaskellScaffoldDescription = "Generated Haskell package"
defaultRustScaffoldDescription :: String
defaultRustScaffoldDescription = "Generated Rust package."
defaultHtmlTemplateDescription :: String
defaultHtmlTemplateDescription = "An HTML, CSS, and JavaScript template package."
defaultPythonLatexTemplateDescription :: String
defaultPythonLatexTemplateDescription = "A Python and LaTeX template package."
defaultCTemplateDescription :: String
defaultCTemplateDescription = "A C template package."
defaultLatexTemplateDescription :: String
defaultLatexTemplateDescription = "A LaTeX template package."
defaultPythonPackageAttribute :: String
defaultPythonPackageAttribute = "python312"
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
renderPythonTemplateBaselineNixSourceWith :: String -> String -> T.Text
renderPythonTemplateBaselineNixSourceWith packageDescription pythonPackageAttribute =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      T.pack ("  python = pkgs." ++ pythonPackageAttribute ++ ";"),
      "in",
      "python.pkgs.buildPythonPackage rec {",
      "  installPhase = ''",
      "    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py",
      "    install -Dm755 main.py $out/bin/${pname}",
      "    if [ -d prm ]; then",
      "      cp -r prm/ $out/${python.sitePackages}/",
      "      cp -r prm/ $out/bin/",
      "    fi",
      "  '';",
      "  meta = {",
      T.pack ("    description = \"" ++ escapeNixDoubleQuotedString packageDescription ++ "\";"),
      "    mainProgram = pname;",
      "  };",
      "  passthru.python = python;",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = [",
      "    python.pkgs.hypothesis",
      "  ];",
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
pythonTemplateBaselineNixSource =
  renderPythonTemplateBaselineNixSourceWith defaultPythonTemplateDescription defaultPythonPackageAttribute
renderScaffoldCargoToml :: FilePath -> Maybe String -> T.Text
renderScaffoldCargoToml packageName packageDescription =
  T.unlines
    [ line
    | sourceLine <- T.lines removeEmptyLinesCargoTomlFixture,
      let line =
            case sourceLine of
              "name = \"remove-empty-lines\"" -> T.pack ("name = \"" ++ packageName ++ "\"")
              "description = \"A CLI tool to remove empty lines from text files.\"" -> T.pack ("description = \"" ++ escapeNixDoubleQuotedString (scaffoldDescription defaultRustScaffoldDescription packageDescription) ++ "\"")
              "all = { level = \"deny\", priority = -1 }" -> "all = {level = \"deny\", priority = -1}"
              "pedantic = { level = \"deny\", priority = -1 }" -> "pedantic = {level = \"deny\", priority = -1}"
              "nursery = { level = \"deny\", priority = -1 }" -> "nursery = {level = \"deny\", priority = -1}"
              "cargo = { level = \"deny\", priority = -1 }" -> "cargo = {level = \"deny\", priority = -1}"
              otherLine -> otherLine
    ]
renderScaffoldHaskellCabal :: FilePath -> Maybe String -> T.Text
renderScaffoldHaskellCabal packageName packageDescription =
  T.unlines
    [ case sourceLine of
        "name:          haskell-template" -> T.pack ("name:          " ++ packageName)
        "synopsis:      Canonical Haskell package template" -> T.pack ("synopsis:      " ++ scaffoldDescription defaultHaskellScaffoldDescription packageDescription)
        "executable haskell-template" -> T.pack ("executable " ++ packageName)
        otherLine -> otherLine
    | sourceLine <- T.lines haskellCabalBaseline
    ]
checkRepositoryStructure :: IO [String]
checkRepositoryStructure = do
  repositoryPaths <- collectRepositoryPaths "."
  let relativePaths = sort [path | path <- repositoryPaths, path /= "."]
      leafPaths = Set.fromList (filter (isLeafPath relativePaths) relativePaths)
      packageRootPaths = Set.fromList (mapMaybe packageRootPathFromRepositoryPath relativePaths)
      hostRootPaths = Set.fromList (mapMaybe hostRootPathFromRepositoryPath relativePaths)
      packageInfos = map (buildPackageInfo leafPaths) (Set.toList packageRootPaths)
      globalAllowedPathRegexes :: [String]
      globalAllowedPathRegexes =
        [ "^\\.git(/.*)?$",
          "^AGENTS\\.md$",
          "^\\.github/workflows/workflow\\.yml$",
          "^\\.gitignore$",
          "^CITATION\\.bib$",
          "^LICENSE$",
          "^README$",
          "^checks/[^/]+/default\\.nix$",
          "^flake\\.lock$",
          "^flake\\.nix$",
          "^formatter\\.nix$",
          "^hosts/[^/]+/configuration\\.nix$",
          "^hosts/[^/]+/hardware-configuration\\.nix$",
          "^prm/[^/]+$",
          "^result$",
          "^secrets/secrets\\.age$",
          "^secrets/secrets\\.env\\.example$",
          "^secrets/secrets\\.nix$"
        ]
      packageAllowedPathRegexes =
        concat
          [ allowedPathRegexesForPackageKind (packageRootPath packageInfo) (packageRootDirectoryName packageInfo) (packageKindFromDetection (packageDetection packageInfo))
          | packageInfo <- packageInfos
          ]
      allowedPathRegexes = globalAllowedPathRegexes ++ packageAllowedPathRegexes
      missingPackageDefaultNixIssues =
        [ packageRootDirectory ++ ": missing required file default.nix"
        | packageRootDirectory <- Set.toList packageRootPaths,
          (packageRootDirectory </> "default.nix") `notElem` relativePaths
        ]
      missingHostConfigurationIssues =
        [ hostRootDirectory ++ ": missing required file configuration.nix"
        | hostRootDirectory <- Set.toList hostRootPaths,
          (hostRootDirectory </> "configuration.nix") `notElem` relativePaths
        ]
      missingCabalForMainHaskellIssues =
        [ packageRootDirectory ++ ": missing required file " ++ packageDirectoryName ++ ".cabal for Main.hs package"
        | packageRootDirectory <- Set.toList packageRootPaths,
          Set.member (packageRootDirectory </> "Main.hs") (Set.fromList relativePaths),
          let packageDirectoryName = takeBaseName packageRootDirectory,
          (packageRootDirectory </> packageDirectoryName <.> "cabal") `notElem` relativePaths
        ]
      misnamedCabalFileIssues =
        [ cabalFilePath ++ ": cabal file must be named " ++ packageDirectoryName ++ ".cabal"
        | cabalFilePath <- relativePaths,
          ".cabal" `isSuffixOf` cabalFilePath,
          let packageRootDirectory = takeDirectory cabalFilePath,
          "packages/" `isPrefixOf` packageRootDirectory,
          let packageDirectoryName = takeBaseName packageRootDirectory,
          takeFileName cabalFilePath /= packageDirectoryName <.> "cabal"
        ]
      ambiguousPackageMarkerIssues =
        concatMap ambiguousPackageMarkerIssuesForPackage packageInfos
      disallowedPathIssues =
        [ path ++ ": is not allowed"
        | path <- Set.toList leafPaths,
          not (any (path =~) allowedPathRegexes)
        ]
  packageNameConventionIssues <-
    fmap catMaybes $
      forM (Set.toList packageRootPaths) $ \packageRootDirectory -> do
        let packageName = takeBaseName packageRootDirectory
        packageKind <- detectPackageKindForPackage packageName
        pure (((packageRootDirectory ++ ": ") ++) <$> validatePackageNameForKind packageKind packageName)
  pure (missingPackageDefaultNixIssues ++ missingHostConfigurationIssues ++ missingCabalForMainHaskellIssues ++ misnamedCabalFileIssues ++ packageNameConventionIssues ++ ambiguousPackageMarkerIssues ++ disallowedPathIssues)
type PackageKind :: Type
data PackageKind
  = HaskellPackage
  | RustPackage
  | HtmlPackage
  | PythonLatexPackage
  | PythonPackage
  | PythonPyPIPackage
  | CPackage
  | TerraformPackage
  | LatexPackage
  | BinaryReleasePackage
  deriving stock (Eq, Ord, Show)
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
  | UnrecognizedPackageMarkers
buildPackageInfo :: Set.Set FilePath -> FilePath -> PackageInfo
buildPackageInfo leafPaths packageRootDirectory =
  let packageDirectoryName = takeBaseName packageRootDirectory
      packageRelativeLeafPaths = mapMaybe (stripPrefix (packageRootDirectory ++ "/")) (Set.toList leafPaths)
      markers = detectPackageMarkers packageRelativeLeafPaths
   in PackageInfo
        { packageRootPath = packageRootDirectory,
          packageRootDirectoryName = packageDirectoryName,
          packageDetection = detectPackageKindFromMarkers markers
        }
detectPackageMarkers :: [FilePath] -> [(String, PackageKind)]
detectPackageMarkers packageRelativeLeafPaths =
  let hasLeafPath leafPath = leafPath `elem` packageRelativeLeafPaths
      hasLeafPathWithPrefix pathPrefix = any (isPrefixOf pathPrefix) packageRelativeLeafPaths
      projectMarkers :: [(String, PackageKind)]
      projectMarkers =
        [ marker
        | (markerExists, marker) <-
            [ (hasLeafPath "Main.hs", ("Main.hs", HaskellPackage)),
              (hasLeafPath "Cargo.toml", ("Cargo.toml", RustPackage)),
              (hasLeafPath "index.html", ("index.html", HtmlPackage)),
              (hasLeafPath "main.py" && hasLeafPath "ms.tex", ("main.py+ms.tex", PythonLatexPackage)),
              (hasLeafPath "main.py" && not (hasLeafPath "ms.tex"), ("main.py", PythonPackage)),
              (hasLeafPath "main.c", ("main.c", CPackage)),
              (hasLeafPath "main.tf", ("main.tf", TerraformPackage)),
              (hasLeafPath "ms.tex" && not (hasLeafPath "main.py"), ("ms.tex", LatexPackage))
            ],
          markerExists
        ]
      binaryLayoutMarker :: [(String, PackageKind)]
      binaryLayoutMarker =
        [ ("binary-layout", BinaryReleasePackage)
        | null projectMarkers && not (hasLeafPathWithPrefix "Cargo.toml")
        ]
   in projectMarkers ++ binaryLayoutMarker
detectPackageKindFromMarkers :: [(String, PackageKind)] -> PackageDetection
detectPackageKindFromMarkers markers =
  case markers of
    [] -> UnrecognizedPackageMarkers
    [(_, markerKind)] -> DetectedPackageKind markerKind
    firstMarker : remainingMarkers ->
      AmbiguousPackageMarkers (fst firstMarker :| map fst remainingMarkers)
packageKindFromDetection :: PackageDetection -> Maybe PackageKind
packageKindFromDetection = \case
  DetectedPackageKind packageKind -> Just packageKind
  AmbiguousPackageMarkers _ -> Nothing
  UnrecognizedPackageMarkers -> Nothing
ambiguousPackageMarkerIssuesForPackage :: PackageInfo -> [String]
ambiguousPackageMarkerIssuesForPackage packageInfo =
  case packageDetection packageInfo of
    AmbiguousPackageMarkers matchedPackageMarkers ->
      [ packageRootPath packageInfo
          ++ ": has ambiguous project markers: "
          ++ intercalate ", " (NE.toList matchedPackageMarkers)
      ]
    DetectedPackageKind _ -> []
    UnrecognizedPackageMarkers -> []
allowedPathRegexesForPackageKind :: FilePath -> FilePath -> Maybe PackageKind -> [String]
allowedPathRegexesForPackageKind packageRootDirectory packageDirectoryName maybePackageKind =
  let escapedPackageRootDirectory = escapeRegexLiteral packageRootDirectory
      escapedPackageDirectoryName = escapeRegexLiteral packageDirectoryName
      basePackagePathRegexes = ["^" ++ escapedPackageRootDirectory ++ "/default\\.nix$", "^" ++ escapedPackageRootDirectory ++ "/\\.gitignore$"]
      withBasePackagePathRegexes additionalPathRegexes = basePackagePathRegexes ++ additionalPathRegexes
   in case maybePackageKind of
        Just HaskellPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/Main\\.hs$", "^" ++ escapedPackageRootDirectory ++ "/" ++ escapedPackageDirectoryName ++ "\\.cabal$"]
        Just RustPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/Cargo\\.toml$", "^" ++ escapedPackageRootDirectory ++ "/Cargo\\.lock$", "^" ++ escapedPackageRootDirectory ++ "/src/main\\.rs$"]
        Just HtmlPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/index\\.html$", "^" ++ escapedPackageRootDirectory ++ "/script\\.js$", "^" ++ escapedPackageRootDirectory ++ "/style\\.css$"]
        Just PythonLatexPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.py$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.tex$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.bib$", "^" ++ escapedPackageRootDirectory ++ "/refs\\.bib$", "^" ++ escapedPackageRootDirectory ++ "/figures(/.*)?$"]
        Just PythonPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.py$"]
        Just PythonPyPIPackage -> basePackagePathRegexes
        Just CPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.c$"]
        Just TerraformPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.tf$", "^" ++ escapedPackageRootDirectory ++ "/\\.terraform(/.*)?$", "^" ++ escapedPackageRootDirectory ++ "/\\.terraform\\.lock\\.hcl$"]
        Just LatexPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/ms\\.tex$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.bib$"]
        Just BinaryReleasePackage -> basePackagePathRegexes
        Nothing -> basePackagePathRegexes
escapeRegexLiteral :: String -> String
escapeRegexLiteral = concatMap escapeCharacter
  where
    escapeCharacter character
      | character `elem` ("\\.^$|?*+()[]{}" :: String) = ['\\', character]
      | otherwise = [character]
collectRepositoryPaths :: FilePath -> IO [FilePath]
collectRepositoryPaths rootPath = do
  childNames <- listDirectory rootPath
  let childPaths = sort [rootPath </> childName | childName <- childNames]
  descendantPaths <- fmap concat $
    forM childPaths $ \childPath -> do
      isDirectory <- doesDirectoryExist childPath
      let relativeChildPath = toRelativePath childPath
      case (isDirectory, shouldTraverseDirectory relativeChildPath) of
        (True, True) -> collectRepositoryPaths childPath
        (True, False) -> pure []
        (False, _) -> pure [relativeChildPath]
  pure (toRelativePath rootPath : descendantPaths)
toRelativePath :: FilePath -> FilePath
toRelativePath = makeRelative "."
shouldTraverseDirectory :: FilePath -> Bool
shouldTraverseDirectory repositoryPath =
  all
    (`notElem` ["tmp", "prm", "target", "result", ".agents", ".codex"])
    (splitDirectories repositoryPath)
isLeafPath :: [FilePath] -> FilePath -> Bool
isLeafPath repositoryPaths candidatePath =
  not (any ((== candidatePath) . takeDirectory) repositoryPaths)
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
listSubdirectoryNames :: FilePath -> IO [FilePath]
listSubdirectoryNames parentDirectory = do
  parentDirectoryExists <- doesDirectoryExist parentDirectory
  if not parentDirectoryExists
    then pure []
    else do
      childNames <- listDirectory parentDirectory
      sort <$> filterM (doesDirectoryExist . (parentDirectory </>)) childNames
checkPackageWith :: CanonicalizationSettings -> FilePath -> IO [String]
checkPackageWith canonicalizationSettings packageName = do
  let packageDefaultNixPath = "packages" </> packageName </> "default.nix"
  packageKind <- detectPackageKindForPackage packageName
  maybePackageDefaultNixSource <- readTextFileIfExists packageDefaultNixPath
  templateIssues <-
    case maybePackageDefaultNixSource of
      Nothing -> pure []
      Just packageDefaultNixSource -> do
        inferredTemplateSpec <- inferTemplateSpec packageName (T.unpack packageDefaultNixSource)
        case inferredTemplateSpec of
          Nothing ->
            pure ["packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"]
          Just templateSpec -> do
            let matchedTemplateName = templateName templateSpec
                allowedNixDifferenceKeysForPackage =
                  if packageName == "c_template" && matchedTemplateName == "c_template"
                    then defaultAllowedNixDifferenceKeys
                    else templateAllowedDifferenceKeys templateSpec
                templateSource =
                  case matchedTemplateName of
                    "python_template" ->
                      renderPythonTemplateBaselineNixSourceWith defaultPythonTemplateDescription (canonicalizationPythonPackageAttribute canonicalizationSettings)
                    "python_pypi_template" ->
                      pythonPyPITemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings)
                    "python_pypi_application_template" ->
                      pythonPyPIApplicationTemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings)
                    _ -> templateBaselineSource templateSpec
            comparePackageDefaultNixWithTemplate packageName packageDefaultNixPath ("packages" </> matchedTemplateName </> "default.nix") allowedNixDifferenceKeysForPackage templateSource
  cargoTomlIssues <- checkCargoToml packageName
  cabalFileIssues <- checkCabalFile packageName
  defaultNixConventionIssues <- checkDefaultNixConventions packageName packageKind
  pythonTestConventionIssues <- checkPythonTestConventions packageName packageKind
  haskellTestConventionIssues <- checkHaskellTestConventions packageName packageKind
  rustTestConventionIssues <- checkRustTestConventions packageName packageKind
  pure
    ( templateIssues
        ++ defaultNixConventionIssues
        ++ cargoTomlIssues
        ++ cabalFileIssues
        ++ pythonTestConventionIssues
        ++ haskellTestConventionIssues
        ++ rustTestConventionIssues
    )
checkTemplateWith :: FilePath -> IO [String]
checkTemplateWith checkName = do
  let checkTemplatePath = "checks" </> checkName </> "default.nix"
  maybeCheckTemplateText <- readTextFileIfExists checkTemplatePath
  case maybeCheckTemplateText of
    Nothing -> pure []
    Just checkTemplateText -> do
      inferredCheckTemplateSpec <- inferCheckTemplateSpec checkName (T.unpack checkTemplateText)
      case inferredCheckTemplateSpec of
        Nothing ->
          pure
            [ "checks/" ++ checkName ++ "/default.nix: could not infer corresponding check template"
            ]
        Just checkTemplateSpec -> do
          packageAssociationIssues <- validateCheckPackageAssociation checkName checkTemplatePath (checkTemplateName checkTemplateSpec)
          templateIssues <- validateCheckTemplate checkName checkTemplatePath checkTemplateSpec
          pure (packageAssociationIssues ++ templateIssues)
validateCheckPackageAssociation :: FilePath -> FilePath -> FilePath -> IO [String]
validateCheckPackageAssociation checkName checkTemplatePath matchedCheckTemplateName =
  case checkPackageAssociation matchedCheckTemplateName checkName of
    Nothing -> pure []
    Just (packageName, expectedPackageKinds) -> do
      packageDirectoryExists <- doesDirectoryExist ("packages" </> packageName)
      actualPackageKind <- detectPackageKindForPackage packageName
      pure
        [ checkTemplatePath
            ++ ": "
            ++ matchedCheckTemplateName
            ++ " requires corresponding "
            ++ intercalate " or " (map renderPackageKind expectedPackageKinds)
            ++ " package packages/"
            ++ packageName
        | not packageDirectoryExists || actualPackageKind `notElem` expectedPackageKinds
        ]
checkPackageAssociation :: FilePath -> FilePath -> Maybe (FilePath, [PackageKind])
checkPackageAssociation matchedCheckTemplateName checkName =
  case matchedCheckTemplateName of
    "haskell_coverage_check" -> withSuffix "-coverage" [HaskellPackage]
    "haskell_profile_check" -> withSuffix "-profile" [HaskellPackage]
    "python_coverage_check" -> withSuffix "_coverage" [PythonPackage, PythonLatexPackage]
    "rust_coverage_check" -> withSuffix "-coverage" [RustPackage]
    "rust_profile_check" -> withSuffix "-profile" [RustPackage]
    _ -> Nothing
  where
    withSuffix :: String -> [PackageKind] -> Maybe (FilePath, [PackageKind])
    withSuffix checkNameSuffix expectedPackageKinds =
      (\packageName -> (T.unpack packageName, expectedPackageKinds))
        <$> T.stripSuffix (T.pack checkNameSuffix) (T.pack checkName)
detectPackageKindForPackage :: FilePath -> IO PackageKind
detectPackageKindForPackage packageName = do
  let packageRootDirectory = "packages" </> packageName
      packageFileExists relativePath = doesFileExist (packageRootDirectory </> relativePath)
  packageMarkerFiles <-
    Set.fromList
      <$> filterM
        packageFileExists
        ["Main.hs", "Cargo.toml", "index.html", "main.py", "ms.tex", "main.c", "main.tf"]
  maybePackageDefaultNixSource <- readTextFileIfExists (packageRootDirectory </> "default.nix")
  let hasMarkerFile = (`Set.member` packageMarkerFiles)
      isPythonPyPIPackage =
        maybe
          False
          ( \packageDefaultNixSource ->
              let packageDefaultNixSourceString = T.unpack packageDefaultNixSource
               in ("buildPythonPackage" `isInfixOf` packageDefaultNixSourceString || "buildPythonApplication" `isInfixOf` packageDefaultNixSourceString)
                    && not ("src = ./.;" `isInfixOf` packageDefaultNixSourceString)
                    && ("fetchPypi" `isInfixOf` packageDefaultNixSourceString || "fetchurl" `isInfixOf` packageDefaultNixSourceString)
          )
          maybePackageDefaultNixSource
      packageKind
        | hasMarkerFile "Main.hs" = HaskellPackage
        | hasMarkerFile "Cargo.toml" = RustPackage
        | hasMarkerFile "index.html" = HtmlPackage
        | hasMarkerFile "main.py" && hasMarkerFile "ms.tex" = PythonLatexPackage
        | hasMarkerFile "main.py" = PythonPackage
        | isPythonPyPIPackage = PythonPyPIPackage
        | hasMarkerFile "main.c" = CPackage
        | hasMarkerFile "main.tf" = TerraformPackage
        | hasMarkerFile "ms.tex" = LatexPackage
        | otherwise = BinaryReleasePackage
  pure packageKind
readTextFileIfExists :: FilePath -> IO (Maybe T.Text)
readTextFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then Just <$> TIO.readFile filePath else pure Nothing
checkDefaultNixConventions :: FilePath -> PackageKind -> IO [String]
checkDefaultNixConventions packageName packageKind = do
  let packageDefaultNixPath = "packages" </> packageName </> "default.nix"
  maybeDefaultNixText <- readTextFileIfExists packageDefaultNixPath
  case maybeDefaultNixText of
    Nothing -> pure []
    Just defaultNixText ->
      let defaultNixSource = T.unpack defaultNixText
          hasTopLevelMainProgram = "\n  mainProgram = pname;" `isInfixOf` defaultNixSource
          hasMetaMainProgram =
            "meta.mainProgram = pname;" `isInfixOf` defaultNixSource
              || ("meta = {" `isInfixOf` defaultNixSource && "mainProgram = pname;" `isInfixOf` defaultNixSource)
          hasExternalFetchUrlSource = "src = pkgs.fetchurl" `isInfixOf` defaultNixSource
          hasLocalSource = "src = ./.;" `isInfixOf` defaultNixSource
          hasPlaceholderVersion = "version = \"0.0.0\";" `isInfixOf` defaultNixSource
          hasVersionAssignment = "version = \"" `isInfixOf` defaultNixSource
          expectsMetaMainProgram =
            packageKind `elem` [RustPackage, PythonLatexPackage, PythonPackage, CPackage, BinaryReleasePackage]
       in pure $
            catMaybes
              [ if packageKind == HaskellPackage && not hasTopLevelMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: Haskell packages must set mainProgram = pname;")
                  else Nothing,
                if packageKind == HaskellPackage && hasMetaMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: Haskell packages must not set meta.mainProgram = pname;")
                  else Nothing,
                if expectsMetaMainProgram && not hasMetaMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: package kind requires meta.mainProgram = pname;")
                  else Nothing,
                if expectsMetaMainProgram && hasTopLevelMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: package kind must use meta.mainProgram (not mainProgram)")
                  else Nothing,
                if hasExternalFetchUrlSource && hasPlaceholderVersion
                  then Just ("packages/" ++ packageName ++ "/default.nix: fetchurl-based packages must use a non-placeholder version")
                  else Nothing,
                if hasLocalSource && hasVersionAssignment && not hasPlaceholderVersion
                  then Just ("packages/" ++ packageName ++ "/default.nix: src = ./.; packages must use version = \"0.0.0\";")
                  else Nothing
              ]
checkPythonTestConventions :: FilePath -> PackageKind -> IO [String]
checkPythonTestConventions packageName packageKind =
  if packageKind `notElem` [PythonPackage, PythonLatexPackage]
    then pure []
    else do
      let mainPythonPath = "packages" </> packageName </> "main.py"
      mainPythonFileExists <- doesFileExist mainPythonPath
      if not mainPythonFileExists
        then pure []
        else do
          python3Path <- findExecutable "python3"
          case python3Path of
            Nothing ->
              pure
                [ "packages/"
                    ++ packageName
                    ++ "/main.py: missing Python 3 interpreter"
                ]
            Just pythonCommand -> do
              (exitCode, validatorStdout, validatorStderr) <- readProcessWithExitCode pythonCommand ["-c", pythonUnittestValidatorPythonSource, mainPythonPath] ""
              let validatorOutputLines = lines validatorStdout
                  validatorErrorCodes = [drop 4 validatorOutputLine | validatorOutputLine <- validatorOutputLines, "ERR " `isPrefixOf` validatorOutputLine]
                  validatorErrorMessages = map (formatPythonValidatorError packageName) validatorErrorCodes
              case exitCode of
                ExitSuccess ->
                  if "OK" `elem` validatorOutputLines
                    then pure []
                    else
                      pure
                        [ "packages/" ++ packageName ++ "/main.py: python AST validator produced unexpected output"
                        ]
                ExitFailure 1 -> pure validatorErrorMessages
                ExitFailure _ ->
                  pure
                    [ "packages/"
                        ++ packageName
                        ++ "/main.py: python AST validator execution failed: "
                        ++ compactTextToSingleLine (T.pack validatorStderr)
                    ]
discoverPythonUnitTestNamesFromSource :: String -> [String]
discoverPythonUnitTestNamesFromSource =
  Set.toAscList . Set.fromList . discoverPythonTestSpecifications . lines
discoverPythonTestSpecifications :: [String] -> [String]
discoverPythonTestSpecifications [] = []
discoverPythonTestSpecifications (sourceLine : remainingLines) =
  case extractPythonUnitTestName sourceLine of
    Nothing -> discoverPythonTestSpecifications remainingLines
    Just testName ->
      let (definitionLines, linesAfterDefinition) = break (isSuffixOf ":" . dropWhileEndIsSpace) (sourceLine : remainingLines)
          sourceAfterDefinition = case linesAfterDefinition of
            [] -> drop (length definitionLines) (sourceLine : remainingLines)
            _ : sourceAfter -> sourceAfter
          testSpecification =
            fromMaybe
              (testSpecificationFromIdentifier testName)
              (listToMaybe (dropWhile (null . dropWhile isSpace) sourceAfterDefinition) >>= extractPythonTestDocstring)
       in testSpecification : discoverPythonTestSpecifications sourceAfterDefinition
  where
    dropWhileEndIsSpace = reverse . dropWhile isSpace . reverse
extractPythonUnitTestName :: String -> Maybe String
extractPythonUnitTestName sourceLine =
  case stripPrefix "def " (dropWhile (== ' ') sourceLine) of
    Just functionDefinition
      | "test_" `isPrefixOf` functionDefinition ->
          let functionName = takeWhile (`notElem` ("( :" :: String)) functionDefinition
           in if null functionName then Nothing else Just functionName
    _ -> Nothing
extractPythonTestDocstring :: String -> Maybe String
extractPythonTestDocstring sourceLine =
  let trimmedLine = dropWhile isSpace sourceLine
      extractBetween delimiter = do
        sourceAfterOpeningDelimiter <- stripPrefix delimiter trimmedLine
        let (description, sourceAfterDescription) = breakOnString delimiter sourceAfterOpeningDelimiter
        if null description || null sourceAfterDescription then Nothing else Just description
   in extractBetween "\"\"\"" <|> extractBetween "'''"
breakOnString :: String -> String -> (String, String)
breakOnString delimiter = go []
  where
    go reversedPrefix remainingSource
      | delimiter `isPrefixOf` remainingSource = (reverse reversedPrefix, remainingSource)
    go reversedPrefix (character : remainingSource) = go (character : reversedPrefix) remainingSource
    go reversedPrefix [] = (reverse reversedPrefix, [])
testSpecificationFromIdentifier :: String -> String
testSpecificationFromIdentifier identifier =
  case wordsFromTestIdentifier (stripTestFrameworkPrefixes identifier) of
    [] -> identifier
    (firstCharacter : firstWordRest) : remainingWords ->
      let sentence = unwords ((toUpper firstCharacter : firstWordRest) : remainingWords)
       in case reverse sentence of
            punctuation : _ | punctuation `elem` (".!?" :: String) -> sentence
            _ -> sentence ++ "."
    _ -> identifier
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
checkHaskellTestConventions :: FilePath -> PackageKind -> IO [String]
checkHaskellTestConventions packageName packageKind =
  if packageKind /= HaskellPackage
    then pure []
    else do
      let mainHaskellPath = "packages" </> packageName </> "Main.hs"
      maybeMainHaskellSourceText <- readTextFileIfExists mainHaskellPath
      case maybeMainHaskellSourceText of
        Nothing -> pure []
        Just mainHaskellSourceText -> do
          let haskellSource = T.unpack mainHaskellSourceText
              haskellIdentifiers = [identifier | HaskellIdentifier identifier <- lexHaskellSource haskellSource]
              hasHUnitTestRunner = "runTestTT" `elem` haskellIdentifiers
              hasNamedTestSuite = "hUnitPackageTests" `elem` haskellIdentifiers
              hasDiscoverableHUnitTest = any isMeaningfulTestLabel (discoverHaskellTestLabels haskellSource)
          pure $
            catMaybes
              [ if hasHUnitTestRunner
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: must run HUnit tests with runTestTT"),
                if hasNamedTestSuite
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: HUnit tests must use hUnitPackageTests"),
                if hasDiscoverableHUnitTest
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: HUnit tests must use literal TestLabel descriptions")
              ]
discoverHaskellUnitTestNamesFromSource :: String -> [String]
discoverHaskellUnitTestNamesFromSource haskellSource =
  Set.toAscList . Set.fromList $
    filter isMeaningfulTestLabel (discoverHaskellTestLabels haskellSource)
      ++ map testSpecificationFromIdentifier (discoverHaskellPropertyNames haskellSource)
isMeaningfulTestLabel :: String -> Bool
isMeaningfulTestLabel label =
  case dropWhile isSpace label of
    firstCharacter : _ -> isAlphaNum firstCharacter
    [] -> False
type HaskellSourceToken :: Type
data HaskellSourceToken
  = HaskellIdentifier String
  | HaskellStringLiteral String
  | HaskellSymbol String
  deriving stock (Eq, Show)
discoverHaskellTestLabels :: String -> [String]
discoverHaskellTestLabels = go . lexHaskellSource
  where
    go (HaskellIdentifier "TestLabel" : HaskellStringLiteral label : remainingTokens) = label : go remainingTokens
    go (_ : remainingTokens) = go remainingTokens
    go [] = []
discoverHaskellPropertyNames :: String -> [String]
discoverHaskellPropertyNames = go . lexHaskellSource
  where
    go (HaskellIdentifier propertyName : HaskellSymbol declarationSymbol : remainingTokens)
      | "prop_" `isPrefixOf` propertyName && declarationSymbol `elem` ["::", "="] = propertyName : go remainingTokens
    go (_ : remainingTokens) = go remainingTokens
    go [] = []
lexHaskellSource :: String -> [HaskellSourceToken]
lexHaskellSource = go
  where
    go [] = []
    go ('-' : '-' : remainingSource) = go (dropHaskellLineComment remainingSource)
    go ('{' : '-' : remainingSource) = go (dropHaskellBlockComment 1 remainingSource)
    go ('"' : remainingSource) =
      case consumeHaskellString ['"'] remainingSource of
        Just (literalSource, sourceAfterLiteral) ->
          case reads literalSource of
            [(literalValue, "")] -> HaskellStringLiteral literalValue : go sourceAfterLiteral
            _ -> go sourceAfterLiteral
        Nothing -> []
    go ('\'' : remainingSource) = go (dropHaskellCharacterLiteral remainingSource)
    go (':' : ':' : remainingSource) = HaskellSymbol "::" : go remainingSource
    go ('=' : remainingSource) = HaskellSymbol "=" : go remainingSource
    go (firstCharacter : remainingSource)
      | isHaskellIdentifierStart firstCharacter =
          let (identifierTail, sourceAfterIdentifier) = span isHaskellIdentifierCharacter remainingSource
           in HaskellIdentifier (firstCharacter : identifierTail) : go sourceAfterIdentifier
      | otherwise = go remainingSource
isHaskellIdentifierStart :: Char -> Bool
isHaskellIdentifierStart character = isAlphaNum character || character == '_'
isHaskellIdentifierCharacter :: Char -> Bool
isHaskellIdentifierCharacter character = isHaskellIdentifierStart character || character == '\''
dropHaskellLineComment :: String -> String
dropHaskellLineComment source =
  case dropWhile (/= '\n') source of
    [] -> []
    _ : remainingSource -> remainingSource
dropHaskellBlockComment :: Int -> String -> String
dropHaskellBlockComment _ [] = []
dropHaskellBlockComment nestingDepth ('{' : '-' : remainingSource) = dropHaskellBlockComment (nestingDepth + 1) remainingSource
dropHaskellBlockComment nestingDepth ('-' : '}' : remainingSource)
  | nestingDepth == 1 = remainingSource
  | otherwise = dropHaskellBlockComment (nestingDepth - 1) remainingSource
dropHaskellBlockComment nestingDepth (_ : remainingSource) = dropHaskellBlockComment nestingDepth remainingSource
consumeHaskellString :: String -> String -> Maybe (String, String)
consumeHaskellString _ [] = Nothing
consumeHaskellString reversedLiteral ('\\' : escapedCharacter : remainingSource) =
  consumeHaskellString (escapedCharacter : '\\' : reversedLiteral) remainingSource
consumeHaskellString reversedLiteral ('"' : remainingSource) =
  Just (reverse ('"' : reversedLiteral), remainingSource)
consumeHaskellString reversedLiteral (character : remainingSource) =
  consumeHaskellString (character : reversedLiteral) remainingSource
dropHaskellCharacterLiteral :: String -> String
dropHaskellCharacterLiteral [] = []
dropHaskellCharacterLiteral ('\\' : _ : remainingSource) = dropHaskellCharacterLiteral remainingSource
dropHaskellCharacterLiteral ('\'' : remainingSource) = remainingSource
dropHaskellCharacterLiteral (_ : remainingSource) = dropHaskellCharacterLiteral remainingSource
checkRustTestConventions :: FilePath -> PackageKind -> IO [String]
checkRustTestConventions packageName packageKind =
  if packageKind /= RustPackage
    then pure []
    else do
      let mainRustPath = "packages" </> packageName </> "src/main.rs"
      mainRustSource <- maybe "" T.unpack <$> readTextFileIfExists mainRustPath
      let hasRustTestModule = "#[cfg(test)]" `isInfixOf` mainRustSource && "mod tests" `isInfixOf` mainRustSource
          hasRustTestCases = "#[test]" `isInfixOf` mainRustSource
      pure $
        catMaybes
          [ if hasRustTestModule
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[cfg(test)] mod tests"),
            if hasRustTestCases
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[test] test cases")
          ]
discoverRustUnitTestNamesFromSource :: String -> [String]
discoverRustUnitTestNamesFromSource = map testSpecificationFromIdentifier . extractRustUnitTestNames . lines
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
formatPythonValidatorError :: FilePath -> String -> String
formatPythonValidatorError packageName errorCode =
  let messagePrefix = "packages/" ++ packageName ++ "/main.py: "
   in case errorCode of
        "parse_error" -> messagePrefix ++ "python source could not be parsed"
        _ -> messagePrefix ++ "python validator failed with error code: " ++ errorCode
pythonUnittestValidatorPythonSource :: String
pythonUnittestValidatorPythonSource =
  unlines
    [ "import ast",
      "import sys",
      "",
      "def main():",
      "    path = sys.argv[1]",
      "    try:",
      "        source = open(path, encoding='utf-8').read()",
      "        ast.parse(source, filename=path)",
      "    except Exception:",
      "        print('ERR parse_error')",
      "        sys.exit(2)",
      "    print('OK')",
      "    sys.exit(0)",
      "",
      "if __name__ == '__main__':",
      "    main()"
    ]
checkCargoToml :: FilePath -> IO [String]
checkCargoToml packageName = do
  let cargoTomlPath = "packages" </> packageName </> "Cargo.toml"
  maybeCargoTomlContents <- readTextFileIfExists cargoTomlPath
  case maybeCargoTomlContents of
    Nothing -> pure []
    Just cargoTomlContents -> do
      let packageSection = extractTomlSection "package" cargoTomlContents
          rustLintsSection = extractTomlSection "lints.rust" cargoTomlContents
          cargoPackageName = lookupTomlString "name" packageSection
          unsafeCodeLint = lookupTomlString "unsafe_code" rustLintsSection
          normalizedCargoToml = normalizeCargoTomlForBaselineComparison packageName cargoTomlContents
          normalizedBaselineCargoToml = normalizeCargoTomlForBaselineComparison packageName rustCargoTomlBaseline
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
            if unsafeCodeLint == Just "forbid"
              then Nothing
              else Just ("packages/" ++ packageName ++ "/Cargo.toml: [lints.rust].unsafe_code must be \"forbid\""),
            if normalizedCargoToml == normalizedBaselineCargoToml
              then Nothing
              else
                Just
                  ( "packages/"
                      ++ packageName
                      ++ "/Cargo.toml: only dependency sections and package metadata fields description/keywords may differ from the internal Rust Cargo baseline"
                  )
          ]
normalizeCargoTomlForBaselineComparison :: FilePath -> T.Text -> T.Text
normalizeCargoTomlForBaselineComparison packageName tomlContents =
  let step currentTomlSectionHeader sourceLine
        | isTomlSectionHeader trimmedLine =
            ( Just trimmedLine,
              if isCargoDependencySectionHeader trimmedLine
                then Nothing
                else Just trimmedLine
            )
        | maybe False isCargoDependencySectionHeader currentTomlSectionHeader =
            (currentTomlSectionHeader, Nothing)
        | T.null trimmedLine =
            (currentTomlSectionHeader, Nothing)
        | currentTomlSectionHeader == Just "[package]" && isTomlNameAssignment trimmedLine =
            (currentTomlSectionHeader, Just normalizedNameLine)
        | currentTomlSectionHeader == Just "[package]"
            && (isTomlDescriptionAssignment trimmedLine || isTomlKeywordsAssignment trimmedLine) =
            (currentTomlSectionHeader, Nothing)
        | currentTomlSectionHeader == Just "[[bin]]" && isTomlNameAssignment trimmedLine =
            (currentTomlSectionHeader, Just normalizedNameLine)
        | otherwise =
            (currentTomlSectionHeader, Just trimmedLine)
        where
          trimmedLine = T.strip sourceLine
      (_, normalizedLines) = mapAccumL step Nothing (T.lines tomlContents)
      normalizedNameLine = "name = \"" <> T.pack packageName <> "\""
   in T.unlines (catMaybes normalizedLines)
isCargoDependencySectionHeader :: T.Text -> Bool
isCargoDependencySectionHeader trimmedLine =
  trimmedLine == "[dependencies]"
    || trimmedLine == "[dev-dependencies]"
    || trimmedLine == "[build-dependencies]"
    || isCargoTargetDependenciesSectionHeader trimmedLine
isCargoTargetDependenciesSectionHeader :: T.Text -> Bool
isCargoTargetDependenciesSectionHeader trimmedLine =
  T.isPrefixOf "[target." trimmedLine
    && T.isSuffixOf ".dependencies]" trimmedLine
isTomlNameAssignment :: T.Text -> Bool
isTomlNameAssignment trimmedLine = "name = \"" `T.isPrefixOf` trimmedLine
isTomlDescriptionAssignment :: T.Text -> Bool
isTomlDescriptionAssignment trimmedLine = "description = \"" `T.isPrefixOf` trimmedLine
isTomlKeywordsAssignment :: T.Text -> Bool
isTomlKeywordsAssignment trimmedLine = "keywords = [" `T.isPrefixOf` trimmedLine
extractTomlSection :: T.Text -> T.Text -> T.Text
extractTomlSection sectionName tomlContents =
  let sectionHeader = "[" <> sectionName <> "]"
      tomlLines = T.lines tomlContents
      sectionStart = dropWhile (\tomlLine -> T.strip tomlLine /= sectionHeader) tomlLines
      sectionBody = drop 1 sectionStart
   in T.unlines (takeWhile (not . isTomlSectionHeader . T.strip) sectionBody)
isTomlSectionHeader :: T.Text -> Bool
isTomlSectionHeader tomlLine =
  T.length tomlLine >= 3
    && "[" `T.isPrefixOf` tomlLine
    && "]" `T.isSuffixOf` tomlLine
lookupTomlString :: T.Text -> T.Text -> Maybe T.Text
lookupTomlString tomlKey sectionContents = do
  let keyPrefix = tomlKey <> " = "
  matchingLine <- find (T.isPrefixOf keyPrefix) (map T.strip (T.lines sectionContents))
  quotedValue <- T.stripPrefix keyPrefix matchingLine
  T.stripPrefix "\"" quotedValue >>= T.stripSuffix "\""
checkCabalFile :: FilePath -> IO [String]
checkCabalFile packageName = do
  let cabalFilePath = "packages" </> packageName </> packageName <.> "cabal"
  maybeCabalContents <- readTextFileIfExists cabalFilePath
  case maybeCabalContents of
    Nothing -> pure []
    Just cabalContents -> do
      let normalizedCabal = normalizeCabalForBaselineComparison packageName cabalContents
          normalizedBaselineCabal = normalizeCabalForBaselineComparison packageName haskellCabalBaseline
          cabalPackageName = lookupCabalField "name" cabalContents
      pure $
        catMaybes
          [ if cabalPackageName == Just (T.pack packageName)
              then Nothing
              else Just ("packages/" ++ packageName ++ "/" ++ packageName ++ ".cabal: name must match directory name"),
            if normalizedCabal == normalizedBaselineCabal
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
normalizeCabalForBaselineComparison :: FilePath -> T.Text -> T.Text
normalizeCabalForBaselineComparison packageName cabalContents =
  let step (insideBuildDependsSection, insideIgnoredMetadataField) sourceLine
        | insideBuildDependsSection =
            if T.null trimmedLine || T.null (snd (T.breakOn ":" trimmedLine))
              then ((True, False), Nothing)
              else ((False, False), Just normalizedLine)
        | insideIgnoredMetadataField && isCabalIndentedContinuationLine sourceLine =
            ((False, True), Nothing)
        | "build-depends:" `T.isPrefixOf` trimmedLine =
            ((True, False), Nothing)
        | T.null trimmedLine =
            ((False, False), Nothing)
        | isCabalSynopsisField trimmedLine || isCabalDescriptionField trimmedLine =
            ((False, True), Nothing)
        | otherwise =
            ((False, False), Just normalizedLine)
        where
          trimmedLine = T.strip sourceLine
          normalizedLine = normalizeCabalLineForBaselineComparison packageName trimmedLine
      (_, normalizedLines) = mapAccumL step (False, False) (T.lines cabalContents)
   in T.unlines (catMaybes normalizedLines)
normalizeCabalLineForBaselineComparison :: FilePath -> T.Text -> T.Text
normalizeCabalLineForBaselineComparison packageName trimmedLine
  | "name:" `T.isPrefixOf` trimmedLine = "name:          " <> T.pack packageName
  | "executable " `T.isPrefixOf` trimmedLine = "executable " <> T.pack packageName
  | otherwise = trimmedLine
isCabalSynopsisField :: T.Text -> Bool
isCabalSynopsisField trimmedLine = "synopsis:" `T.isPrefixOf` trimmedLine
isCabalDescriptionField :: T.Text -> Bool
isCabalDescriptionField trimmedLine = "description:" `T.isPrefixOf` trimmedLine
isCabalIndentedContinuationLine :: T.Text -> Bool
isCabalIndentedContinuationLine sourceLine =
  case T.uncons sourceLine of
    Just (firstCharacter, _) -> firstCharacter == ' ' || firstCharacter == '\t'
    Nothing -> False
lookupCabalField :: T.Text -> T.Text -> Maybe T.Text
lookupCabalField cabalField cabalContents =
  let fieldPrefix = cabalField <> ":"
      matchingLines = dropWhile (\sourceLine -> not (fieldPrefix `T.isPrefixOf` T.strip sourceLine)) (T.lines cabalContents)
   in case matchingLines of
        [] -> Nothing
        sourceLine : remainingLines ->
          let strippedValue = T.strip (T.drop (T.length fieldPrefix) (T.strip sourceLine))
           in Just $
                if T.null strippedValue
                  then T.intercalate "\n" (map T.strip (takeWhile isCabalIndentedContinuationLine remainingLines))
                  else stripCabalQuotedValue strippedValue
stripCabalQuotedValue :: T.Text -> T.Text
stripCabalQuotedValue quotedValue =
  fromMaybe quotedValue (T.stripPrefix "\"" quotedValue >>= T.stripSuffix "\"")
comparePackageDefaultNixWithTemplate :: FilePath -> FilePath -> FilePath -> Set.Set T.Text -> T.Text -> IO [String]
comparePackageDefaultNixWithTemplate packageName subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys templateBaselineSourceText = do
  packageKind <- detectPackageKindForPackage packageName
  let ignoredTopLevelFunctionParams = optionalTemplateFunctionParams packageKind templateBaselineNixPath
  compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys templateBaselineSourceText
optionalTemplateFunctionParams :: PackageKind -> FilePath -> Set.Set T.Text
optionalTemplateFunctionParams packageKind templateBaselineNixPath =
  if packageKind == CPackage || takeBaseName (takeDirectory templateBaselineNixPath) == "python_template"
    then Set.singleton "inputs"
    else Set.empty
compareCheckTemplateWithBaseline :: FilePath -> T.Text -> IO [String]
compareCheckTemplateWithBaseline checkTemplatePath templateBaselineText = do
  checkTemplateSource <- TIO.readFile checkTemplatePath
  let normalizedCheckDefaultNix = normalizePythonPackageAttributeReferences (T.strip checkTemplateSource)
      normalizedTemplateDefaultNix = normalizePythonPackageAttributeReferences (T.strip templateBaselineText)
  pure $
    if normalizedCheckDefaultNix == normalizedTemplateDefaultNix
      then []
      else
        let checkDefaultNixLines = T.lines normalizedCheckDefaultNix
            templateDefaultNixLines = T.lines normalizedTemplateDefaultNix
            mismatchDetails =
              case firstMismatchedLine checkDefaultNixLines templateDefaultNixLines of
                Just (lineNumber, actualLine, expectedLine) ->
                  [ "  - changed line " ++ show lineNumber,
                    "    expected: " ++ truncateDiagnosticValue (T.unpack expectedLine),
                    "    actual:   " ++ truncateDiagnosticValue (T.unpack actualLine)
                  ]
                Nothing ->
                  [ "  - expected normalized form: " ++ truncateDiagnosticValue (compactTextToSingleLine normalizedTemplateDefaultNix),
                    "  - actual normalized form:   " ++ truncateDiagnosticValue (compactTextToSingleLine normalizedCheckDefaultNix)
                  ]
         in [ checkTemplatePath
                ++ ": differs from embedded check template\n"
                ++ intercalate "\n" mismatchDetails
            ]
normalizePythonPackageAttributeReferences :: T.Text -> T.Text
normalizePythonPackageAttributeReferences =
  T.pack . go . T.unpack
  where
    go [] = []
    go textValue@(sourceChar : remainingChars) =
      case stripPrefix "pkgs.python" textValue of
        Just remainder ->
          let (pythonVersionDigits, remainderAfterVersion) = span (`elem` ['0' .. '9']) remainder
           in if null pythonVersionDigits
                then sourceChar : go remainingChars
                else "pkgs.python3" ++ go remainderAfterVersion
        Nothing -> sourceChar : go remainingChars
validateCheckTemplate :: FilePath -> FilePath -> CheckTemplateSpec -> IO [String]
validateCheckTemplate checkName checkTemplatePath checkTemplateSpec =
  case checkTemplateComparisonMode checkTemplateSpec of
    ExactCheckTemplate ->
      compareCheckTemplateWithBaseline checkTemplatePath (checkTemplateBaselineSource checkTemplateSpec)
    StructuralCPackageVm ->
      validateCPackageVmCheck checkName checkTemplatePath
validateCPackageVmCheck :: FilePath -> FilePath -> IO [String]
validateCPackageVmCheck checkName checkTemplatePath = do
  maybeCheckTemplateText <- readTextFileIfExists checkTemplatePath
  case maybeCheckTemplateText of
    Nothing -> pure []
    Just checkTemplateText -> do
      packageKind <- detectPackageKindForPackage checkName
      pure (validateCPackageVmCheckSource packageKind checkName checkTemplatePath (T.unpack checkTemplateText))
validateCPackageVmCheckSource :: PackageKind -> FilePath -> FilePath -> String -> [String]
validateCPackageVmCheckSource packageKind checkName checkTemplatePath checkTemplateSource =
  let hasCanonicalNameBinding =
        "name = builtins.baseNameOf ./.;" `isInfixOf` checkTemplateSource
          || "name = baseNameOf ./.;" `isInfixOf` checkTemplateSource
      hasRunNixOSTest = "pkgs.testers.runNixOSTest" `isInfixOf` checkTemplateSource
      hasMachineNode = "nodes.machine" `isInfixOf` checkTemplateSource
      hasTestScript = "testScript = ''" `isInfixOf` checkTemplateSource
      hasSameNamePackageReference =
        "inputs.self.packages.${pkgs.stdenv.system}.${name}" `isInfixOf` checkTemplateSource
          || ("inputs.self.packages.${pkgs.stdenv.system}." ++ checkName) `isInfixOf` checkTemplateSource
   in catMaybes
        [ if packageKind == CPackage
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
inferTemplateSpec :: FilePath -> String -> IO (Maybe TemplateSpec)
inferTemplateSpec packageName nixSource =
  listToMaybe <$> filterM (\templateSpec -> templateMatches templateSpec packageName nixSource) templateSpecs
inferCheckTemplateSpec :: FilePath -> String -> IO (Maybe CheckTemplateSpec)
inferCheckTemplateSpec checkName nixSource =
  listToMaybe <$> filterM (\checkTemplateSpec -> checkTemplateMatches checkTemplateSpec checkName nixSource) checkTemplateSpecs
normalizeNixExpr :: Set.Set T.Text -> Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys (Fix (Compose (AnnUnit nixExprSpan expressionFunctor))) =
  let rebuiltExpressionFunctor = case expressionFunctor of
        NSet isRecursive bindings -> NSet isRecursive (normalizeNixBindings allowedNixDifferenceKeys bindings)
        NLet bindings body -> NLet (normalizeNixBindings allowedNixDifferenceKeys bindings) (normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys body)
        NAbs (ParamSet paramsEllipsis paramsAt params) body ->
          NAbs
            (ParamSet paramsEllipsis paramsAt (sortNixParams (filterIgnoredNixParams ignoredTopLevelFunctionParams params)))
            (normalizeNixExpr Set.empty allowedNixDifferenceKeys body)
        NAbs (Param paramName) body -> NAbs (Param paramName) (normalizeNixExpr Set.empty allowedNixDifferenceKeys body)
        otherNixExpr -> fmap (normalizeNixExpr Set.empty allowedNixDifferenceKeys) otherNixExpr
   in Fix (Compose (AnnUnit nixExprSpan rebuiltExpressionFunctor))
filterIgnoredNixParams :: Set.Set T.Text -> [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
filterIgnoredNixParams ignoredTopLevelFunctionParams =
  filter (\(VarName paramName, _) -> Set.notMember paramName ignoredTopLevelFunctionParams)
sortNixParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortNixParams = sortOn (\(VarName paramName, _) -> paramName)
normalizeNixBindings :: Set.Set T.Text -> [Binding NExprLoc] -> [Binding NExprLoc]
normalizeNixBindings allowedNixDifferenceKeys bindings =
  [normalizeNixBinding allowedNixDifferenceKeys binding | binding <- bindings, not (isAllowedNixDifferenceBinding allowedNixDifferenceKeys binding)]
normalizeNixBinding :: Set.Set T.Text -> Binding NExprLoc -> Binding NExprLoc
normalizeNixBinding allowedNixDifferenceKeys = \case
  NamedVar keyPath bindingValue sourcePosition -> NamedVar keyPath (normalizeNixExpr Set.empty allowedNixDifferenceKeys bindingValue) sourcePosition
  Inherit maybeBoundNixExpr inheritedNames sourcePosition -> Inherit (normalizeNixExpr Set.empty allowedNixDifferenceKeys <$> maybeBoundNixExpr) inheritedNames sourcePosition
isAllowedNixDifferenceBinding :: Set.Set T.Text -> Binding NExprLoc -> Bool
isAllowedNixDifferenceBinding allowedNixDifferenceKeys = \case
  NamedVar (bindingKey :| _) _ _ ->
    maybe False (`Set.member` allowedNixDifferenceKeys) (nixKeyNameText bindingKey)
  _ -> False
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
        Map.toList
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
  [ (bindingKey, normalizeRenderedNixBindingValue bindingKey (renderNixExpr bindingValue))
  | NamedVar keyPath bindingValue _ <- bindings,
    let bindingKey = T.intercalate "." (mapMaybe nixKeyNameText (NE.toList keyPath))
  ]
normalizeRenderedNixBindingValue :: T.Text -> T.Text -> T.Text
normalizeRenderedNixBindingValue bindingKey renderedBindingValue =
  let normalizedValue = T.pack (compactTextToSingleLine renderedBindingValue)
   in if bindingKey == "meta" then stripMetaDescriptionAssignment normalizedValue else normalizedValue
stripMetaDescriptionAssignment :: T.Text -> T.Text
stripMetaDescriptionAssignment renderedMetaValue =
  let trimmedValue = T.strip renderedMetaValue
      attrsetValue = fromMaybe trimmedValue (T.stripPrefix "meta = " trimmedValue)
   in case T.stripPrefix "{ description = " attrsetValue of
        Just descriptionPrefixRest ->
          case T.breakOn "; " descriptionPrefixRest of
            (descriptionValue, remainder) ->
              if not (T.null descriptionValue) && not (T.null remainder)
                then "{ " <> T.drop 2 remainder
                else renderedMetaValue
        Nothing -> renderedMetaValue
runPackageTests :: IO ()
runPackageTests = runPackageTestsWith hUnitPackageTests
runPackageTestsWithTimings :: FilePath -> IO ()
runPackageTestsWithTimings timingsPath = do
  writeFile timingsPath ""
  runPackageTestsWith (timeHUnitTests timingsPath hUnitPackageTests)
runPackageTestsWith :: Test -> IO ()
runPackageTestsWith packageTests = do
  hUnitCounts <- runTestTT packageTests
  if errors hUnitCounts == 0 && failures hUnitCounts == 0
    then putStrLn "test ... ok"
    else exitFailure
timeHUnitTests :: FilePath -> Test -> Test
timeHUnitTests timingsPath = \case
  TestLabel testName (TestCase testAction) -> TestLabel testName (TestCase (timeTestAction timingsPath testName testAction))
  TestLabel testName nestedTest -> TestLabel testName (timeHUnitTests timingsPath nestedTest)
  TestList nestedTests -> TestList (map (timeHUnitTests timingsPath) nestedTests)
  testCase -> testCase
timeTestAction :: FilePath -> String -> IO a -> IO a
timeTestAction timingsPath testName testAction = do
  startedAt <- getMonotonicTimeNSec
  testAction
    `finally` do
      finishedAt <- getMonotonicTimeNSec
      let elapsedSeconds = fromIntegral (finishedAt - startedAt) / 1000000000 :: Double
      appendFile timingsPath ("test\t" ++ showFFloat (Just 6) elapsedSeconds "" ++ "\t" ++ testName ++ "\n")
hUnitPackageTests :: Test
hUnitPackageTests =
  TestList
    [ TestLabel "Discovers conventional Haskell tests as behavioral specifications." (TestCase haskellTestDiscoveryTest),
      TestLabel "Ignores non-test Haskell strings and comments during discovery." (TestCase haskellTestDiscoveryFalsePositiveTest),
      TestLabel "Uses Python test docstrings as behavioral specifications." (TestCase pythonTestDiscoveryTest),
      TestLabel "Humanizes conventional test identifiers across frameworks." (TestCase testIdentifierSpecificationTest),
      TestLabel "Parses versioned coverage summaries strictly." (TestCase repositoryCoverageParsingTest),
      TestLabel "Renders stable text and JSON repository summaries." (TestCase repositorySummaryRenderingTest),
      TestLabel "Reports concise Nix template parameter differences." (TestCase nixTemplateParameterDifferenceTest),
      TestLabel "Accepts python_template without inputs or shellHook." (TestCase pythonTemplateOptionalInputsAndShellHookTest),
      TestLabel "Documents help and invokes it consistently." (TestCase commandLineHelpEndToEndTest),
      TestLabel "Rejects missing and unknown commands with usage on stderr." (TestCase invalidCommandEndToEndTest),
      TestLabel "Scaffolds a package and its requested checks from a nested directory." (TestCase addPackageEndToEndTest),
      TestLabel "Adds requested checks without changing an existing package." (TestCase addChecksToExistingPackageEndToEndTest),
      TestLabel "Validates existing packages before adding checks." (TestCase validateExistingPackageAddEndToEndTest),
      TestLabel "Rejects colliding check batches without partial output." (TestCase existingCheckCollisionEndToEndTest),
      TestLabel "Reports generated package behavior in the text summary." (TestCase textSummaryEndToEndTest),
      TestLabel "Reports generated package behavior in the JSON summary." (TestCase jsonSummaryEndToEndTest),
      TestLabel "Reports conventional tests for generated Haskell packages." (TestCase haskellSummaryEndToEndTest),
      TestLabel "Accepts a generated package during repository checks." (TestCase generatedPackageCheckEndToEndTest),
      TestLabel "Rejects unlabeled HUnit cases in generated Haskell packages." (TestCase unlabeledHaskellPackageCheckEndToEndTest),
      TestLabel "Reports the phase and file when a package check fails." (TestCase corruptedPackageCheckEndToEndTest),
      TestLabel "Rejects unknown package options without creating partial output." (TestCase unknownAddOptionEndToEndTest),
      TestLabel "Rejects package names that violate the package convention." (TestCase invalidPackageNameEndToEndTest),
      TestLabel "Rejects unsupported checks without creating partial output." (TestCase unsupportedPackageCheckEndToEndTest)
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
pythonTestDiscoveryTest =
  assertEqual
    "A test's first statement supplies its specification, with an identifier fallback."
    ["Handles a multiline definition.", "Undocumented behavior."]
    ( discoverPythonUnitTestNamesFromSource
        ( unlines
            [ "def test_undocumented_behavior() -> None:",
              "    assert True",
              "",
              "def test_multiline_definition(",
              "    value: str,",
              ") -> None:",
              "    \"\"\"Handles a multiline definition.\"\"\"",
              "    assert value"
            ]
        )
    )
testIdentifierSpecificationTest :: IO ()
testIdentifierSpecificationTest =
  assertEqual
    "Framework prefixes, snake case, and camel case do not leak into specifications."
    ["Canonicalization is idempotent.", "Strip empty lines matches filtered sequence.", "Attribute sets canonicalize by key order."]
    ( map
        testSpecificationFromIdentifier
        [ "test_property_canonicalization_is_idempotent",
          "quickcheck_strip_empty_lines_matches_filtered_sequence",
          "prop_attributeSetsCanonicalizeByKeyOrder"
        ]
    )
repositoryCoverageParsingTest :: IO ()
repositoryCoverageParsingTest = do
  assertEqual
    "Local flake evaluation uses Git filtering so ignored repository files are excluded."
    "git+file:///tmp/example-repository"
    (localGitFlakeReference "/tmp/example-repository")
  assertEqual
    "A valid coverage artifact preserves its labeled numerator and denominator."
    (Just (CoverageMeasurement LineCoverage 91 100))
    (parseRepositoryCoverageSummary "coverage-v1\tlines\t91\t100\n")
  forM_
    [ "coverage-v2\tlines\t91\t100",
      "coverage-v1\tunknown\t91\t100",
      "coverage-v1\tlines\t101\t100",
      "coverage-v1\tlines\t0\t0",
      "coverage-v1\tlines\tnan\t100"
    ]
    (assertEqual "Malformed coverage artifacts are rejected." Nothing . parseRepositoryCoverageSummary)
  assertEqual
    "A batched Nix result maps every requested check to its output path."
    (Just (Map.fromList [("demo-coverage", "/nix/store/demo"), ("sample_coverage", "/nix/store/sample")]))
    ( parseRepositoryCheckOutputPaths
        ["demo-coverage", "sample_coverage"]
        "demo-coverage\t/nix/store/demo\nsample_coverage\t/nix/store/sample"
    )
  assertEqual
    "Incomplete batched Nix results are rejected."
    Nothing
    (parseRepositoryCheckOutputPaths ["demo-coverage", "sample_coverage"] "demo-coverage\t/nix/store/demo")
  assertEqual
    "A valid profile artifact preserves total and per-test durations."
    (Just (RepositoryProfileMeasured (Duration 1.25) (Map.fromList [("Reports behavior.", Duration 0.125)])))
    (parseRepositoryProfileSummary "profile-v1\ttotal-seconds\t1.25\ntest\t0.125\tReports behavior.\n")
  assertEqual
    "A coverage timing artifact preserves per-test durations without a profile check."
    (Just (Map.fromList [("Reports behavior.", Duration 0.125)]))
    (parseRepositoryTestTimings "test\t0.125\tReports behavior.\n")
  forM_
    [ "profile-v1\ttotal-seconds\tNaN\ntest\t0.125\tReports behavior.\n",
      "profile-v1\ttotal-seconds\tInfinity\ntest\t0.125\tReports behavior.\n",
      "profile-v1\ttotal-seconds\t1.0\ntest\tNaN\tReports behavior.\n",
      "profile-v1\ttotal-seconds\t1.0\ntest\tInfinity\tReports behavior.\n"
    ]
    (assertEqual "Non-finite summary timings are rejected." Nothing . parseRepositoryProfileSummary)
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
            repositoryPackageTestNames = ["Reports \"quoted\" behavior."],
            repositoryPackageChecks =
              RepositoryPackageChecksSummary
                { repositoryPackageCoverage =
                    Just
                      ( RepositoryCoverageMeasured
                          (CoverageMeasurement StatementCoverage 19 20)
                          (Map.fromList [("Reports \"quoted\" behavior.", Duration 9.999)])
                      ),
                  repositoryPackageProfile = Just (RepositoryProfileMeasured (Duration 1.234) (Map.fromList [("Reports \"quoted\" behavior.", Duration 0.125)]))
                }
          }
      repositorySummary =
        RepositorySummary
          { repositorySummaryPath = "example.test/owner/demo",
            repositorySummaryPackages = [packageSummary]
          }
  assertEqual
    "Profile timings take precedence over coverage timings."
    (Map.fromList [("Reports \"quoted\" behavior.", Duration 0.125)])
    (repositoryPackageTestDurations packageSummary)
  assertEqual
    "Text rendering has stable fields, indentation, and fallbacks."
    ( unlines
        [ "repository: example.test/owner/demo",
          "packageName: demo",
          "packageType: python",
          "description: (none)",
          "     checks:",
          "             coverage (statements 19/20, 95.0%)",
          "             profile (all tests: 1.234s)",
          "      tests:",
          "             (0.125s) Reports \"quoted\" behavior.",
          ""
        ]
    )
    (renderRepositorySummariesText [repositorySummary])
  assertEqual
    "JSON rendering preserves the schema and escapes strings."
    ( unlines
        [ "{",
          "  \"repositories\": [",
          "    {",
          "      \"path\": \"example.test/owner/demo\",",
          "      \"packages\": [",
          "        {",
          "          \"name\": \"demo\",",
          "          \"packageType\": \"python\",",
          "          \"description\": null,",
          "          \"checks\": { \"coverage\": true, \"profile\": true },",
          "          \"coverage\": { \"status\": \"measured\", \"metric\": \"statements\", \"covered\": 19, \"total\": 20, \"percent\": 95.0 },",
          "          \"profile\": { \"status\": \"measured\", \"totalSeconds\": 1.234 },",
          "          \"testDurationsSeconds\": { \"Reports \\\"quoted\\\" behavior.\": 0.125 },",
          "          \"tests\": [\"Reports \\\"quoted\\\" behavior.\"]",
          "        }",
          "      ]",
          "    }",
          "  ]",
          "}"
        ]
    )
    (renderRepositorySummariesJson [repositorySummary])
  assertEqual
    "JSON string rendering escapes embedded newlines."
    "\"line one\\nline two\""
    (renderJsonString "line one\nline two")
commandLineHelpEndToEndTest :: IO ()
commandLineHelpEndToEndTest =
  withTemporaryPackageRepository "command-line-help" $ \temporaryDirectory -> do
    (helpExit, helpStdout, helpStderr) <- runEndToEndCommandIn temporaryDirectory ["-h"]
    assertEqual "The top-level help command succeeds." ExitSuccess helpExit
    assertBool
      "The top-level help command prints manual-style help to stdout."
      ( "GIT-REPOSITORY-CANONICALIZATION(1)" `isPrefixOf` helpStdout
          && "SYNOPSIS" `isInfixOf` helpStdout
          && "DESCRIPTION" `isInfixOf` helpStdout
          && "COMMANDS" `isInfixOf` helpStdout
      )
    assertEqual "The top-level help command leaves stderr empty." "" helpStderr
    (commandHelpExit, commandHelpStdout, commandHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["check", "--help"]
    assertEqual "A command-specific help request exits unsuccessfully." (ExitFailure 1) commandHelpExit
    assertEqual "A command-specific help request leaves stdout empty." "" commandHelpStdout
    assertEqual "A command-specific help request prints the main usage to stderr." mainUsageText commandHelpStderr
invalidCommandEndToEndTest :: IO ()
invalidCommandEndToEndTest =
  withTemporaryPackageRepository "invalid-command" $ \temporaryDirectory -> do
    (missingCommandExit, missingCommandStdout, missingCommandStderr) <- runEndToEndCommandIn temporaryDirectory []
    assertEqual "An omitted command exits unsuccessfully." (ExitFailure 1) missingCommandExit
    assertEqual "An omitted command leaves stdout empty." "" missingCommandStdout
    assertEqual "An omitted command prints the main usage to stderr." mainUsageText missingCommandStderr
    (invalidCommandExit, invalidCommandStdout, invalidCommandStderr) <- runEndToEndCommandIn temporaryDirectory ["unknown-command"]
    assertEqual "An invalid command uses Git's usage exit status." usageExitCode invalidCommandExit
    assertEqual "An invalid command leaves stdout empty." "" invalidCommandStdout
    assertEqual "An invalid command prints the main usage to stderr." mainUsageText invalidCommandStderr
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
        ["add", "python", "demo", "Demo package", "--coverage"]
    assertEqual "Adding a package through the installed CLI succeeds." ExitSuccess addExit
    assertEqual "A successful add produces no stdout." "" addStdout
    assertEqual "A successful add leaves stderr empty." "" addStderr
    generatedFilesExist <-
      and
        <$> mapM
          doesFileExist
          [ temporaryRepository </> "packages/demo/.gitignore",
            temporaryRepository </> "packages/demo/default.nix",
            temporaryRepository </> "packages/demo/main.py",
            temporaryRepository </> "checks/demo_coverage/default.nix"
          ]
    assertBool "The installed CLI creates the package and requested checks on disk." generatedFilesExist
addChecksToExistingPackageEndToEndTest :: IO ()
addChecksToExistingPackageEndToEndTest =
  withGeneratedHaskellPackageRepository "add-checks-to-existing-package" $ \temporaryRepository -> do
    let packageDefaultNixPath = temporaryRepository </> "packages/demo/default.nix"
        packageMainPath = temporaryRepository </> "packages/demo/Main.hs"
        profileCheckPath = temporaryRepository </> "checks/demo-profile/default.nix"
    packageDefaultNixBefore <- TIO.readFile packageDefaultNixPath
    packageMainBefore <- TIO.readFile packageMainPath
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "demo", "--profile"]
    assertEqual "Adding a check to an existing package succeeds." ExitSuccess addExit
    assertEqual "Successful check addition produces no stdout." "" addStdout
    assertEqual "Successful check addition leaves stderr empty." "" addStderr
    profileCheckExists <- doesFileExist profileCheckPath
    assertBool "The requested check is created." profileCheckExists
    packageDefaultNixAfter <- TIO.readFile packageDefaultNixPath
    packageMainAfter <- TIO.readFile packageMainPath
    assertEqual "The existing default.nix remains unchanged." packageDefaultNixBefore packageDefaultNixAfter
    assertEqual "The existing package source remains unchanged." packageMainBefore packageMainAfter
    (stagedExit, stagedStdout, stagedStderr) <-
      readProcessWithExitCode "git" ["-C", temporaryRepository, "diff", "--cached", "--name-only"] ""
    assertEqual "Inspecting staged paths succeeds." ExitSuccess stagedExit
    assertEqual "Only the generated check is staged." "checks/demo-profile/default.nix\n" stagedStdout
    assertEqual "Inspecting staged paths leaves stderr empty." "" stagedStderr
validateExistingPackageAddEndToEndTest :: IO ()
validateExistingPackageAddEndToEndTest =
  withGeneratedPythonPackageRepository "validate-existing-package-add" $ \temporaryRepository -> do
    (statusBeforeExit, statusBeforeStdout, statusBeforeStderr) <-
      readProcessWithExitCode "git" ["-C", temporaryRepository, "status", "--short"] ""
    assertEqual "Inspecting the initial repository status succeeds." ExitSuccess statusBeforeExit
    assertEqual "Inspecting the initial repository status leaves stderr empty." "" statusBeforeStderr
    let rejectedCommands :: [([String], String)]
        rejectedCommands =
          [ ( ["add", "demo"],
              "specify at least one check option"
            ),
            ( ["add", "missing", "--profile"],
              "package does not exist"
            ),
            ( ["add", "python", "demo", "Replacement description", "--profile"],
              "cannot set a description"
            ),
            ( ["add", "rust", "demo", "--profile"],
              "is a python package, not rust"
            )
          ]
    forM_ rejectedCommands $ \(arguments, expectedError) -> do
      (addExit, addStdout, addStderr) <- runEndToEndCommandIn temporaryRepository arguments
      assertEqual "Invalid existing-package augmentation fails." (ExitFailure 1) addExit
      assertEqual "Invalid existing-package augmentation produces no stdout." "" addStdout
      assertBool "Invalid existing-package augmentation reports its cause." (expectedError `isInfixOf` addStderr)
    profileCheckExists <- doesPathExist (temporaryRepository </> "checks/demo_profile")
    assertBool "Rejected augmentations create no check." (not profileCheckExists)
    (statusExit, statusStdout, statusStderr) <-
      readProcessWithExitCode "git" ["-C", temporaryRepository, "status", "--short"] ""
    assertEqual "Inspecting the repository status succeeds." ExitSuccess statusExit
    assertEqual "Rejected augmentations leave the repository unchanged." statusBeforeStdout statusStdout
    assertEqual "Inspecting the repository status leaves stderr empty." "" statusStderr
existingCheckCollisionEndToEndTest :: IO ()
existingCheckCollisionEndToEndTest =
  withGeneratedHaskellPackageRepository "existing-check-collision" $ \temporaryRepository -> do
    (setupExit, _setupStdout, setupStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "demo", "--coverage"]
    assertEqual ("Preparing the colliding check succeeds: " ++ setupStderr) ExitSuccess setupExit
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn
        temporaryRepository
        ["add", "demo", "--coverage", "--profile"]
    assertEqual "A colliding check batch fails." (ExitFailure 1) addExit
    assertEqual "A colliding check batch produces no stdout." "" addStdout
    assertBool
      "A colliding check batch reports the existing path."
      ("path already exists: checks/demo-coverage/default.nix" `isInfixOf` addStderr)
    profileCheckExists <- doesPathExist (temporaryRepository </> "checks/demo-profile")
    assertBool "A colliding check batch creates no partial check." (not profileCheckExists)
textSummaryEndToEndTest :: IO ()
textSummaryEndToEndTest =
  withGeneratedPythonPackageRepository "text-summary-end-to-end" $ \temporaryRepository -> do
    repositoryPath <- canonicalizePath temporaryRepository
    (summaryExit, summaryStdout, summaryStderr) <- runEndToEndCommandIn temporaryRepository ["summary"]
    assertEqual "Text summary succeeds for the generated repository." ExitSuccess summaryExit
    assertEqual
      "Text summary exactly reports generated metadata, checks, and discovered tests."
      ( renderRepositorySummariesText
          [RepositorySummary repositoryPath [expectedGeneratedPythonPackageSummary]]
      )
      summaryStdout
    assertEqual "A successful text summary leaves stderr empty." "" summaryStderr
jsonSummaryEndToEndTest :: IO ()
jsonSummaryEndToEndTest =
  withGeneratedPythonPackageRepository "json-summary-end-to-end" $ \temporaryRepository -> do
    repositoryPath <- canonicalizePath temporaryRepository
    (jsonExit, jsonStdout, jsonStderr) <- runEndToEndCommandIn temporaryRepository ["summary", "--json"]
    assertEqual "JSON summary succeeds for the generated repository." ExitSuccess jsonExit
    assertEqual
      "JSON summary exactly reports generated metadata, checks, and discovered tests."
      ( renderRepositorySummariesJson
          [RepositorySummary repositoryPath [expectedGeneratedPythonPackageSummary]]
      )
      jsonStdout
    assertEqual "A successful JSON summary leaves stderr empty." "" jsonStderr
haskellSummaryEndToEndTest :: IO ()
haskellSummaryEndToEndTest =
  withGeneratedHaskellPackageRepository "haskell-summary-end-to-end" $ \temporaryRepository -> do
    repositoryPath <- canonicalizePath temporaryRepository
    (summaryExit, summaryStdout, summaryStderr) <- runEndToEndCommandIn temporaryRepository ["summary"]
    assertEqual "A generated Haskell package summary succeeds." ExitSuccess summaryExit
    assertEqual
      "The summary reports its conventional HUnit label."
      ( renderRepositorySummariesText
          [RepositorySummary repositoryPath [expectedGeneratedHaskellPackageSummary]]
      )
      summaryStdout
    assertEqual "A successful Haskell summary leaves stderr empty." "" summaryStderr
generatedPackageCheckEndToEndTest :: IO ()
generatedPackageCheckEndToEndTest =
  withGeneratedPythonPackageRepository "generated-check-end-to-end" $ \temporaryRepository -> do
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check"]
    assertEqual "Checking the generated repository succeeds." ExitSuccess checkExit
    assertEqual "A successful check produces no stdout." "" checkStdout
    assertEqual "A successful check produces no stderr." "" checkStderr
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
      ( "git repository-canonicalization check failed at phase: file-compliance" `isInfixOf` failedCheckStderr
          && "packages/demo/default.nix:" `isInfixOf` failedCheckStderr
      )
unknownAddOptionEndToEndTest :: IO ()
unknownAddOptionEndToEndTest =
  withEmptyCanonicalRepository "unknown-add-option-end-to-end" $ \temporaryRepository -> do
    (unknownOptionExit, unknownOptionStdout, unknownOptionStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "python", "demo", "--unknown"]
    assertEqual "An unknown add option uses Git's usage exit status." usageExitCode unknownOptionExit
    assertEqual "An unknown add option leaves stdout empty." "" unknownOptionStdout
    assertBool "An unknown add option prints add usage to stderr." ("usage: git repository-canonicalization add" `isPrefixOf` unknownOptionStderr)
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
unsupportedPackageCheckEndToEndTest :: IO ()
unsupportedPackageCheckEndToEndTest =
  withEmptyCanonicalRepository "unsupported-package-check-end-to-end" $ \temporaryRepository -> do
    (unsupportedCheckExit, unsupportedCheckStdout, unsupportedCheckStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "html", "demo", "--coverage"]
    assertEqual "An unsupported check selection fails." (ExitFailure 1) unsupportedCheckExit
    assertEqual "An unsupported check selection leaves stdout empty." "" unsupportedCheckStdout
    assertBool "An unsupported check selection reports the rejected option." ("unsupported checks for package type html: --coverage" `isInfixOf` unsupportedCheckStderr)
    packageDirectoryExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "An unsupported check does not leave a partial package directory." (not packageDirectoryExists)
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
        ["add", "python", "demo", "Demo package", "--coverage"]
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
      repositoryPackageTestNames =
        [ "Canonical labels should only contain lowercase ASCII, digits, and hyphens.",
          "Canonicalizing twice should not change the result.",
          "Deduplicating canonical labels twice should be stable.",
          "Deduplication should keep first surviving canonical labels in order.",
          "Empty canonical labels should produce the fallback message.",
          "Equivalent spellings should collapse to the same label.",
          "The default message should summarize unique canonical labels.",
          "main() should emit the canonical label summary."
        ],
      repositoryPackageChecks =
        RepositoryPackageChecksSummary
          { repositoryPackageCoverage = Just (RepositoryCoverageUnavailable Map.empty),
            repositoryPackageProfile = Just RepositoryProfileUnavailable
          }
    }
expectedGeneratedHaskellPackageSummary :: RepositoryPackageSummary
expectedGeneratedHaskellPackageSummary =
  RepositoryPackageSummary
    { repositoryPackageName = "demo",
      repositoryPackageKind = HaskellPackage,
      repositoryPackageDescription = Just "Demo package",
      repositoryPackageTestNames = ["Renders the sample message."],
      repositoryPackageChecks =
        RepositoryPackageChecksSummary
          { repositoryPackageCoverage = Nothing,
            repositoryPackageProfile = Nothing
          }
    }
runEndToEndCommandIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runEndToEndCommandIn workingDirectory arguments =
  readProcessWithExitCode "git" (["-C", workingDirectory, "repository-canonicalization"] ++ arguments) ""
initializeGitRepositoryFixture :: FilePath -> IO ()
initializeGitRepositoryFixture repositoryPath =
  findExecutable "git" >>= \case
    Nothing -> assertFailure "git is required for command-line end-to-end tests"
    Just _ -> do
      (gitInitExit, _gitInitStdout, gitInitStderr) <- readProcessWithExitCode "git" ["init", "--quiet", repositoryPath] ""
      when (gitInitExit /= ExitSuccess) $
        assertFailure ("Failed to initialize Git repository fixture: " ++ gitInitStderr)
runGitFixtureCommand :: [String] -> IO ()
runGitFixtureCommand arguments = do
  (gitExit, _gitStdout, gitStderr) <- readProcessWithExitCode "git" arguments ""
  when (gitExit /= ExitSuccess) $
    assertFailure ("Git fixture command failed: git " ++ unwords arguments ++ if null gitStderr then "" else ": " ++ gitStderr)
withTemporaryPackageRepository :: String -> (FilePath -> IO a) -> IO a
withTemporaryPackageRepository tempDirName action = do
  temporaryDirectory <- getTemporaryDirectory
  (temporaryPath, temporaryHandle) <- openTempFile temporaryDirectory tempDirName
  hClose temporaryHandle
  removeFile temporaryPath
  createDirectoryIfMissing True temporaryPath
  action temporaryPath `finally` removePathForcibly temporaryPath
pythonGitignoreSource :: T.Text
pythonGitignoreSource =
  T.unlines
    [ "__pycache__/",
      ".mypy_cache/",
      ".ruff_cache/",
      "coverage/",
      "tmp/"
    ]
pythonLatexGitignoreSource :: T.Text
pythonLatexGitignoreSource = T.unlines ["tmp/"]
rustGitignoreSource :: T.Text
rustGitignoreSource =
  T.unlines
    [ "coverage/",
      "target/"
    ]
haskellGitignoreSource :: T.Text
haskellGitignoreSource =
  T.unlines
    [ "coverage/",
      ".direnv/",
      "tmp/"
    ]
htmlGitignoreSource :: T.Text
htmlGitignoreSource = T.unlines [".direnv/"]
cGitignoreSource :: T.Text
cGitignoreSource = T.unlines ["*.o"]
latexGitignoreSource :: T.Text
latexGitignoreSource =
  T.unlines
    [ "*.aux",
      "*.log",
      "*.pdf"
    ]
pythonMainSource :: T.Text
pythonMainSource =
  T.unlines
    [ "#!/usr/bin/env python3",
      "# ruff: noqa: S101",
      "\"\"\"Canonicalize labels into a stable, human-readable summary.\"\"\"",
      "",
      "from __future__ import annotations",
      "",
      "import contextlib",
      "import io",
      "import re",
      "",
      "from hypothesis import given",
      "from hypothesis import strategies as st",
      "",
      "DEFAULT_LABELS = [",
      "    \"Hello, World!\",",
      "    \"hello_world\",",
      "    \"HELLO   WORLD\",",
      "    \"Python-Template\",",
      "    \"python template\",",
      "]",
      "",
      "",
      "def canonicalize_label(label: str) -> str:",
      "    \"\"\"Collapse arbitrary label spellings to lowercase kebab-case.\"\"\"",
      "    normalized = re.sub(r\"[^0-9A-Za-z]+\", \"-\", label.casefold()).strip(\"-\")",
      "    return re.sub(r\"-{2,}\", \"-\", normalized)",
      "",
      "",
      "def unique_canonical_labels(labels: list[str]) -> list[str]:",
      "    \"\"\"Keep first occurrences after canonicalization.\"\"\"",
      "    seen: set[str] = set()",
      "    canonical_labels: list[str] = []",
      "    for label in labels:",
      "        canonical = canonicalize_label(label)",
      "        if canonical and canonical not in seen:",
      "            seen.add(canonical)",
      "            canonical_labels.append(canonical)",
      "    return canonical_labels",
      "",
      "",
      "def render_message(labels: list[str] | None = None) -> str:",
      "    \"\"\"Return a deterministic summary of canonical labels.\"\"\"",
      "    source_labels = DEFAULT_LABELS if labels is None else labels",
      "    canonical_labels = unique_canonical_labels(source_labels)",
      "    if not canonical_labels:",
      "        return \"No canonical labels\"",
      "    return \", \".join(canonical_labels)",
      "",
      "",
      "def main() -> None:",
      "    \"\"\"Run main.\"\"\"",
      "    print(render_message())  # noqa: T201",
      "",
      "",
      "def test_canonicalize_label_examples() -> None:",
      "    \"\"\"Equivalent spellings should collapse to the same label.\"\"\"",
      "    examples = [",
      "        \"Hello, World!\",",
      "        \"hello_world\",",
      "        \"HELLO   WORLD\",",
      "    ]",
      "    canonical = [canonicalize_label(example) for example in examples]",
      "    assert canonical == [\"hello-world\", \"hello-world\", \"hello-world\"]",
      "",
      "",
      "def test_render_message_uses_canonical_unique_labels() -> None:",
      "    \"\"\"The default message should summarize unique canonical labels.\"\"\"",
      "    assert render_message() == \"hello-world, python-template\"",
      "",
      "",
      "def test_render_message_reports_empty_result_when_no_labels_survive() -> None:",
      "    \"\"\"Empty canonical labels should produce the fallback message.\"\"\"",
      "    assert render_message([\"...\", \"   \", \"---\"]) == \"No canonical labels\"",
      "",
      "",
      "def test_unique_canonical_labels_keeps_first_surviving_occurrence() -> None:",
      "    \"\"\"Deduplication should keep first surviving canonical labels in order.\"\"\"",
      "    labels = [",
      "        \"Hello, World!\",",
      "        \"python template\",",
      "        \"---\",",
      "        \"hello_world\",",
      "        \"Python-Template\",",
      "        \"HELLO   WORLD\",",
      "    ]",
      "    assert unique_canonical_labels(labels) == [\"hello-world\", \"python-template\"]",
      "",
      "",
      "def test_main_prints_message() -> None:",
      "    \"\"\"main() should emit the canonical label summary.\"\"\"",
      "    output = io.StringIO()",
      "    with contextlib.redirect_stdout(output):",
      "        main()",
      "    assert output.getvalue().strip() == \"hello-world, python-template\"",
      "",
      "",
      "@given(st.text())  # type: ignore[untyped-decorator]",
      "def test_property_canonicalization_is_idempotent(label: str) -> None:",
      "    \"\"\"Canonicalizing twice should not change the result.\"\"\"",
      "    canonical = canonicalize_label(label)",
      "    assert canonicalize_label(canonical) == canonical",
      "",
      "",
      "@given(st.text())  # type: ignore[untyped-decorator]",
      "def test_property_canonicalization_uses_restricted_character_set(label: str) -> None:",
      "    \"\"\"Canonical labels should only contain lowercase ASCII, digits, and hyphens.\"\"\"",
      "    canonical = canonicalize_label(label)",
      "    assert not canonical or (",
      "        re.fullmatch(r\"[a-z0-9]+(?:-[a-z0-9]+)*\", canonical) is not None",
      "    )",
      "",
      "",
      "@given(st.lists(st.text(), max_size=25))  # type: ignore[untyped-decorator]",
      "def test_property_unique_canonical_labels_is_idempotent(labels: list[str]) -> None:",
      "    \"\"\"Deduplicating canonical labels twice should be stable.\"\"\"",
      "    canonical_labels = unique_canonical_labels(labels)",
      "    assert unique_canonical_labels(canonical_labels) == canonical_labels",
      "",
      "",
      "if __name__ == \"__main__\":",
      "    main()"
    ]
pythonLatexMainSource :: T.Text
pythonLatexMainSource =
  T.unlines
    [ "#!/usr/bin/env python3",
      "# ruff: noqa: S101",
      "\"\"\"Generate Python-produced artifacts for a LaTeX build.\"\"\"",
      "",
      "from __future__ import annotations",
      "",
      "import math",
      "import re",
      "import tempfile",
      "from pathlib import Path",
      "from unittest import mock",
      "",
      "import matplotlib as mpl",
      "from hypothesis import given, settings",
      "from hypothesis import strategies as st",
      "",
      "mpl.use(\"Agg\")",
      "import matplotlib.pyplot as plt",
      "import pandas as pd",
      "",
      "DEFAULT_SAMPLES = [2.0, 3.5, 5.0, 9.5, 12.0]",
      "LATEX_ESCAPES = {",
      "    \"\\\\\": r\"\\textbackslash{}\",",
      "    \"&\": r\"\\&\",",
      "    \"%\": r\"\\%\",",
      "    \"$\": r\"\\$\",",
      "    \"#\": r\"\\#\",",
      "    \"_\": r\"\\_\",",
      "    \"{\": r\"\\{\",",
      "    \"}\": r\"\\}\",",
      "}",
      "",
      "",
      "def latex_escape(text: str) -> str:",
      "    \"\"\"Escape a narrow, deterministic subset of LaTeX-special characters.\"\"\"",
      "    return \"\".join(LATEX_ESCAPES.get(character, character) for character in text)",
      "",
      "",
      "def summarize_samples(samples: list[float]) -> list[tuple[str, float]]:",
      "    \"\"\"Return stable summary statistics for a non-empty sample list.\"\"\"",
      "    if not samples:",
      "        msg = \"samples must not be empty\"",
      "        raise ValueError(msg)",
      "    total = float(sum(samples))",
      "    count = float(len(samples))",
      "    mean = total / count",
      "    return [",
      "        (\"count\", count),",
      "        (\"total\", total),",
      "        (\"mean\", mean),",
      "        (\"min\", float(min(samples))),",
      "        (\"max\", float(max(samples))),",
      "    ]",
      "",
      "",
      "def build_table_frame(samples: list[float]) -> pd.DataFrame:",
      "    \"\"\"Represent summary statistics as a DataFrame for LaTeX emission.\"\"\"",
      "    rows = summarize_samples(samples)",
      "    return pd.DataFrame(rows, columns=[\"Metric\", \"Value\"])",
      "",
      "",
      "def render_table(samples: list[float]) -> str:",
      "    \"\"\"Render a LaTeX tabular with escaped metric names and fixed decimals.\"\"\"",
      "    frame = build_table_frame(samples)",
      "    lines = [",
      "        \"\\\\begin{tabular}{lr}\",",
      "        \"\\\\toprule\",",
      "        \"Metric & Value \\\\\\\\\",",
      "        \"\\\\midrule\",",
      "    ]",
      "    lines.extend(",
      "        f\"{latex_escape(str(row.Metric))} & {row.Value:.2f} \\\\\\\\\"",
      "        for row in frame.itertuples(index=False)",
      "    )",
      "    lines.extend([\"\\\\bottomrule\", \"\\\\end{tabular}\", \"\"])",
      "    return \"\\n\".join(lines)",
      "",
      "",
      "def create_figure(path: Path, samples: list[float]) -> None:",
      "    \"\"\"Create a deterministic figure for the LaTeX document.\"\"\"",
      "    figure, axis = plt.subplots(figsize=(5, 3))",
      "    x_values = list(range(1, len(samples) + 1))",
      "    axis.plot(x_values, samples, color=\"#1f77b4\", linewidth=2.5, marker=\"o\")",
      "    axis.set_xlabel(\"Sample index\")",
      "    axis.set_ylabel(\"Value\")",
      "    axis.set_title(\"Python-generated figure\")",
      "    axis.grid(alpha=0.3)",
      "    figure.tight_layout()",
      "    figure.savefig(path, dpi=200)",
      "    plt.close(figure)",
      "",
      "",
      "def create_table(path: Path, samples: list[float]) -> None:",
      "    \"\"\"Create a LaTeX table with pandas-backed summary statistics.\"\"\"",
      "    path.write_text(render_table(samples), encoding=\"utf-8\")",
      "",
      "",
      "def create_workspace_artifacts(",
      "    workspace: Path,",
      "    samples: list[float] | None = None,",
      ") -> None:",
      "    \"\"\"Generate all artifacts required by the LaTeX document.\"\"\"",
      "    resolved_samples = DEFAULT_SAMPLES if samples is None else samples",
      "    workspace.mkdir(parents=True, exist_ok=True)",
      "    create_figure(workspace / \"figure.png\", resolved_samples)",
      "    create_table(workspace / \"table.tex\", resolved_samples)",
      "",
      "",
      "def main() -> None:",
      "    \"\"\"Generate the build workspace artifacts for LaTeX compilation.\"\"\"",
      "    workspace = Path.cwd().resolve() / \"tmp\"",
      "    create_workspace_artifacts(workspace)",
      "",
      "",
      "def test_summarize_samples_contains_expected_metrics() -> None:",
      "    \"\"\"Summary statistics should remain stable for the default dataset.\"\"\"",
      "    summary = dict(summarize_samples(DEFAULT_SAMPLES))",
      "    expected = {",
      "        \"count\": 5.0,",
      "        \"total\": 32.0,",
      "        \"mean\": 6.4,",
      "        \"min\": 2.0,",
      "        \"max\": 12.0,",
      "    }",
      "    assert summary == expected",
      "",
      "",
      "def test_create_table_contains_expected_metrics() -> None:",
      "    \"\"\"Table output should expose the expected summary metrics and values.\"\"\"",
      "    with tempfile.TemporaryDirectory() as tmpdir:",
      "        table_path = Path(tmpdir) / \"table.tex\"",
      "        create_table(table_path, DEFAULT_SAMPLES)",
      "        table = table_path.read_text(encoding=\"utf-8\")",
      "        expected_fragments = [",
      "            \"\\\\begin{tabular}{lr}\",",
      "            \"Metric & Value \\\\\\\\\",",
      "            \"count & 5.00 \\\\\\\\\",",
      "            \"total & 32.00 \\\\\\\\\",",
      "            \"mean & 6.40 \\\\\\\\\",",
      "            \"min & 2.00 \\\\\\\\\",",
      "            \"max & 12.00 \\\\\\\\\",",
      "            \"\\\\end{tabular}\",",
      "        ]",
      "        for fragment in expected_fragments:",
      "            assert fragment in table",
      "",
      "",
      "def test_create_figure_writes_non_empty_png() -> None:",
      "    \"\"\"Figure generation should create a real PNG file.\"\"\"",
      "    with tempfile.TemporaryDirectory() as tmpdir:",
      "        figure_path = Path(tmpdir) / \"figure.png\"",
      "        create_figure(figure_path, DEFAULT_SAMPLES)",
      "        assert figure_path.exists()",
      "        assert figure_path.stat().st_size > 0",
      "",
      "",
      "def test_main_generates_workspace_artifacts_in_current_directory() -> None:",
      "    \"\"\"main() should write artifacts into <cwd>/tmp.\"\"\"",
      "    with tempfile.TemporaryDirectory() as tmpdir:",
      "        fake_cwd = Path(tmpdir)",
      "        with mock.patch(\"pathlib.Path.cwd\", return_value=fake_cwd):",
      "            main()",
      "        workspace = fake_cwd.resolve() / \"tmp\"",
      "        assert (workspace / \"figure.png\").exists()",
      "        assert (workspace / \"table.tex\").exists()",
      "",
      "",
      "@given(",
      "    st.lists(",
      "        st.floats(",
      "            min_value=-1_000,",
      "            max_value=1_000,",
      "            allow_nan=False,",
      "            allow_infinity=False,",
      "        ),",
      "        min_size=1,",
      "        max_size=25,",
      "    ),",
      ")  # type: ignore[untyped-decorator]",
      "@settings(deadline=None)  # type: ignore[untyped-decorator]",
      "def test_property_summary_is_permutation_invariant(samples: list[float]) -> None:",
      "    \"\"\"Aggregate statistics should not depend on sample ordering.\"\"\"",
      "    forward = summarize_samples(samples)",
      "    backward = summarize_samples(list(reversed(samples)))",
      "    assert len(forward) == len(backward)",
      "    for (left_metric, left_value), (right_metric, right_value) in zip(",
      "        forward,",
      "        backward,",
      "        strict=True,",
      "    ):",
      "        assert left_metric == right_metric",
      "        assert math.isclose(left_value, right_value, rel_tol=1e-9, abs_tol=1e-9)",
      "",
      "",
      "@given(st.text())  # type: ignore[untyped-decorator]",
      "def test_property_latex_escape_prefixes_special_characters(text: str) -> None:",
      "    \"\"\"Escaped output should prefix special characters that need escaping.\"\"\"",
      "    escaped = latex_escape(text)",
      "    for character in \"&%$#_\":",
      "        assert re.search(rf\"(?<!\\\\){re.escape(character)}\", escaped) is None",
      "",
      "",
      "@given(",
      "    st.lists(",
      "        st.text(",
      "            alphabet=\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-\",",
      "            min_size=1,",
      "            max_size=8,",
      "        ),",
      "        min_size=1,",
      "        max_size=5,",
      "    ),",
      ")  # type: ignore[untyped-decorator]",
      "@settings(deadline=None, max_examples=25)  # type: ignore[untyped-decorator]",
      "def test_property_workspace_artifacts_created_for_nested_paths(",
      "    path_segments: list[str],",
      ") -> None:",
      "    \"\"\"Artifact generation should work for nested workspace paths.\"\"\"",
      "    with tempfile.TemporaryDirectory() as tmpdir:",
      "        workspace = Path(tmpdir).joinpath(*path_segments)",
      "        create_workspace_artifacts(workspace)",
      "        figure_path = workspace / \"figure.png\"",
      "        table_path = workspace / \"table.tex\"",
      "        assert figure_path.exists()",
      "        assert figure_path.stat().st_size > 0",
      "        assert table_path.exists()",
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
      "module Main (main, runPackageTests, runPackageTestsWithTimings) where",
      "import Control.Exception (finally)",
      "import GHC.Clock (getMonotonicTimeNSec)",
      "import Numeric (showFFloat)",
      "import System.Exit (exitFailure)",
      "import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)",
      "",
      "renderMessage :: String",
      "renderMessage = \"Hello World Haskell\"",
      "",
      "runPackageTests :: IO ()",
      "runPackageTests = runPackageTestsWith hUnitPackageTests",
      "runPackageTestsWithTimings :: FilePath -> IO ()",
      "runPackageTestsWithTimings timingsPath = do",
      "  writeFile timingsPath \"\"",
      "  runPackageTestsWith (timeHUnitTests timingsPath hUnitPackageTests)",
      "runPackageTestsWith :: Test -> IO ()",
      "runPackageTestsWith packageTests = do",
      "  counts <- runTestTT packageTests",
      "  if errors counts == 0 && failures counts == 0",
      "    then putStrLn \"test ... ok\"",
      "    else exitFailure",
      "",
      "timeHUnitTests :: FilePath -> Test -> Test",
      "timeHUnitTests timingsPath (TestLabel testName (TestCase testAction)) = TestLabel testName (TestCase (timeTestAction timingsPath testName testAction))",
      "timeHUnitTests timingsPath (TestLabel testName nestedTest) = TestLabel testName (timeHUnitTests timingsPath nestedTest)",
      "timeHUnitTests timingsPath (TestList nestedTests) = TestList (map (timeHUnitTests timingsPath) nestedTests)",
      "timeHUnitTests _ testCase = testCase",
      "timeTestAction :: FilePath -> String -> IO a -> IO a",
      "timeTestAction timingsPath testName testAction = do",
      "  startedAt <- getMonotonicTimeNSec",
      "  testAction `finally` do",
      "    finishedAt <- getMonotonicTimeNSec",
      "    let elapsedSeconds = fromIntegral (finishedAt - startedAt) / 1000000000 :: Double",
      "    appendFile timingsPath (\"test\\t\" ++ showFFloat (Just 6) elapsedSeconds \"\" ++ \"\\t\" ++ testName ++ \"\\n\")",
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
      "use anyhow::{Context, Result};",
      "use ignore::WalkBuilder;",
      "use std::fs;",
      "use std::io::{BufRead, BufReader, Write};",
      "use std::path::Path;",
      "fn main() -> Result<()> {",
      "    let input_paths: Vec<String> = std::env::args().skip(1).collect();",
      "    if input_paths.is_empty() {",
      "        process_root_path(Path::new(\".\"));",
      "    } else {",
      "        for input_path in input_paths {",
      "            process_root_path(Path::new(&input_path));",
      "        }",
      "    }",
      "    Ok(())",
      "}",
      "fn process_root_path(root: &Path) {",
      "    let walker = WalkBuilder::new(root).require_git(false).build();",
      "    for result in walker {",
      "        match result {",
      "            Ok(entry) => {",
      "                let path = entry.path();",
      "                if path.is_file() {",
      "                    if let Err(e) = remove_empty_lines(path) {",
      "                        let path_display = path.display();",
      "                        eprintln!(\"Error processing {path_display}: {e}\");",
      "                    }",
      "                }",
      "            }",
      "            Err(err) => eprintln!(\"Error walking path: {err}\"),",
      "        }",
      "    }",
      "}",
      "fn remove_empty_lines(path: &Path) -> Result<()> {",
      "    let path_display = path.display();",
      "    let data = fs::read(path).with_context(|| format!(\"Failed to read file: {path_display}\"))?;",
      "    if content_inspector::inspect(&data).is_binary() {",
      "        return Ok(());",
      "    }",
      "    let output = strip_empty_lines_from_bytes(&data)?;",
      "    if output != data {",
      "        fs::write(path, output).with_context(|| format!(\"Failed to write file: {path_display}\"))?;",
      "    }",
      "    Ok(())",
      "}",
      "fn strip_empty_lines_from_bytes(data: &[u8]) -> Result<Vec<u8>> {",
      "    let reader = BufReader::new(data);",
      "    let mut output = Vec::new();",
      "    for line_result in reader.lines() {",
      "        let line = line_result?;",
      "        if !line.trim().is_empty() {",
      "            writeln!(output, \"{line}\")?;",
      "        }",
      "    }",
      "    Ok(output)",
      "}",
      "#[cfg(test)]",
      "mod tests {",
      "    use super::*;",
      "    use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};",
      "    use std::env;",
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
      "    fn test_process_root_path_removes_empty_lines_from_text_files() -> Result<()> {",
      "        use tempfile::tempdir;",
      "        let dir = tempdir()?;",
      "        let root = dir.path();",
      "        let file1_path = root.join(\"test.txt\");",
      "        fs::write(&file1_path, \"line1\\n\\nline2\\n   \\nline3\\n\")?;",
      "        process_root_path(root);",
      "        let content1 = fs::read_to_string(&file1_path)?;",
      "        assert_eq!(content1, \"line1\\nline2\\nline3\\n\");",
      "        Ok(())",
      "    }",
      "    #[test]",
      "    fn test_process_root_path_respects_gitignore_and_skips_binary_files() -> Result<()> {",
      "        use tempfile::tempdir;",
      "        let dir = tempdir()?;",
      "        let root = dir.path();",
      "        let gitignore_path = root.join(\".gitignore\");",
      "        fs::write(&gitignore_path, \"ignored.txt\\n\")?;",
      "        let ignored_path = root.join(\"ignored.txt\");",
      "        fs::write(&ignored_path, \"should be ignored\\n\\n\")?;",
      "        let binary_path = root.join(\"binary.bin\");",
      "        fs::write(&binary_path, [0, 15, 255, 0, 1, 2, 3])?;",
      "        process_root_path(root);",
      "        let content_ignored = fs::read_to_string(&ignored_path)?;",
      "        assert_eq!(content_ignored, \"should be ignored\\n\\n\");",
      "        let content_binary = fs::read(&binary_path)?;",
      "        assert_eq!(content_binary, vec![0, 15, 255, 0, 1, 2, 3]);",
      "        Ok(())",
      "    }",
      "    #[test]",
      "    fn test_main_processes_current_directory_when_no_args() -> Result<()> {",
      "        use tempfile::tempdir;",
      "        let dir = tempdir()?;",
      "        let root = dir.path();",
      "        let file_path = root.join(\"test.txt\");",
      "        fs::write(&file_path, \"line1\\n\\nline2\\n\")?;",
      "        let previous_dir = env::current_dir()?;",
      "        env::set_current_dir(root)?;",
      "        let result = main();",
      "        env::set_current_dir(previous_dir)?;",
      "        result?;",
      "        let content = fs::read_to_string(&file_path)?;",
      "        assert_eq!(content, \"line1\\nline2\\n\");",
      "        Ok(())",
      "    }",
      "    #[test]",
      "    fn quickcheck_strip_empty_lines_matches_filtered_sequence() {",
      "        fn property(lines: Vec<LogicalLine>) -> TestResult {",
      "            let input = render_lines(&lines);",
      "            match strip_empty_lines_from_bytes(&input) {",
      "                Ok(actual) => TestResult::from_bool(actual == expected_non_empty_lines(&lines)),",
      "                Err(_) => TestResult::error(\"strip_empty_lines_from_bytes returned an error\"),",
      "            }",
      "        }",
      "        QuickCheck::new()",
      "            .tests(100)",
      "            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);",
      "    }",
      "    #[test]",
      "    fn quickcheck_strip_empty_lines_is_idempotent() {",
      "        fn property(lines: Vec<LogicalLine>) -> TestResult {",
      "            let input = render_lines(&lines);",
      "            match strip_empty_lines_from_bytes(&input) {",
      "                Ok(first_pass) => match strip_empty_lines_from_bytes(&first_pass) {",
      "                    Ok(second_pass) => TestResult::from_bool(first_pass == second_pass),",
      "                    Err(_) => {",
      "                        TestResult::error(\"second strip_empty_lines_from_bytes returned an error\")",
      "                    }",
      "                },",
      "                Err(_) => TestResult::error(\"first strip_empty_lines_from_bytes returned an error\"),",
      "            }",
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
      "\tfont-family: sans-serif;",
      "\ttext-align: center;",
      "\tpadding-top: 50px;",
      "}"
    ]
cMainSource :: T.Text
cMainSource =
  T.unlines
    [ "#include <stdio.h>",
      "int main(void) {",
      "  printf(\"Hello World\\n\");",
      "  return 0;",
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
    [ "[[bin]]",
      "name = \"remove-empty-lines\"",
      "path = \"src/main.rs\"",
      "",
      "[dependencies]",
      "ignore = \"0.4\"",
      "anyhow = \"1.0\"",
      "content_inspector = \"0.2\"",
      "tempfile = \"3.8\"",
      "",
      "[lints.clippy]",
      "all = { level = \"deny\", priority = -1 }",
      "pedantic = { level = \"deny\", priority = -1 }",
      "nursery = { level = \"deny\", priority = -1 }",
      "cargo = { level = \"deny\", priority = -1 }",
      "",
      "[lints.rust]",
      "unsafe_code = \"forbid\"",
      "",
      "[package]",
      "name = \"remove-empty-lines\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      "description = \"A CLI tool to remove empty lines from text files.\"",
      "license = \"MIT\"",
      "repository = \"https://github.com/pbizopoulos/canonicalization\"",
      "readme = \"../../README\"",
      "keywords = [\"cleanup\", \"formatter\"]",
      "categories = [\"development-tools\"]",
      ""
    ]
rustCargoTomlBaseline :: T.Text
rustCargoTomlBaseline =
  T.unlines
    [ "[[bin]]",
      "name = \"rust-template\"",
      "path = \"src/main.rs\"",
      "",
      "[dependencies]",
      "colored = \"2.1.0\"",
      "",
      "[lints.clippy]",
      "all = {level = \"deny\", priority = -1}",
      "pedantic = {level = \"deny\", priority = -1}",
      "nursery = {level = \"deny\", priority = -1}",
      "cargo = {level = \"deny\", priority = -1}",
      "",
      "[lints.rust]",
      "unsafe_code = \"forbid\"",
      "",
      "[package]",
      "name = \"rust-template\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      "description = \"A Rust template project.\"",
      "license = \"MIT\"",
      "repository = \"https://github.com/pbizopoulos/canonicalization\"",
      "readme = \"../../README\"",
      "keywords = [\"template\"]",
      "categories = [\"development-tools\"]",
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
      "  ghc-options:   -O2 -Weverything -Werror -threaded",
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
      "  ghcForTests = pkgs.haskellPackages.ghcWithPackages (_: executableHaskellDepends);",
      "in",
      "pkgs.haskellPackages.mkDerivation rec {",
      "  inherit executableHaskellDepends;",
      "  executableToolDepends = [",
      "    pkgs.makeWrapper",
      "  ];",
      "  mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  postInstall = ''",
      "    wrapProgram $out/bin/${pname} --run \"rm -f tmp/${pname}.tix\" --set-default HPCTIXFILE tmp/${pname}.tix",
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
      "pkgs.rustPlatform.buildRustPackage rec {",
      "  cargoHash = \"sha256-5FZKAFwP3QKw6KDiJsshJXkpU9jbUCeQStsTAkIfOjA=\";",
      "  doInstallCheck = pkgs.stdenv.isLinux;",
      "  env = {",
      "    RUSTDOCFLAGS = \"-D warnings\";",
      "    RUSTFLAGS = \"-D warnings\";",
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
      "  pname = baseNameOf ./.;",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
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
      "    install -Dm755 ${pname} $out/bin/${pname}",
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
      "    install -Dm644 ms.pdf $out/ms.pdf",
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
      "    cp -r ${../..}/. \"$workdir/\"",
      "    chmod -R u+w \"$workdir\"",
      "    rm -rf \"$workdir/packages/${name}/.terraform\" \"$workdir/packages/${name}/.terraform.lock.hcl\"",
      "    tofu -chdir=\"$workdir/packages/${name}\" init -reconfigure",
      "    tofu -chdir=\"$workdir/packages/${name}\" apply",
      "  '';",
      "}",
      ""
    ]
pythonPyPITemplateBaselineNixSource :: T.Text
pythonPyPITemplateBaselineNixSource = pythonPyPITemplateBaselineNixSourceWith defaultPythonPackageAttribute
pythonPyPITemplateBaselineNixSourceWith :: String -> T.Text
pythonPyPITemplateBaselineNixSourceWith pythonPackageAttribute =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      T.pack ("  python = pkgs." ++ pythonPackageAttribute ++ ";"),
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
pythonPyPIApplicationTemplateBaselineNixSource = pythonPyPIApplicationTemplateBaselineNixSourceWith defaultPythonPackageAttribute
pythonPyPIApplicationTemplateBaselineNixSourceWith :: String -> T.Text
pythonPyPIApplicationTemplateBaselineNixSourceWith pythonPackageAttribute =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      T.pack ("  python = pkgs." ++ pythonPackageAttribute ++ ";"),
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
      "    install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname} $out/bin/${pname}",
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
pythonLatexTemplateBaselineNixSource :: T.Text
pythonLatexTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python3;",
      "  pythonDeps = [",
      "    python.pkgs.matplotlib",
      "    python.pkgs.pandas",
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
      "    install -Dm644 main.py ms.tex ms.bib -t \"$datadir\"",
      "    install -Dm644 tmp/ms.pdf \"$out/ms.pdf\"",
      "    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py",
      "  '';",
      "  meta = {",
      "    description = \"A Python and LaTeX template package.\";",
      "    mainProgram = pname;",
      "  };",
      "  nativeBuildInputs = [ pkgs.texliveFull ];",
      "  passthru = {",
      "    inherit python;",
      "  };",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = pythonDeps ++ [",
      "    python.pkgs.hypothesis",
      "    python.pkgs.pytest",
      "  ];",
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
      "  checkName = builtins.baseNameOf ./.;",
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
      "    mkdir -p \"$out/html\" \"$workspace/coverage\" \"$workspace/hpc\"",
      "    cd \"$workspace\"",
      "    cat > TestMain.hs <<EOF",
      "    module TestMain (main) where",
      "    import qualified Main as PackageMain",
      "    main :: IO ()",
      "    main = PackageMain.runPackageTests",
      "    EOF",
      "    ghc -fhpc -hpcdir \"$workspace/hpc\" -main-is TestMain.main \\",
      "      -i\"$src\" -outputdir \"$workspace\" -odir \"$workspace\" -hidir \"$workspace\" \\",
      "      -o \"${packageName}\" TestMain.hs \"$src/Main.hs\"",
      "    HPCTIXFILE=\"$workspace/coverage/${packageName}.tix\" \"./${packageName}\"",
      "    hpc markup \"$workspace/coverage/${packageName}.tix\" --hpcdir=\"$workspace/hpc\" --destdir=\"$out/html\"",
      "    hpc report \"$workspace/coverage/${packageName}.tix\" --hpcdir=\"$workspace/hpc\" | tee \"$out/report.txt\"",
      "    coverageCounts=\"$(sed -n 's/.*expressions used (\\([0-9][0-9]*\\)\\/\\([0-9][0-9]*\\)).*/\\1 \\2/p' \"$out/report.txt\")\"",
      "    read -r covered total <<< \"$coverageCounts\"",
      "    test -n \"$covered\" -a -n \"$total\"",
      "    printf 'coverage-v1\\texpressions\\t%s\\t%s\\n' \"$covered\" \"$total\" > \"$out/coverage-summary.tsv\"",
      "  ''"
    ]
haskellProfileCheckBaselineNixSource :: T.Text
haskellProfileCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = import (../.. + \"/packages/${packageName}/default.nix\") {",
      "    inherit pkgs;",
      "  };",
      "  packageName = pkgs.lib.removeSuffix \"-profile\" checkName;",
      "  profileGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);",
      "in",
      "pkgs.runCommand checkName",
      "  {",
      "    nativeBuildInputs = [",
      "      packageDrv",
      "      pkgs.git",
      "      pkgs.time",
      "      profileGhc",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    packageName=\"${packageName}\"",
      "    mkdir -p \"$out\" \"$workspace\"",
      "    cd \"$workspace\"",
      "    cat > \"$workspace/TestMain.hs\" <<EOF",
      "    module TestMain (main) where",
      "    import qualified Main as PackageMain",
      "    import System.Environment (getEnv)",
      "    main :: IO ()",
      "    main = getEnv \"PROFILE_TIMINGS_PATH\" >>= PackageMain.runPackageTestsWithTimings",
      "    EOF",
      "    \"${profileGhc}/bin/ghc\" \\",
      "      -prof \\",
      "      -fprof-auto \\",
      "      -rtsopts \\",
      "      -O2 \\",
      "      -main-is TestMain.main \\",
      "      -i\"$src\" \\",
      "      -outputdir \"$workspace\" \\",
      "      -odir \"$workspace\" \\",
      "      -hidir \"$workspace\" \\",
      "      -o \"$workspace/$packageName\" \\",
      "      \"$workspace/TestMain.hs\" \\",
      "      \"$src/Main.hs\"",
      "    PROFILE_TIMINGS_PATH=\"$out/test-timings.tsv\" ${pkgs.time}/bin/time -f %e -o \"$out/total-seconds\" \\",
      "      \"$workspace/$packageName\" +RTS -p -RTS",
      "    mv \"$workspace/$packageName.prof\" \"$out/report.prof\"",
      "    printf 'profile-v1\\ttotal-seconds\\t%s\\n' \"$(cat \"$out/total-seconds\")\" > \"$out/profile-summary.tsv\"",
      "    cat \"$out/test-timings.tsv\" >> \"$out/profile-summary.tsv\"",
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
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"_coverage\" checkName;",
      "  profilingDrv = pkgs.callPackage (",
      "    (inputs.canonicalization or inputs.self) + \"/packages/pytest_profiling\"",
      "  ) { };",
      "  pythonEnv = packageDrv.python.withPackages (",
      "    _:",
      "    packageDrv.propagatedBuildInputs",
      "    ++ [",
      "      packageDrv.python.pkgs.pytest-cov",
      "      profilingDrv",
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
      "    python -m pytest -p no:cacheprovider --profile --pstats-dir \"$out/prof\" --cov=\"$src\" --cov-report term --cov-report \"json:$out/report.json\" --cov-report \"html:$out/html\" --junitxml=\"$out/junit.xml\" \"$src/main.py\"",
      "    python - \"$src/main.py\" \"$out/report.json\" \"$out/junit.xml\" \"$out/coverage-summary.tsv\" \"$out/test-timings.tsv\" \"$out/prof/combined.prof\" \"$out/profile-report.txt\" \"$out/profile-summary.tsv\" <<'PY'",
      "    import ast",
      "    import json",
      "    import pathlib",
      "    import pstats",
      "    import re",
      "    import sys",
      "    import xml.etree.ElementTree as ET",
      "    source_path, report_path, junit_path, coverage_path, timings_path, profile_path, profile_report_path, profile_summary_path = map(pathlib.Path, sys.argv[1:])",
      "    totals = json.loads(report_path.read_text())[\"totals\"]",
      "    coverage_path.write_text(",
      "        f\"coverage-v1\\tstatements\\t{totals['covered_lines']}\\t{totals['num_statements']}\\n\"",
      "    )",
      "    specifications = {}",
      "    for node in ast.parse(source_path.read_text()).body:",
      "        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name.startswith(\"test_\"):",
      "            words = re.sub(r\"^test_(?:property_)?\", \"\", node.name).replace(\"_\", \" \")",
      "            specifications[node.name] = ast.get_docstring(node) or words[:1].upper() + words[1:] + \".\"",
      "    timing_lines = []",
      "    for test_case in sorted(ET.parse(junit_path).iter(\"testcase\"), key=lambda element: element.attrib[\"name\"]):",
      "        test_name = test_case.attrib[\"name\"].split(\"[\", 1)[0]",
      "        timing_lines.append(f\"test\\t{test_case.attrib['time']}\\t{specifications[test_name]}\")",
      "    timings_path.write_text(\"\\n\".join(timing_lines) + \"\\n\")",
      "    with profile_report_path.open(\"w\") as stream:",
      "        profile_stats = pstats.Stats(str(profile_path), stream=stream)",
      "        profile_stats.sort_stats(\"cumulative\").print_stats(20)",
      "    profile_summary_path.write_text(",
      "        \"\\n\".join([f\"profile-v1\\ttotal-seconds\\t{profile_stats.total_tt}\", *timing_lines]) + \"\\n\"",
      "    )",
      "    PY",
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
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"-coverage\" checkName;",
      "  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = rustBaseInputs ++ [",
      "      pkgs.cargo-llvm-cov",
      "      pkgs.jq",
      "      pkgs.llvmPackages.llvm",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm \"llvm-cov\"}'",
      "    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm \"llvm-profdata\"}'",
      "    workspace=\"$PWD/workspace\"",
      "    cp -R --no-preserve=mode \"$src\" \"$workspace\"",
      "    install -Dm644 \"${cargoDeps}/.cargo/config.toml\" \"$workspace/.cargo/config.toml\"",
      "    substituteInPlace \"$workspace/.cargo/config.toml\" \\",
      "      --replace-fail \"@vendor@\" \"${cargoDeps}\"",
      "    cd \"$workspace\"",
      "    mkdir -p \"$out\"",
      "    cargo llvm-cov --json --summary-only --output-path \"$out/report.json\"",
      "    covered=\"$(jq -r '.data[0].totals.lines.covered' \"$out/report.json\")\"",
      "    total=\"$(jq -r '.data[0].totals.lines.count' \"$out/report.json\")\"",
      "    test \"$covered\" != null -a \"$total\" != null",
      "    printf 'coverage-v1\\tlines\\t%s\\t%s\\n' \"$covered\" \"$total\" > \"$out/coverage-summary.tsv\"",
      "  ''"
    ]
rustProfileCheckBaselineNixSource :: T.Text
rustProfileCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  inherit (packageDrv) cargoDeps;",
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"-profile\" checkName;",
      "  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = rustBaseInputs ++ [",
      "      pkgs.cargo-nextest",
      "      pkgs.perf",
      "      pkgs.python3",
      "      pkgs.time",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    workspace=\"$PWD/workspace\"",
      "    cp -R --no-preserve=mode \"$src\" \"$workspace\"",
      "    install -Dm644 \"${cargoDeps}/.cargo/config.toml\" \"$workspace/.cargo/config.toml\"",
      "    substituteInPlace \"$workspace/.cargo/config.toml\" \\",
      "      --replace-fail \"@vendor@\" \"${cargoDeps}\"",
      "    mkdir -p \"$out\" \"$workspace/.config\"",
      "    cat > \"$workspace/.config/nextest.toml\" <<EOF",
      "    [profile.profile.junit]",
      "    path = \"$out/junit.xml\"",
      "    EOF",
      "    cd \"$workspace\"",
      "    cargo nextest list --profile profile >/dev/null",
      "    if perf stat -e cpu-clock true >/dev/null 2>&1; then",
      "      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o \"$out/total-seconds\" \\",
      "        perf record --no-buildid-mmap --call-graph dwarf -e cpu-clock -o \"$out/perf.data\" -- \\",
      "        cargo nextest run --profile profile",
      "      perf report --stdio -i \"$out/perf.data\" > \"$out/report.txt\"",
      "    else",
      "      echo \"perf is unavailable in this environment; timings were still recorded.\" > \"$out/report.txt\"",
      "      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o \"$out/total-seconds\" \\",
      "        cargo nextest run --profile profile",
      "    fi",
      "    python - \"$out/junit.xml\" \"$out/total-seconds\" \"$out/profile-summary.tsv\" <<'PY'",
      "    import pathlib",
      "    import re",
      "    import sys",
      "    import xml.etree.ElementTree as ET",
      "    junit_path, total_path, output_path = map(pathlib.Path, sys.argv[1:])",
      "    lines = [f\"profile-v1\\ttotal-seconds\\t{total_path.read_text().strip()}\"]",
      "    for test_case in sorted(ET.parse(junit_path).iter(\"testcase\"), key=lambda element: element.attrib[\"name\"]):",
      "        identifier = test_case.attrib[\"name\"].rsplit(\"::\", 1)[-1]",
      "        words = re.sub(r\"^(?:test|quickcheck)_\", \"\", identifier).replace(\"_\", \" \")",
      "        test_name = words[:1].upper() + words[1:] + \".\"",
      "        lines.append(f\"test\\t{test_case.attrib['time']}\\t{test_name}\")",
      "    output_path.write_text(\"\\n\".join(lines) + \"\\n\")",
      "    PY",
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
      "  name = builtins.baseNameOf ./.;",
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
      "  host = pkgs.lib.removeSuffix \"VmWithDisko\" (builtins.baseNameOf ./.);",
      "in",
      "pkgs.runCommand (builtins.baseNameOf ./.) {",
      "  buildInputs = [",
      "    inputs.self.nixosConfigurations.${host}.config.system.build.vmWithDisko",
      "  ];",
      "} \"touch $out\""
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
      "    install -Dm755 ${pname} $out/bin/${pname}",
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
