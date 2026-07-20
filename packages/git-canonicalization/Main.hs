{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main, runPackageEndToEndTests, runPackageTests) where
import Control.Applicative ((<|>))
import Control.Exception (IOException, finally, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isAsciiLower, isDigit)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, sort, sortBy, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getCurrentDirectory, getHomeDirectory, listDirectory, removeFile, removePathForcibly, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath ((<.>), (</>))
import System.FilePath.Posix (isRelative, makeRelative, splitDirectories, takeBaseName, takeDirectory, takeFileName)
import System.IO (hClose, hPutStr, hPutStrLn, openTempFile, stderr)
import System.Process (rawSystem, readProcessWithExitCode)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertBool, assertEqual, assertFailure, runTestTT)
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
    templateBaselineSource :: Maybe T.Text
  }
type CheckTemplateSpec :: Type
data CheckTemplateSpec = CheckTemplateSpec
  { checkTemplateName :: FilePath,
    checkTemplateMatches :: FilePath -> String -> IO Bool,
    checkTemplateAllowedDifferenceKeys :: Set.Set T.Text,
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
        templateBaselineSource = Just haskellTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        templateMatches = \_ nixSource -> pure ("rustPlatform.buildRustPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = Just rustTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "html_template",
        templateMatches = \_ nixSource -> pure ("writeShellApplication" `isInfixOf` nixSource && "http-server" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "text" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = Just htmlTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_latex_template",
        templateMatches = matchesPythonLatexTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["meta", "propagatedBuildInputs", "version"],
        templateBaselineSource = Just pythonLatexTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_application_template",
        templateMatches = matchesPythonPyPIApplicationTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        templateBaselineSource = Just pythonPyPIApplicationTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_pypi_template",
        templateMatches = matchesPythonPyPITemplate,
        templateAllowedDifferenceKeys = Set.fromList ["format", "installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        templateBaselineSource = Just pythonPyPITemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "binary_release_template",
        templateMatches = matchesBinaryReleaseTemplate,
        templateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "src", "version"],
        templateBaselineSource = Just binaryReleaseTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "python_template",
        templateMatches = \_ nixSource -> pure ("buildPythonPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"],
        templateBaselineSource = Just pythonTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "deploy_host_template",
        templateMatches = \_ nixSource ->
          pure
            ( "writeShellApplication" `isInfixOf` nixSource
                && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource)
            ),
        templateAllowedDifferenceKeys = Set.insert "meta.description" defaultAllowedNixDifferenceKeys,
        templateBaselineSource = Just deployHostTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "latex_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "latexmk -pdf ms.tex" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
        templateBaselineSource = Just latexTemplateBaselineNixSource
      },
    TemplateSpec
      { templateName = "c_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "cc -o ${pname} main.c -std=c89" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]),
        templateBaselineSource = Just cTemplateBaselineNixSource
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
        templateBaselineSource = Just uncommentTemplateBaselineNixSource
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
templateSpecByName :: FilePath -> Maybe TemplateSpec
templateSpecByName templateNameToFind = find ((== templateNameToFind) . templateName) templateSpecs
checkTemplateSpecs :: [CheckTemplateSpec]
checkTemplateSpecs =
  [ CheckTemplateSpec
      { checkTemplateName = "haskell_coverage_check",
        checkTemplateMatches = matchesHaskellCoverageCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = haskellCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "haskell_profile_check",
        checkTemplateMatches = matchesHaskellProfileCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = haskellProfileCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "haskell_property_testing_check",
        checkTemplateMatches = matchesHaskellPropertyTestingCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = haskellPropertyTestingCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "python_coverage_check",
        checkTemplateMatches = matchesPythonCoverageCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = pythonCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "python_profile_check",
        checkTemplateMatches = matchesPythonProfileCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = pythonProfileCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "python_property_testing_check",
        checkTemplateMatches = matchesPythonPropertyTestingCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = pythonPropertyTestingCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_coverage_check",
        checkTemplateMatches = matchesRustCoverageCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = rustCoverageCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_profile_check",
        checkTemplateMatches = matchesRustProfileCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = rustProfileCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_property_testing_check",
        checkTemplateMatches = matchesRustPropertyTestingCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = rustPropertyTestingCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "rust_mutation_testing_check",
        checkTemplateMatches = matchesRustMutationTestingCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = rustMutationTestingCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "html_template_check",
        checkTemplateMatches = matchesHtmlPackageDefaultCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = htmlTemplateCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "c_template_check",
        checkTemplateMatches = \checkName _ -> pure (checkName == "c_template"),
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "c_package_vm_check",
        checkTemplateMatches = matchesCPackageVmCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkTemplateComparisonMode = StructuralCPackageVm
      },
    CheckTemplateSpec
      { checkTemplateName = "default_vm_with_disko_check",
        checkTemplateMatches = matchesDefaultVmWithDiskoCheck,
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = defaultVmWithDiskoCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      },
    CheckTemplateSpec
      { checkTemplateName = "host_default_check",
        checkTemplateMatches = \checkName _ -> pure (checkName == "host_default"),
        checkTemplateAllowedDifferenceKeys = Set.empty,
        checkTemplateBaselineSource = hostDefaultCheckBaselineNixSource,
        checkTemplateComparisonMode = ExactCheckTemplate
      }
  ]
checkTemplateSpecByName :: FilePath -> Maybe CheckTemplateSpec
checkTemplateSpecByName checkTemplateNameToFind = find ((== checkTemplateNameToFind) . checkTemplateName) checkTemplateSpecs
matchesHaskellCoverageCheck :: FilePath -> String -> IO Bool
matchesHaskellCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["ghcWithPackages", "-fhpc"]
matchesHaskellProfileCheck :: FilePath -> String -> IO Bool
matchesHaskellProfileCheck = matchesCheckNameSuffixAndSourceContains "-profile" ["ghcWithPackages", "-fprof-auto"]
matchesHaskellPropertyTestingCheck :: FilePath -> String -> IO Bool
matchesHaskellPropertyTestingCheck = matchesCheckNameSuffixAndSourceContains "-property-testing" ["ghcWithPackages", "runPackageTests"]
matchesPythonCoverageCheck :: FilePath -> String -> IO Bool
matchesPythonCoverageCheck = matchesCheckNameSuffixAndSourceContains "_coverage" ["--cov=\"$src\""]
matchesPythonProfileCheck :: FilePath -> String -> IO Bool
matchesPythonProfileCheck = matchesCheckNameSuffixAndSourceContains "_profile" ["pyinstrument"]
matchesPythonPropertyTestingCheck :: FilePath -> String -> IO Bool
matchesPythonPropertyTestingCheck = matchesCheckNameSuffixAndSourceContains "_property_testing" ["python -m pytest -v main.py -k property"]
matchesRustCoverageCheck :: FilePath -> String -> IO Bool
matchesRustCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["cargo llvm-cov"]
matchesRustProfileCheck :: FilePath -> String -> IO Bool
matchesRustProfileCheck = matchesCheckNameSuffixAndSourceContains "-profile" ["pkgs.perf", "perf record"]
matchesRustPropertyTestingCheck :: FilePath -> String -> IO Bool
matchesRustPropertyTestingCheck = matchesCheckNameSuffixAndSourceContains "-property-testing" ["cargo test --locked"]
matchesRustMutationTestingCheck :: FilePath -> String -> IO Bool
matchesRustMutationTestingCheck = matchesCheckNameSuffixAndSourceContains "-mutation-testing" ["cargo mutants"]
matchesHtmlPackageDefaultCheck :: FilePath -> String -> IO Bool
matchesHtmlPackageDefaultCheck checkName nixSource = do
  packageKind <- detectPackageKindForPackage checkName
  pure
    ( packageKind == HtmlPackage
        && "pkgs.testers.runNixOSTest" `isInfixOf` nixSource
        && "pkgs.curl" `isInfixOf` nixSource
        && "Hello World!" `isInfixOf` nixSource
    )
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
type CheckOutcome :: Type
data CheckOutcome = CheckPassed | CheckFailed | CheckSkipped deriving stock (Eq, Show)
checkOutcomeFromIssues :: [a] -> CheckOutcome
checkOutcomeFromIssues = \case [] -> CheckPassed; _ -> CheckFailed
type PackageTest :: Type
data PackageTest = PackageTest String CheckOutcome [PackageTestCase]
type PackageTestCase :: Type
data PackageTestCase = PackageTestCase String CheckOutcome [String]
type PackageCheck :: Type
data PackageCheck = PackageCheck
  { packageCheckName :: String,
    packageCheckKind :: PackageKind,
    packageCheckTests :: [PackageTest],
    packageCheckIssues :: [String]
  }
type RepositoryCheckKind :: Type
data RepositoryCheckKind
  = RepositoryDefaultCheck
  | RepositoryCoverageCheck
  | RepositoryProfileCheck
  | RepositoryPropertyTestingCheck
  | RepositoryMutationTestingCheck
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
type RepositoryLocation :: Type
data RepositoryLocation = RepositoryLocation
  { repositoryLocationHostname :: String,
    repositoryLocationUsername :: String,
    repositoryLocationName :: FilePath,
    repositoryLocationUrl :: String
  }
  deriving stock (Eq, Show)
main :: IO ()
main = getArgs >>= runCli defaultCanonicalizationSettings
runCli :: CanonicalizationSettings -> [String] -> IO ()
runCli canonicalizationSettings commandLineArgs =
  let summarizeRepository render =
        collectRepositoryComplianceWith canonicalizationSettings >>= \case
          Left (checkPhaseName, checkPhaseIssues) -> do
            reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
            exitFailure
          Right (RepositoryComplianceSuccess packageNames checkNames) -> do
            let repositoryCheckNames = Set.fromList checkNames
            packageSummaries <- forM packageNames (summarizeRepositoryPackage repositoryCheckNames)
            putStr (render packageSummaries)
   in case commandLineArgs of
        [] -> printMainHelpAndExit (ExitFailure 1)
        ["-h"] -> printMainHelpAndExit ExitSuccess
        ["--help"] -> printMainHelpAndExit ExitSuccess
        ["help"] -> printMainHelpAndExit ExitSuccess
        _ | isHelpRequest commandLineArgs -> printCommandUsageAndExit ExitSuccess commandLineArgs
        _ -> case commandLineArgs of
          ["check", location] -> checkCanonicalizationLocation canonicalizationSettings location
          "check" : _ -> printCommandUsageAndExit usageExitCode commandLineArgs
          ["summary"] -> runInGitRepositoryRoot "." (summarizeRepository renderRepositoryPackageSummariesText)
          ["summary", "--json"] -> runInGitRepositoryRoot "." (summarizeRepository renderRepositoryPackageSummariesJson)
          ["add", repositoryUrl] ->
            case parseRepositoryUrl repositoryUrl of
              Just repositoryLocation -> addGitRepositoryFromLocation repositoryLocation
              Nothing -> printCommandUsageAndExit usageExitCode commandLineArgs
          ["init"] -> initializeHomeGitRepository
          commandArgs ->
            case commandArgs of
              "init" : _ -> printCommandUsageAndExit usageExitCode commandArgs
              "clone" : _ -> printCommandUsageAndExit usageExitCode commandArgs
              ["rm", removalTarget] ->
                removeCanonicalizationTarget removalTarget >>= \case
                  Left removeError -> do
                    hPutStrLn stderr removeError
                    exitFailure
                  Right () -> pure ()
              "rm" : _ -> printCommandUsageAndExit usageExitCode commandArgs
              _ ->
                case parseAddPackageArgs commandArgs of
                  Just (packageKindName, packageName, packageDescription, requestedCheckKinds) ->
                    runInGitRepositoryRoot "." $
                      case parseSupportedAddPackageKind packageKindName of
                        Nothing -> do
                          hPutStrLn stderr ("unsupported package type: " ++ packageKindName)
                          hPutStrLn stderr ("supported package types: " ++ intercalate ", " (map fst supportedAddPackageKinds))
                          exitFailure
                        Just packageKind -> do
                          addResult <- addPackageToCurrentRepositoryWith canonicalizationSettings packageKind packageName packageDescription requestedCheckKinds
                          case addResult of
                            Left addError -> do
                              hPutStrLn stderr addError
                              exitFailure
                            Right _ -> pure ()
                  Nothing -> printCommandUsageAndExit usageExitCode commandArgs
printMainHelpAndExit :: ExitCode -> IO a
printMainHelpAndExit exitCode = do
  if exitCode == ExitSuccess then putStr mainHelpText else hPutStr stderr mainHelpText
  exitWith exitCode
printCommandUsageAndExit :: ExitCode -> [String] -> IO a
printCommandUsageAndExit exitCode commandLineArgs = do
  if exitCode == ExitSuccess
    then putStr (usageTextForCommand (listToMaybe commandLineArgs))
    else hPutStr stderr (usageTextForCommand (listToMaybe commandLineArgs))
  exitWith exitCode
isHelpRequest :: [String] -> Bool
isHelpRequest = any (`elem` ["-h", "--help"])
usageExitCode :: ExitCode
usageExitCode = ExitFailure 129
mainHelpText :: String
mainHelpText =
  unlines
    [ "usage: git canonicalization [-h | --help] <command> [<args>]",
      "",
      "   add        Add a repository or a package and its requested checks",
      "   check      Check a canonicalization location",
      "   init       Initialize $HOME as a Git repository",
      "   rm         Remove a package or local repository submodule",
      "   summary    Show a summary of packages and checks",
      "",
      "See 'git canonicalization <command> -h' for help on a specific command.",
      ""
    ]
usageTextForCommand :: Maybe String -> String
usageTextForCommand = \case
  Just "add" ->
    unlines
      [ "usage: git canonicalization add <https-or-ssh-repository-url>",
        "   or: git canonicalization add [<options>] <package-type> <package-name> [<description>...]",
        "",
        "Repository URLs are added below $HOME as <hostname>/<username>/<repository-name>.",
        "",
        "    --default-check       add the package's default check",
        "    --coverage            add a coverage check",
        "    --profile             add a profiling check",
        "    --property-testing    add a property-testing check",
        "    --mutation-testing    add a mutation-testing check",
        ""
      ]
  Just "summary" ->
    unlines
      [ "usage: git canonicalization summary [--json]",
        "",
        "    --json                output the repository summary as JSON",
        ""
      ]
  Just "check" ->
    unlines
      [ "usage: git canonicalization check <location>",
        "",
        "Check the nearest Git repository. The repository rooted at $HOME uses",
        "the home .gitmodules path policy; other repositories use canonical checks.",
        ""
      ]
  Just "init" ->
    unlines
      [ "usage: git canonicalization init",
        "",
        "Initialize $HOME as a Git repository and ensure its .gitignore contains *.",
        ""
      ]
  Just "rm" ->
    unlines
      [ "usage: git canonicalization rm (<package-name> | <local-repository>)",
        "",
        "Remove a package and its corresponding checks from the current repository,",
        "or remove a local repository from the Git repository rooted at $HOME.",
        "Repository submodule entries in .gitmodules are removed and staged by git rm.",
        ""
      ]
  _ -> unlines ["usage: git canonicalization [-h | --help] <command> [<args>]", ""]
parseAddPackageArgs :: [String] -> Maybe (String, FilePath, Maybe String, Set.Set RepositoryCheckKind)
parseAddPackageArgs commandLineArgs = do
  (packageKindName, packageName, remainingArguments) <-
    case commandLineArgs of
      "add" : packageKindName : packageName : remainingArguments ->
        Just (packageKindName, packageName, remainingArguments)
      _ -> Nothing
  let (flagArguments, packageDescriptionArguments) =
        ( filter ("--" `isPrefixOf`) remainingArguments,
          filter (not . ("--" `isPrefixOf`)) remainingArguments
        )
      packageDescription = case packageDescriptionArguments of [] -> Nothing; args -> Just (unwords args)
  requestedCheckKinds <- parseRepositoryCheckFlags flagArguments
  pure (packageKindName, packageName, packageDescription, requestedCheckKinds)
parseRepositoryUrl :: String -> Maybe RepositoryLocation
parseRepositoryUrl repositoryUrl =
  parseHttpsRepositoryUrl repositoryUrl
    <|> parseSshSchemeRepositoryUrl repositoryUrl
    <|> parseScpRepositoryUrl repositoryUrl
parseHttpsRepositoryUrl :: String -> Maybe RepositoryLocation
parseHttpsRepositoryUrl repositoryUrl = do
  hostnameAndPath <- stripPrefix "https://" repositoryUrl
  repositoryLocationFromUrlPath repositoryUrl hostnameAndPath
parseSshSchemeRepositoryUrl :: String -> Maybe RepositoryLocation
parseSshSchemeRepositoryUrl repositoryUrl = do
  sshLocation <- stripPrefix "ssh://" repositoryUrl <|> stripPrefix "git+ssh://" repositoryUrl
  hostnameAndPath <- stripPrefix "git@" sshLocation
  repositoryLocationFromUrlPath repositoryUrl hostnameAndPath
parseScpRepositoryUrl :: String -> Maybe RepositoryLocation
parseScpRepositoryUrl repositoryUrl = do
  hostnameAndPath <- stripPrefix "git@" repositoryUrl
  let (hostname, colonAndPath) = break (== ':') hostnameAndPath
  repositoryPath <- stripPrefix ":" colonAndPath
  repositoryLocationFromUrlParts repositoryUrl hostname repositoryPath
repositoryLocationFromUrlPath :: String -> String -> Maybe RepositoryLocation
repositoryLocationFromUrlPath repositoryUrl hostnameAndPath =
  case T.splitOn "/" (T.pack hostnameAndPath) of
    hostname : repositoryPathParts -> repositoryLocationFromUrlParts repositoryUrl (T.unpack hostname) (T.unpack (T.intercalate "/" repositoryPathParts))
    [] -> Nothing
repositoryLocationFromUrlParts :: String -> String -> String -> Maybe RepositoryLocation
repositoryLocationFromUrlParts repositoryUrl hostname repositoryPath =
  case T.splitOn "/" (T.pack repositoryPath) of
    [username, repositoryNameWithSuffix] -> do
      let repositoryName = fromMaybe repositoryNameWithSuffix (T.stripSuffix ".git" repositoryNameWithSuffix)
          location = RepositoryLocation hostname (T.unpack username) (T.unpack repositoryName) repositoryUrl
      if any T.null [T.pack hostname, username, repositoryName]
        then Nothing
        else Just location
    _ -> Nothing
parseRepositoryCheckFlags :: [String] -> Maybe (Set.Set RepositoryCheckKind)
parseRepositoryCheckFlags flagArguments =
  Set.fromList
    <$> mapM
      ( \repositoryCheckFlag ->
          lookup
            repositoryCheckFlag
            [ ("--default-check", RepositoryDefaultCheck),
              ("--coverage", RepositoryCoverageCheck),
              ("--profile", RepositoryProfileCheck),
              ("--property-testing", RepositoryPropertyTestingCheck),
              ("--mutation-testing", RepositoryMutationTestingCheck)
            ]
      )
      flagArguments
runInGitRepositoryRoot :: FilePath -> IO a -> IO a
runInGitRepositoryRoot repositoryDirectory action = do
  canonicalRepositoryRoot <- discoverGitRepositoryRoot repositoryDirectory
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory canonicalRepositoryRoot
  action `finally` setCurrentDirectory previousDirectory
discoverGitRepositoryRoot :: FilePath -> IO FilePath
discoverGitRepositoryRoot repositoryDirectory = do
  (repositoryRootExit, repositoryRootStdout, repositoryRootStderr) <-
    readProcessWithExitCode "git" ["-C", repositoryDirectory, "rev-parse", "--show-toplevel"] ""
  if repositoryRootExit == ExitSuccess
    then canonicalizePath (T.unpack (T.strip (T.pack repositoryRootStdout)))
    else do
      putStr repositoryRootStdout
      hPutStr stderr repositoryRootStderr
      exitWith repositoryRootExit
checkCanonicalizationLocation :: CanonicalizationSettings -> FilePath -> IO ()
checkCanonicalizationLocation canonicalizationSettings location = do
  repositoryRoot <- discoverGitRepositoryRoot location
  homeDirectory <- getHomeDirectory
  canonicalHomeDirectory <- canonicalizePath homeDirectory
  if repositoryRoot == canonicalHomeDirectory
    then checkHomeGitmodules canonicalHomeDirectory >>= either failCanonicalizationCheck pure
    else
      runInGitRepositoryRoot repositoryRoot $
        collectRepositoryComplianceWith canonicalizationSettings >>= \case
          Left (checkPhaseName, checkPhaseIssues) -> do
            reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
            exitFailure
          Right _ -> pure ()
failCanonicalizationCheck :: String -> IO a
failCanonicalizationCheck diagnostic = do
  hPutStrLn stderr diagnostic
  exitFailure
checkHomeGitmodules :: FilePath -> IO (Either String ())
checkHomeGitmodules homeDirectory = do
  let gitmodulesPath = homeDirectory </> ".gitmodules"
  gitmodulesExists <- doesFileExist gitmodulesPath
  if not gitmodulesExists
    then pure (Left ("missing file: " ++ gitmodulesPath))
    else do
      readResult <- try (TIO.readFile gitmodulesPath)
      pure $
        case readResult of
          Left (readError :: IOException) -> Left ("failed to read " ++ gitmodulesPath ++ ": " ++ show readError)
          Right contents ->
            case filter (not . isCompatibleHomeGitmodulePath) (parseHomeGitmodulePathEntries contents) of
              [] -> Right ()
              invalidPathEntries ->
                Left
                  ( intercalate
                      "\n"
                      [ T.unpack pathEntry ++ ": must be exactly <host>/<owner>/<repo>"
                      | pathEntry <- invalidPathEntries
                      ]
                  )
parseHomeGitmodulePathEntries :: T.Text -> [T.Text]
parseHomeGitmodulePathEntries contents =
  foldl collectPathEntry [] (T.lines contents)
  where
    collectPathEntry pathEntries sourceLine =
      let (rawKey, rawValueWithEquals) = T.breakOn "=" (T.strip sourceLine)
          pathEntry = T.strip (T.drop 1 rawValueWithEquals)
       in if T.strip rawKey == "path"
            && not (T.null rawValueWithEquals)
            && not (T.null pathEntry)
            && pathEntry `notElem` pathEntries
            then pathEntries ++ [pathEntry]
            else pathEntries
isCompatibleHomeGitmodulePath :: T.Text -> Bool
isCompatibleHomeGitmodulePath pathEntry =
  not (T.isPrefixOf "/" pathEntry)
    && case T.splitOn "/" pathEntry of
      [hostname, username, repositoryName] ->
        all (\component -> not (T.null component) && component `notElem` [".", ".."]) [hostname, username, repositoryName]
      _ -> False
initializeHomeGitRepository :: IO ()
initializeHomeGitRepository = do
  homeDirectory <- getHomeDirectory
  let gitignorePath = homeDirectory </> ".gitignore"
  gitignoreExists <- doesFileExist gitignorePath
  when gitignoreExists $ do
    gitignoreContents <- TIO.readFile gitignorePath
    unless (homeGitignoreIsCompatible gitignoreContents) $
      failCanonicalizationCheck (gitignorePath ++ ": existing file must contain only *")
  gitInitExit <- rawSystem "git" ["init", homeDirectory]
  unless (gitInitExit == ExitSuccess) (exitWith gitInitExit)
  unless gitignoreExists (TIO.writeFile gitignorePath "*\n")
homeGitignoreIsCompatible :: T.Text -> Bool
homeGitignoreIsCompatible gitignoreContents = gitignoreContents == "*" || gitignoreContents == "*\n"
addGitRepositoryFromLocation :: RepositoryLocation -> IO ()
addGitRepositoryFromLocation repositoryLocation = do
  homeDirectory <- getHomeDirectory
  case gitSubmoduleAddArguments homeDirectory repositoryLocation of
    Left validationError -> failCanonicalizationCheck validationError
    Right gitArguments -> do
      gitAddExit <- rawSystem "git" gitArguments
      unless (gitAddExit == ExitSuccess) (exitWith gitAddExit)
gitSubmoduleAddArguments :: FilePath -> RepositoryLocation -> Either String [String]
gitSubmoduleAddArguments homeDirectory repositoryLocation =
  case catMaybes [validateNewName "hostname" hostname, validateNewName "username" username, validateNewName "repository" repositoryName] of
    validationError : _ -> Left validationError
    [] ->
      let repositoryPathEntry = hostname </> username </> repositoryName
          repositoryUrl = repositoryLocationUrl repositoryLocation
       in Right ["-C", homeDirectory, "submodule", "add", "--force", repositoryUrl, repositoryPathEntry]
  where
    hostname = repositoryLocationHostname repositoryLocation
    username = repositoryLocationUsername repositoryLocation
    repositoryName = repositoryLocationName repositoryLocation
removeCanonicalizationTarget :: FilePath -> IO (Either String ())
removeCanonicalizationTarget removalTarget = do
  targetIsGitRepository <- doesDirectoryExist (removalTarget </> ".git")
  targetHasGitFile <- doesFileExist (removalTarget </> ".git")
  if targetIsGitRepository || targetHasGitFile || any (`elem` ("/\\" :: String)) removalTarget
    then removeGitRepository removalTarget
    else runInGitRepositoryRoot "." (removePackageFromCurrentRepository removalTarget)
removeGitRepository :: FilePath -> IO (Either String ())
removeGitRepository repositoryPath = do
  homeDirectory <- getHomeDirectory
  removeGitRepositoryInHome homeDirectory repositoryPath
removeGitRepositoryInHome :: FilePath -> FilePath -> IO (Either String ())
removeGitRepositoryInHome homeDirectory repositoryPath = do
  repositoryPathExists <- doesPathExist repositoryPath
  if not repositoryPathExists
    then pure (Left ("path does not exist: " ++ repositoryPath))
    else do
      canonicalHomeDirectory <- canonicalizePath homeDirectory
      canonicalRepositoryPath <- canonicalizePath repositoryPath
      let repositoryPathEntry = makeRelative canonicalHomeDirectory canonicalRepositoryPath
          repositoryPathParts = splitDirectories repositoryPathEntry
          repositoryIsBelowHome = isRelative repositoryPathEntry && repositoryPathEntry /= "." && ".." `notElem` repositoryPathParts
      if not repositoryIsBelowHome
        then pure (Left ("repository is not below " ++ canonicalHomeDirectory ++ ": " ++ canonicalRepositoryPath))
        else do
          (homeGitRootExit, homeGitRootStdout, _homeGitRootStderr) <- readProcessWithExitCode "git" ["-C", canonicalHomeDirectory, "rev-parse", "--show-toplevel"] ""
          let reportedHomeGitRoot = T.unpack (T.strip (T.pack homeGitRootStdout))
          canonicalHomeGitRoot <- if homeGitRootExit == ExitSuccess then canonicalizePath reportedHomeGitRoot else pure reportedHomeGitRoot
          if homeGitRootExit /= ExitSuccess || canonicalHomeGitRoot /= canonicalHomeDirectory
            then pure (Left ("not a git repository root directory: " ++ canonicalHomeDirectory))
            else
              runRepositoryCreationCommand
                "could not remove repository submodule"
                ["-C", canonicalHomeDirectory, "rm", "--quiet", "--", repositoryPathEntry]
                >>= \case
                  Just removeError -> pure (Left removeError)
                  Nothing -> pure (Right ())
removePackageFromCurrentRepository :: FilePath -> IO (Either String ())
removePackageFromCurrentRepository packageName = do
  let packagePath = "packages" </> packageName
  packageExists <- doesDirectoryExist packagePath
  if not packageExists
    then pure (Left ("package does not exist: " ++ packageName))
    else do
      packageKind <- detectPackageKindForPackage packageName
      if packageKind == UnknownPackage
        then pure (Left ("could not determine package type: " ++ packageName))
        else do
          let checkKinds =
                [ RepositoryDefaultCheck,
                  RepositoryCoverageCheck,
                  RepositoryProfileCheck,
                  RepositoryPropertyTestingCheck,
                  RepositoryMutationTestingCheck
                ]
              associatedCheckPaths =
                [ "checks" </> checkName
                | checkKind <- checkKinds,
                  Just checkName <- [repositoryCheckNameForKind packageKind packageName checkKind]
                ]
          existingAssociatedCheckPaths <- filterM doesPathExist associatedCheckPaths
          let removalPaths = packagePath : existingAssociatedCheckPaths
          runRepositoryCreationCommand
            "could not remove package"
            (["rm", "-r", "--quiet", "--ignore-unmatch", "--"] ++ removalPaths)
            >>= \case
              Just removeError -> pure (Left removeError)
              Nothing -> do
                forM_ removalPaths $ \removalPath -> do
                  removalPathExists <- doesPathExist removalPath
                  when removalPathExists (removePathForcibly removalPath)
                pure (Right ())
runRepositoryCreationCommand :: String -> [String] -> IO (Maybe String)
runRepositoryCreationCommand errorContext = runRepositoryCreationProgram errorContext "git"
runRepositoryCreationProgram :: String -> FilePath -> [String] -> IO (Maybe String)
runRepositoryCreationProgram errorContext executable arguments = do
  (commandExit, commandStdout, commandStderr) <- readProcessWithExitCode executable arguments ""
  let commandDiagnostic = T.unpack (T.strip (T.pack (if null commandStderr then commandStdout else commandStderr)))
  pure $
    if commandExit == ExitSuccess
      then Nothing
      else Just (errorContext ++ if null commandDiagnostic then "" else ": " ++ commandDiagnostic)
requiredRepositoryRootFiles :: [FilePath]
requiredRepositoryRootFiles = ["flake.nix", "flake.lock"]
checkRequiredRepositoryRootFiles :: IO [String]
checkRequiredRepositoryRootFiles = do
  requiredFilePresence <- forM requiredRepositoryRootFiles $ \requiredFile -> do
    requiredFileExists <- doesFileExist requiredFile
    pure (requiredFile, requiredFileExists)
  pure
    [ "missing required file: " ++ requiredFile
    | (requiredFile, requiredFileExists) <- requiredFilePresence,
      not requiredFileExists
    ]
collectRepositoryComplianceWith :: CanonicalizationSettings -> IO (Either (String, [String]) RepositoryComplianceSuccess)
collectRepositoryComplianceWith canonicalizationSettings = do
  requiredRootFileIssues <- checkRequiredRepositoryRootFiles
  if null requiredRootFileIssues
    then collectRepositoryContentComplianceWith canonicalizationSettings
    else pure (Left ("required-root-files", requiredRootFileIssues))
collectRepositoryContentComplianceWith :: CanonicalizationSettings -> IO (Either (String, [String]) RepositoryComplianceSuccess)
collectRepositoryContentComplianceWith canonicalizationSettings = do
  repositoryStructureIssues <- checkRepositoryStructure
  if not (null repositoryStructureIssues)
    then pure (Left ("directory-structure", repositoryStructureIssues))
    else do
      packageNames <- listSubdirectoryNames "packages"
      packageChecks <- forM packageNames (checkPackageWith canonicalizationSettings [])
      checkNames <- listSubdirectoryNames "checks"
      checkComplianceIssues <- concat <$> forM checkNames checkTemplateWith
      let fileComplianceIssues = concatMap packageCheckIssues packageChecks ++ checkComplianceIssues
      if not (null fileComplianceIssues)
        then pure (Left ("file-compliance", fileComplianceIssues))
        else
          pure
            ( Right
                RepositoryComplianceSuccess
                  { repositoryCompliancePackageNames = packageNames,
                    repositoryComplianceCheckNames = checkNames
                  }
            )
reportCheckRepositoryFailures :: String -> [String] -> IO ()
reportCheckRepositoryFailures checkPhaseName checkPhaseIssues = do
  hPutStrLn stderr ("canonicalization check failed at phase: " ++ checkPhaseName)
  forM_ checkPhaseIssues $ \issue ->
    hPutStrLn stderr ("- [" ++ checkPhaseName ++ "] " ++ issue)
  case checkPhaseName of
    "required-root-files" ->
      hPutStrLn stderr "hint: add flake.nix and run 'nix flake update' to create or update flake.lock."
    "directory-structure" ->
      hPutStrLn stderr "hint: fix directory and required-file layout under packages/, hosts/, checks/, and repository root."
    "file-compliance" ->
      hPutStrLn stderr "hint: align package files with the expected internal templates and language-specific policy checks."
    _ -> pure ()
type RepositoryPackageChecksSummary :: Type
data RepositoryPackageChecksSummary = RepositoryPackageChecksSummary
  { repositoryPackageHasCheck :: Bool,
    repositoryPackageHasCoverageCheck :: Bool,
    repositoryPackageHasProfileCheck :: Bool,
    repositoryPackageHasPropertyTestingCheck :: Bool,
    repositoryPackageHasMutationTestingCheck :: Bool
  }
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
    repositoryPackageType :: String,
    repositoryPackageDescription :: Maybe String,
    repositoryPackageTestNames :: [String],
    repositoryPackageChecks :: RepositoryPackageChecksSummary
  }
renderRepositoryPackageSummariesText :: [RepositoryPackageSummary] -> String
renderRepositoryPackageSummariesText packageSummaries =
  intercalate
    "\n"
    [ unlines
        ( let packageChecks = repositoryPackageChecks packageSummary
           in [ renderRepositoryPackageFieldName "packageName" ++ " " ++ repositoryPackageName packageSummary,
                renderRepositoryPackageFieldName "packageType" ++ " " ++ repositoryPackageType packageSummary,
                renderRepositoryPackageFieldName "description" ++ " " ++ fromMaybe "(none)" (repositoryPackageDescription packageSummary),
                renderRepositoryPackageFieldName "checks"
              ]
                ++ case enabledRepositoryPackageCheckNames packageChecks of
                  [] -> [repositoryPackageValueIndent ++ "(none)"]
                  checkNames -> [repositoryPackageValueIndent ++ checkName | checkName <- checkNames]
                ++ [renderRepositoryPackageFieldName "tests"]
                ++ case repositoryPackageTestNames packageSummary of
                  [] -> [repositoryPackageValueIndent ++ "(none)"]
                  testNames -> [repositoryPackageValueIndent ++ testName | testName <- testNames]
        )
    | packageSummary <- packageSummaries
    ]
    ++ if null packageSummaries then "" else "\n"
renderRepositoryPackageFieldName :: String -> String
renderRepositoryPackageFieldName fieldName =
  replicate (length ("description" :: String) - length fieldName) ' ' ++ fieldName ++ ":"
repositoryPackageValueIndent :: String
repositoryPackageValueIndent = replicate (length ("description: " :: String)) ' '
renderRepositoryPackageSummariesJson :: [RepositoryPackageSummary] -> String
renderRepositoryPackageSummariesJson packageSummaries =
  "{\n"
    ++ "  \"packages\": [\n"
    ++ intercalate ",\n" [renderRepositoryPackageSummaryJson packageSummary | packageSummary <- packageSummaries]
    ++ "\n"
    ++ "  ]\n"
    ++ "}\n"
renderRepositoryPackageSummaryJson :: RepositoryPackageSummary -> String
renderRepositoryPackageSummaryJson packageSummary =
  unlines
    [ "    {",
      "      \"name\": " ++ renderJsonString (repositoryPackageName packageSummary) ++ ",",
      "      \"packageType\": " ++ renderJsonString (repositoryPackageType packageSummary) ++ ",",
      "      \"description\": " ++ maybe "null" renderJsonString (repositoryPackageDescription packageSummary) ++ ",",
      "      \"checks\": " ++ renderRepositoryPackageChecksJson (repositoryPackageChecks packageSummary) ++ ",",
      "      \"tests\": " ++ "[" ++ intercalate ", " (map renderJsonString (repositoryPackageTestNames packageSummary)) ++ "]",
      "    }"
    ]
renderRepositoryPackageChecksJson :: RepositoryPackageChecksSummary -> String
renderRepositoryPackageChecksJson packageChecks =
  "{ "
    ++ intercalate
      ", "
      [ "\"" ++ checkName ++ "\": " ++ bool "false" "true" isEnabled
      | (checkName, isEnabled) <- repositoryPackageCheckEntries packageChecks
      ]
    ++ " }"
repositoryPackageCheckEntries :: RepositoryPackageChecksSummary -> [(String, Bool)]
repositoryPackageCheckEntries packageChecks =
  [ ("default-check", repositoryPackageHasCheck packageChecks),
    ("coverage", repositoryPackageHasCoverageCheck packageChecks),
    ("profile", repositoryPackageHasProfileCheck packageChecks),
    ("property-testing", repositoryPackageHasPropertyTestingCheck packageChecks),
    ("mutation-testing", repositoryPackageHasMutationTestingCheck packageChecks)
  ]
enabledRepositoryPackageCheckNames :: RepositoryPackageChecksSummary -> [String]
enabledRepositoryPackageCheckNames = map fst . filter snd . repositoryPackageCheckEntries
summarizeRepositoryPackage :: Set.Set FilePath -> FilePath -> IO RepositoryPackageSummary
summarizeRepositoryPackage repositoryCheckNames packageName = do
  packageKind <- detectPackageKindForPackage packageName
  let packageRoot = "packages" </> packageName
  repositoryPackageDescriptionValue <-
    if packageKind `elem` [PythonPackage, PythonLatexPackage, PythonPyPIPackage]
      then do
        maybePyprojectTomlContents <- readTextFileIfExists (packageRoot </> "pyproject.toml")
        maybeDefaultNixContents <- readTextFileIfExists (packageRoot </> "default.nix")
        let maybePyprojectDescription = maybePyprojectTomlContents >>= extractPythonPackageDescriptionFromPyprojectToml
            maybeDefaultNixDescription = maybeDefaultNixContents >>= extractDefaultNixPackageDescription
        pure (maybePyprojectDescription <|> maybeDefaultNixDescription)
      else case packageKind of
        HaskellPackage -> do
          maybeCabalContents <- readTextFileIfExists (packageRoot </> (packageName <.> "cabal"))
          pure (extractHaskellPackageDescription =<< maybeCabalContents)
        RustPackage -> do
          maybeCargoTomlContents <- readTextFileIfExists (packageRoot </> "Cargo.toml")
          pure (extractRustPackageDescription =<< maybeCargoTomlContents)
        _ -> do
          maybeDefaultNixContents <- readTextFileIfExists (packageRoot </> "default.nix")
          pure (extractDefaultNixPackageDescription =<< maybeDefaultNixContents)
  repositoryPackageTestNamesValue <-
    if packageKind `elem` [PythonPackage, PythonLatexPackage]
      then do
        maybeMainPythonSourceText <- readTextFileIfExists (packageRoot </> "main.py")
        pure (maybe [] (discoverPythonUnitTestNamesFromSource . T.unpack) maybeMainPythonSourceText)
      else case packageKind of
        HaskellPackage -> do
          maybeMainHaskellSourceText <- readTextFileIfExists (packageRoot </> "Main.hs")
          pure (maybe [] (discoverHaskellUnitTestNamesFromSource . T.unpack) maybeMainHaskellSourceText)
        RustPackage -> do
          maybeMainRustSourceText <- readTextFileIfExists (packageRoot </> "src/main.rs")
          pure (maybe [] (discoverRustUnitTestNamesFromSource . T.unpack) maybeMainRustSourceText)
        _ -> pure []
  let repositoryPackageChecksValue =
        RepositoryPackageChecksSummary
          { repositoryPackageHasCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryDefaultCheck),
            repositoryPackageHasCoverageCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryCoverageCheck),
            repositoryPackageHasProfileCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryProfileCheck),
            repositoryPackageHasPropertyTestingCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryPropertyTestingCheck),
            repositoryPackageHasMutationTestingCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryMutationTestingCheck)
          }
  pure
    RepositoryPackageSummary
      { repositoryPackageName = packageName,
        repositoryPackageType = renderPackageKind packageKind,
        repositoryPackageDescription = repositoryPackageDescriptionValue,
        repositoryPackageTestNames = repositoryPackageTestNamesValue,
        repositoryPackageChecks = repositoryPackageChecksValue
      }
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
      case extractQuotedNixAssignmentValue "meta.description =" sourceLine of
        Just description -> Just (T.unpack description)
        Nothing ->
          if insideMetaBlock
            then case extractQuotedNixAssignmentValue "description =" sourceLine of
              Just description -> Just (T.unpack description)
              Nothing ->
                if "};" `T.isPrefixOf` T.strip sourceLine
                  then go False remainingLines
                  else go insideMetaBlock remainingLines
            else
              if "meta = {" `T.isPrefixOf` T.strip sourceLine
                then go True remainingLines
                else go insideMetaBlock remainingLines
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
    UnknownPackage -> "unknown"
supportedAddPackageKinds :: [(String, PackageKind)]
supportedAddPackageKinds =
  [ ("haskell", HaskellPackage),
    ("rust", RustPackage),
    ("html", HtmlPackage),
    ("python", PythonPackage),
    ("python-latex", PythonLatexPackage),
    ("c", CPackage),
    ("latex", LatexPackage)
  ]
parseSupportedAddPackageKind :: String -> Maybe PackageKind
parseSupportedAddPackageKind packageKindName = lookup packageKindName supportedAddPackageKinds
validatePackageNameForKind :: PackageKind -> FilePath -> Maybe String
validatePackageNameForKind packageKind packageName =
  case packageNameConventionForKind packageKind of
    Just (conventionName, separator) ->
      if isDelimitedLowercaseName separator packageName
        then Nothing
        else
          Just
            ( "package name must use "
                ++ conventionName
                ++ " for "
                ++ renderPackageKind packageKind
                ++ " packages"
            )
    Nothing -> validateNewName "package" packageName
packageNameConventionForKind :: PackageKind -> Maybe (String, Char)
packageNameConventionForKind packageKind =
  case packageKind of
    HaskellPackage -> Just ("kebab-case", '-')
    RustPackage -> Just ("kebab-case", '-')
    BinaryReleasePackage -> Just ("kebab-case", '-')
    HtmlPackage -> Just ("snake_case", '_')
    PythonLatexPackage -> Just ("snake_case", '_')
    PythonPackage -> Just ("snake_case", '_')
    PythonPyPIPackage -> Just ("snake_case", '_')
    CPackage -> Just ("snake_case", '_')
    TerraformPackage -> Just ("snake_case", '_')
    LatexPackage -> Just ("snake_case", '_')
    UnknownPackage -> Nothing
isDelimitedLowercaseName :: Char -> String -> Bool
isDelimitedLowercaseName separator packageName =
  case splitOnCharacter separator packageName of
    [] -> False
    nameParts -> all isValidNamePart nameParts
  where
    isValidNamePart :: String -> Bool
    isValidNamePart namePart =
      not (null namePart)
        && all (\character -> isAsciiLower character || isDigit character) namePart
splitOnCharacter :: Char -> String -> [String]
splitOnCharacter separator = go
  where
    go remainingValue =
      case break (== separator) remainingValue of
        (namePart, []) -> [namePart]
        (namePart, _ : remainingParts) -> namePart : go remainingParts
validateNewName :: String -> FilePath -> Maybe String
validateNewName nameKind name
  | null name = Just (nameKind ++ " name must not be empty")
  | name `elem` [".", ".."] = Just (nameKind ++ " name must not be '.' or '..'")
  | any isPathSeparator name = Just (nameKind ++ " name must not contain path separators")
  | not (all isAllowedNameCharacter name) = Just (nameKind ++ " name must contain only letters, digits, '.', '-', or '_'")
  | otherwise = Nothing
  where
    isAllowedNameCharacter character = isAlphaNum character || character `elem` ("._-" :: String)
    isPathSeparator character = character == '/' || character == '\\'
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
addPackageToCurrentRepositoryWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Maybe String -> Set.Set RepositoryCheckKind -> IO (Either String [FilePath])
addPackageToCurrentRepositoryWith canonicalizationSettings packageKind packageName packageDescription requestedCheckKinds =
  case validatePackageNameForKind packageKind packageName <|> validateRepositoryCheckSelection packageKind requestedCheckKinds of
    Just validationError -> pure (Left validationError)
    Nothing -> do
      let packageRootDirectory = "packages" </> packageName
          packageScaffoldFiles =
            [ RepositoryScaffoldFile
                (packageRootDirectory </> scaffoldFileRelativePath scaffoldFile)
                (scaffoldFileContents scaffoldFile)
            | scaffoldFile <- renderScaffoldFilesWith canonicalizationSettings packageKind packageName packageDescription
            ]
          checkScaffoldFiles = renderRepositoryCheckScaffoldFiles packageKind packageName requestedCheckKinds
          scaffoldFiles = packageScaffoldFiles ++ checkScaffoldFiles
          scaffoldPaths = map repositoryScaffoldFilePath scaffoldFiles
      if null packageScaffoldFiles
        then pure (Left ("unsupported package type: " ++ renderPackageKind packageKind))
        else do
          existingPaths <- filterM doesPathExist (packageRootDirectory : scaffoldPaths)
          case existingPaths of
            existingPath : _ -> pure (Left ("path already exists: " ++ existingPath))
            [] -> do
              forM_ scaffoldFiles $ \repositoryScaffoldFile -> do
                let absolutePath = repositoryScaffoldFilePath repositoryScaffoldFile
                createDirectoryIfMissing True (takeDirectory absolutePath)
                TIO.writeFile absolutePath (repositoryScaffoldFileContents repositoryScaffoldFile)
              pure (Right scaffoldPaths)
validateRepositoryCheckSelection :: PackageKind -> Set.Set RepositoryCheckKind -> Maybe String
validateRepositoryCheckSelection packageKind requestedCheckKinds =
  let unsupportedCheckKinds =
        sort
          [ requestedCheckKind
          | requestedCheckKind <- Set.toList requestedCheckKinds,
            isNothing (repositoryCheckNameForKind packageKind "example" requestedCheckKind)
          ]
   in case unsupportedCheckKinds of
        [] -> Nothing
        _ ->
          Just
            ( "unsupported checks for package type "
                ++ renderPackageKind packageKind
                ++ ": "
                ++ intercalate
                  ", "
                  [ case repositoryCheckKind of
                      RepositoryDefaultCheck -> "--default-check"
                      RepositoryCoverageCheck -> "--coverage"
                      RepositoryProfileCheck -> "--profile"
                      RepositoryPropertyTestingCheck -> "--property-testing"
                      RepositoryMutationTestingCheck -> "--mutation-testing"
                  | repositoryCheckKind <- unsupportedCheckKinds
                  ]
            )
renderRepositoryCheckScaffoldFiles :: PackageKind -> FilePath -> Set.Set RepositoryCheckKind -> [RepositoryScaffoldFile]
renderRepositoryCheckScaffoldFiles packageKind packageName requestedCheckKinds =
  catMaybes
    [ do
        checkName <- repositoryCheckNameForKind packageKind packageName requestedCheckKind
        checkTemplateSource <- repositoryCheckBaselineSource packageKind requestedCheckKind
        pure
          ( RepositoryScaffoldFile
              ("checks" </> checkName </> "default.nix")
              checkTemplateSource
          )
    | requestedCheckKind <- Set.toList requestedCheckKinds
    ]
repositoryCheckNameForKind :: PackageKind -> FilePath -> RepositoryCheckKind -> Maybe FilePath
repositoryCheckNameForKind packageKind packageName repositoryCheckKind =
  case (packageKind, repositoryCheckKind) of
    (HaskellPackage, RepositoryCoverageCheck) -> Just (packageName ++ "-coverage")
    (HaskellPackage, RepositoryProfileCheck) -> Just (packageName ++ "-profile")
    (HaskellPackage, RepositoryPropertyTestingCheck) -> Just (packageName ++ "-property-testing")
    (RustPackage, RepositoryCoverageCheck) -> Just (packageName ++ "-coverage")
    (RustPackage, RepositoryProfileCheck) -> Just (packageName ++ "-profile")
    (RustPackage, RepositoryPropertyTestingCheck) -> Just (packageName ++ "-property-testing")
    (RustPackage, RepositoryMutationTestingCheck) -> Just (packageName ++ "-mutation-testing")
    (PythonPackage, RepositoryCoverageCheck) -> Just (packageName ++ "_coverage")
    (PythonPackage, RepositoryProfileCheck) -> Just (packageName ++ "_profile")
    (PythonPackage, RepositoryPropertyTestingCheck) -> Just (packageName ++ "_property_testing")
    (PythonLatexPackage, RepositoryCoverageCheck) -> Just (packageName ++ "_coverage")
    (PythonLatexPackage, RepositoryProfileCheck) -> Just (packageName ++ "_profile")
    (PythonLatexPackage, RepositoryPropertyTestingCheck) -> Just (packageName ++ "_property_testing")
    (HtmlPackage, RepositoryDefaultCheck) -> Just packageName
    (CPackage, RepositoryDefaultCheck) -> Just packageName
    _ -> Nothing
repositoryCheckBaselineSource :: PackageKind -> RepositoryCheckKind -> Maybe T.Text
repositoryCheckBaselineSource packageKind repositoryCheckKind =
  case (packageKind, repositoryCheckKind) of
    (HaskellPackage, RepositoryCoverageCheck) -> Just haskellCoverageCheckBaselineNixSource
    (HaskellPackage, RepositoryProfileCheck) -> Just haskellProfileCheckBaselineNixSource
    (HaskellPackage, RepositoryPropertyTestingCheck) -> Just haskellPropertyTestingCheckBaselineNixSource
    (RustPackage, RepositoryCoverageCheck) -> Just rustCoverageCheckBaselineNixSource
    (RustPackage, RepositoryProfileCheck) -> Just rustProfileCheckBaselineNixSource
    (RustPackage, RepositoryPropertyTestingCheck) -> Just rustPropertyTestingCheckBaselineNixSource
    (RustPackage, RepositoryMutationTestingCheck) -> Just rustMutationTestingCheckBaselineNixSource
    (PythonPackage, RepositoryCoverageCheck) -> Just pythonCoverageCheckBaselineNixSource
    (PythonPackage, RepositoryProfileCheck) -> Just pythonProfileCheckBaselineNixSource
    (PythonPackage, RepositoryPropertyTestingCheck) -> Just pythonPropertyTestingCheckBaselineNixSource
    (PythonLatexPackage, RepositoryCoverageCheck) -> Just pythonCoverageCheckBaselineNixSource
    (PythonLatexPackage, RepositoryProfileCheck) -> Just pythonProfileCheckBaselineNixSource
    (PythonLatexPackage, RepositoryPropertyTestingCheck) -> Just pythonPropertyTestingCheckBaselineNixSource
    (HtmlPackage, RepositoryDefaultCheck) -> Just htmlTemplateCheckBaselineNixSource
    (CPackage, RepositoryDefaultCheck) -> Just cTemplateCheckBaselineNixSource
    _ -> Nothing
renderScaffoldFilesWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Maybe String -> [ScaffoldFile]
renderScaffoldFilesWith canonicalizationSettings packageKind packageName packageDescription =
  case packageKind of
    HaskellPackage ->
      [ ScaffoldFile ".gitignore" haskellGitignoreSource,
        ScaffoldFile "default.nix" haskellTemplateBaselineNixSource,
        ScaffoldFile "Main.hs" haskellMainSource,
        ScaffoldFile (packageName <.> "cabal") (renderScaffoldHaskellCabal packageName packageDescription)
      ]
    RustPackage ->
      [ ScaffoldFile ".gitignore" rustGitignoreSource,
        ScaffoldFile "default.nix" rustTemplateBaselineNixSource,
        ScaffoldFile "Cargo.toml" (renderScaffoldCargoToml packageName packageDescription),
        ScaffoldFile "src/main.rs" rustMainSource
      ]
    HtmlPackage ->
      [ ScaffoldFile ".gitignore" htmlGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultHtmlTemplateDescription packageDescription htmlTemplateBaselineNixSource),
        ScaffoldFile "index.html" htmlIndexSource,
        ScaffoldFile "script.js" htmlScriptSource,
        ScaffoldFile "style.css" htmlStyleSource
      ]
    PythonLatexPackage ->
      [ ScaffoldFile ".gitignore" pythonLatexGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultPythonLatexTemplateDescription packageDescription pythonLatexTemplateBaselineNixSource),
        ScaffoldFile "main.py" pythonLatexMainSource,
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
    PythonPackage ->
      [ ScaffoldFile ".gitignore" pythonGitignoreSource,
        ScaffoldFile "default.nix" (renderPythonTemplateBaselineNixSourceWith (scaffoldDescription defaultPythonTemplateDescription packageDescription) (canonicalizationPythonPackageAttribute canonicalizationSettings)),
        ScaffoldFile "main.py" pythonMainSource
      ]
    CPackage ->
      [ ScaffoldFile ".gitignore" cGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultCTemplateDescription packageDescription cTemplateBaselineNixSource),
        ScaffoldFile "main.c" cMainSource
      ]
    LatexPackage ->
      [ ScaffoldFile ".gitignore" latexGitignoreSource,
        ScaffoldFile "default.nix" (renderNixTemplateDescription defaultLatexTemplateDescription packageDescription latexTemplateBaselineNixSource),
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
    _ -> []
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
          [ allowedPathRegexesForPackageKind (packageRootPath packageInfo) (packageRootDirectoryName packageInfo) (detectedPackageKind packageInfo)
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
  | UnknownPackage
  deriving stock (Eq, Ord, Show)
type PackageInfo :: Type
data PackageInfo = PackageInfo
  { packageRootPath :: FilePath,
    packageRootDirectoryName :: FilePath,
    packageLeafPaths :: [FilePath],
    detectedPackageKind :: PackageKind,
    matchedPackageMarkers :: [String]
  }
buildPackageInfo :: Set.Set FilePath -> FilePath -> PackageInfo
buildPackageInfo leafPaths packageRootDirectory =
  let packageDirectoryName = takeBaseName packageRootDirectory
      packageRelativeLeafPaths =
        catMaybes
          [ stripPrefix (packageRootDirectory ++ "/") path
          | path <- Set.toList leafPaths,
            (packageRootDirectory ++ "/") `isPrefixOf` path
          ]
      markers = detectPackageMarkers packageRelativeLeafPaths
   in PackageInfo
        { packageRootPath = packageRootDirectory,
          packageRootDirectoryName = packageDirectoryName,
          packageLeafPaths = packageRelativeLeafPaths,
          detectedPackageKind = detectPackageKindFromMarkers markers,
          matchedPackageMarkers = map fst markers
        }
detectPackageMarkers :: [FilePath] -> [(String, PackageKind)]
detectPackageMarkers packageRelativeLeafPaths =
  let hasLeafPath leafPath = leafPath `elem` packageRelativeLeafPaths
      hasLeafPathWithPrefix pathPrefix = any (isPrefixOf pathPrefix) packageRelativeLeafPaths
   in catMaybes
        [ if hasLeafPath "Main.hs" then Just ("Main.hs", HaskellPackage) else Nothing,
          if hasLeafPath "Cargo.toml" then Just ("Cargo.toml", RustPackage) else Nothing,
          if hasLeafPath "index.html" then Just ("index.html", HtmlPackage) else Nothing,
          if hasLeafPath "main.py" && hasLeafPath "ms.tex" then Just ("main.py+ms.tex", PythonLatexPackage) else Nothing,
          if hasLeafPath "main.py" && not (hasLeafPath "ms.tex") then Just ("main.py", PythonPackage) else Nothing,
          if hasLeafPath "main.c" then Just ("main.c", CPackage) else Nothing,
          if hasLeafPath "main.tf" then Just ("main.tf", TerraformPackage) else Nothing,
          if hasLeafPath "ms.tex" && not (hasLeafPath "main.py") then Just ("ms.tex", LatexPackage) else Nothing,
          if hasLeafPathWithPrefix "Cargo.toml" then Nothing else if not (hasLeafPath "main.c") && not (hasLeafPath "Main.hs") && not (hasLeafPath "main.py") && not (hasLeafPath "index.html") && not (hasLeafPath "main.tf") && not (hasLeafPath "ms.tex") then Just ("binary-layout", BinaryReleasePackage) else Nothing
        ]
detectPackageKindFromMarkers :: [(String, PackageKind)] -> PackageKind
detectPackageKindFromMarkers markers =
  case [markerKind | (_, markerKind) <- markers, markerKind /= BinaryReleasePackage] of
    [markerKind] -> markerKind
    [] ->
      if any ((== BinaryReleasePackage) . snd) markers
        then BinaryReleasePackage
        else UnknownPackage
    _ -> UnknownPackage
ambiguousPackageMarkerIssuesForPackage :: PackageInfo -> [String]
ambiguousPackageMarkerIssuesForPackage packageInfo =
  [ packageRootPath packageInfo
      ++ ": has ambiguous project markers: "
      ++ intercalate ", " (matchedPackageMarkers packageInfo)
  | length (matchedPackageMarkers packageInfo) > 1
  ]
allowedPathRegexesForPackageKind :: FilePath -> FilePath -> PackageKind -> [String]
allowedPathRegexesForPackageKind packageRootDirectory packageDirectoryName packageKind =
  let escapedPackageRootDirectory = escapeRegexLiteral packageRootDirectory
      escapedPackageDirectoryName = escapeRegexLiteral packageDirectoryName
      basePackagePathRegexes = ["^" ++ escapedPackageRootDirectory ++ "/default\\.nix$", "^" ++ escapedPackageRootDirectory ++ "/\\.gitignore$"]
      withBasePackagePathRegexes additionalPathRegexes = basePackagePathRegexes ++ additionalPathRegexes
   in case packageKind of
        HaskellPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/Main\\.hs$", "^" ++ escapedPackageRootDirectory ++ "/" ++ escapedPackageDirectoryName ++ "\\.cabal$"]
        RustPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/Cargo\\.toml$", "^" ++ escapedPackageRootDirectory ++ "/Cargo\\.lock$", "^" ++ escapedPackageRootDirectory ++ "/src/main\\.rs$"]
        HtmlPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/index\\.html$", "^" ++ escapedPackageRootDirectory ++ "/script\\.js$", "^" ++ escapedPackageRootDirectory ++ "/style\\.css$"]
        PythonLatexPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.py$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.tex$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.bib$", "^" ++ escapedPackageRootDirectory ++ "/refs\\.bib$", "^" ++ escapedPackageRootDirectory ++ "/figures(/.*)?$"]
        PythonPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.py$"]
        PythonPyPIPackage -> basePackagePathRegexes
        CPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.c$"]
        TerraformPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/main\\.tf$", "^" ++ escapedPackageRootDirectory ++ "/\\.terraform(/.*)?$", "^" ++ escapedPackageRootDirectory ++ "/\\.terraform\\.lock\\.hcl$"]
        LatexPackage -> withBasePackagePathRegexes ["^" ++ escapedPackageRootDirectory ++ "/ms\\.tex$", "^" ++ escapedPackageRootDirectory ++ "/ms\\.bib$"]
        BinaryReleasePackage -> basePackagePathRegexes
        UnknownPackage -> basePackagePathRegexes
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
  keptChildren <- fmap catMaybes $
    forM childPaths $ \childPath -> do
      isDirectory <- doesDirectoryExist childPath
      let relativeChildPath = toRelativePath childPath
      case (isDirectory, shouldTraverseDirectory relativeChildPath) of
        (True, True) -> Just <$> collectRepositoryPaths childPath
        (True, False) -> pure Nothing
        (False, _) -> pure (Just [relativeChildPath])
  pure (toRelativePath rootPath : concat keptChildren)
toRelativePath :: FilePath -> FilePath
toRelativePath "." = "."
toRelativePath filePath =
  case splitDirectories filePath of
    "." : relativeSegments -> foldl1 (</>) relativeSegments
    segments -> foldl1 (</>) segments
shouldTraverseDirectory :: FilePath -> Bool
shouldTraverseDirectory repositoryPath =
  not
    ( any
        (`elem` ["tmp", "prm", "target", "result", ".agents", ".codex"])
        (splitDirectories repositoryPath)
    )
isLeafPath :: [FilePath] -> FilePath -> Bool
isLeafPath repositoryPaths candidatePath =
  let childPaths = [path | path <- repositoryPaths, takeDirectory path == candidatePath]
   in null childPaths
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
      childIsDirectoryFlags <- forM childNames $ \childName -> doesDirectoryExist (parentDirectory </> childName)
      pure $ sort [childName | (childName, isDirectory) <- zip childNames childIsDirectoryFlags, isDirectory]
checkPackageWith :: CanonicalizationSettings -> [String] -> FilePath -> IO PackageCheck
checkPackageWith canonicalizationSettings allRepositoryStructureIssues packageName = do
  let packageDefaultNixPath = "packages" </> packageName </> "default.nix"
      packageStructureIssues =
        [ issue
        | issue <- allRepositoryStructureIssues,
          ("packages/" ++ packageName) `isPrefixOf` issue
        ]
  packageKind <- detectPackageKindForPackage packageName
  packageDefaultNixExists <- doesFileExist packageDefaultNixPath
  (templateIssues, _) <-
    if not packageDefaultNixExists
      then pure ([], Nothing)
      else do
        packageDefaultNixSource <- TIO.readFile packageDefaultNixPath
        inferredTemplateName <- inferTemplateName packageName (T.unpack packageDefaultNixSource)
        case inferredTemplateName of
          Nothing ->
            pure
              ( [ "packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"
                ],
                Nothing
              )
          Just matchedTemplateName -> do
            case templateSpecByName matchedTemplateName of
              Nothing ->
                pure
                  ( [ "packages/" ++ packageName ++ "/default.nix: unsupported template " ++ matchedTemplateName
                    ],
                    Just matchedTemplateName
                  )
              Just templateSpec ->
                case templateBaselineSource templateSpec of
                  Just templateSource ->
                    do
                      let allowedNixDifferenceKeysForPackage =
                            if packageName == "c_template" && matchedTemplateName == "c_template"
                              then defaultAllowedNixDifferenceKeys
                              else templateAllowedDifferenceKeys templateSpec
                          templateSourceOverride =
                            case matchedTemplateName of
                              "python_template" ->
                                Just (renderPythonTemplateBaselineNixSourceWith defaultPythonTemplateDescription (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              "python_pypi_template" ->
                                Just (pythonPyPITemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              "python_pypi_application_template" ->
                                Just (pythonPyPIApplicationTemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              _ -> Just templateSource
                      templateComparisonIssues <- comparePackageDefaultNixWithTemplate packageName packageDefaultNixPath ("packages" </> matchedTemplateName </> "default.nix") allowedNixDifferenceKeysForPackage templateSourceOverride
                      pure (templateComparisonIssues, Just matchedTemplateName)
                  Nothing ->
                    pure
                      ( [ "packages/"
                            ++ packageName
                            ++ "/default.nix: internal error: missing embedded template baseline for "
                            ++ matchedTemplateName
                        ],
                        Just matchedTemplateName
                      )
  cargoTomlIssues <- checkCargoToml packageName
  cabalFileIssues <- checkCabalFile packageName
  defaultNixConventionIssues <- checkDefaultNixConventions packageName packageKind
  pythonTestConventionIssues <- checkPythonTestConventions packageName packageKind
  haskellTestConventionIssues <- checkHaskellTestConventions packageName packageKind
  rustTestConventionIssues <- checkRustTestConventions packageName packageKind
  pythonUnitTestNames <- discoverPythonUnitTestNames packageName packageKind
  haskellUnitTestNames <- discoverHaskellUnitTestNames packageName packageKind
  rustUnitTestNames <- discoverRustUnitTestNames packageName packageKind
  let cargoTomlOutcome = checkOutcomeFromIssues cargoTomlIssues
      cabalFileOutcome = checkOutcomeFromIssues cabalFileIssues
      makePackageTestCase testCaseName outcome issues =
        PackageTestCase
          testCaseName
          outcome
          (if outcome == CheckFailed then issues else [])
      makePackageTest testName outcome testCaseName issues =
        PackageTest
          testName
          outcome
          [makePackageTestCase testCaseName outcome issues]
      defaultNixIssues =
        [issue | issue <- packageStructureIssues, "/default.nix" `isInfixOf` issue]
          ++ templateIssues
      defaultNixOutcome =
        if packageDefaultNixExists && null defaultNixIssues
          then CheckPassed
          else CheckFailed
      pythonTestConventionOutcome = if packageKind `elem` [PythonPackage, PythonLatexPackage] then checkOutcomeFromIssues pythonTestConventionIssues else CheckSkipped
      haskellTestConventionOutcome = if packageKind == HaskellPackage then checkOutcomeFromIssues haskellTestConventionIssues else CheckSkipped
      rustTestConventionOutcome = if packageKind == RustPackage then checkOutcomeFromIssues rustTestConventionIssues else CheckSkipped
      basePackageTests =
        [ PackageTest
            "directory structure"
            (checkOutcomeFromIssues packageStructureIssues)
            [],
          makePackageTest "default.nix" defaultNixOutcome "matches template and policy" defaultNixIssues
        ]
      languageSpecificPackageTests =
        concat
          [ if packageKind == RustPackage
              then
                [ makePackageTest "Cargo.toml" cargoTomlOutcome "matches Cargo.toml conventions" cargoTomlIssues,
                  PackageTest
                    "src/main.rs"
                    rustTestConventionOutcome
                    ( makePackageTestCase
                        "defines Rust test cases"
                        rustTestConventionOutcome
                        rustTestConventionIssues
                        : [PackageTestCase rustUnitTestName CheckSkipped [] | rustUnitTestName <- rustUnitTestNames]
                    )
                ]
              else [],
            if packageKind == HaskellPackage
              then
                [ makePackageTest (packageName ++ ".cabal") cabalFileOutcome "matches Cabal conventions" cabalFileIssues,
                  PackageTest
                    "Main.hs"
                    haskellTestConventionOutcome
                    ( makePackageTestCase
                        "defines HUnit test cases"
                        haskellTestConventionOutcome
                        haskellTestConventionIssues
                        : [ PackageTestCase
                              haskellUnitTestName
                              CheckSkipped
                              []
                          | haskellUnitTestName <- if null haskellUnitTestNames then ["No named HUnit test labels discovered"] else haskellUnitTestNames
                          ]
                    )
                ]
              else [],
            [ PackageTest
                "main.py"
                pythonTestConventionOutcome
                ( makePackageTestCase
                    "defines pytest tests"
                    pythonTestConventionOutcome
                    pythonTestConventionIssues
                    : [PackageTestCase pythonUnitTestName CheckSkipped [] | pythonUnitTestName <- pythonUnitTestNames]
                )
            | packageKind `elem` [PythonPackage, PythonLatexPackage]
            ]
          ]
      packageTests = basePackageTests ++ languageSpecificPackageTests
      packageIssues =
        packageStructureIssues
          ++ templateIssues
          ++ defaultNixConventionIssues
          ++ cargoTomlIssues
          ++ cabalFileIssues
          ++ pythonTestConventionIssues
          ++ haskellTestConventionIssues
          ++ rustTestConventionIssues
  pure
    PackageCheck
      { packageCheckName = packageName,
        packageCheckKind = packageKind,
        packageCheckTests = packageTests,
        packageCheckIssues = packageIssues
      }
checkTemplateWith :: FilePath -> IO [String]
checkTemplateWith checkName = do
  let checkTemplatePath = "checks" </> checkName </> "default.nix"
  maybeCheckTemplateText <- readTextFileIfExists checkTemplatePath
  case maybeCheckTemplateText of
    Nothing -> pure []
    Just checkTemplateText -> do
      inferredCheckTemplateName <- inferCheckTemplateName checkName (T.unpack checkTemplateText)
      case inferredCheckTemplateName of
        Nothing ->
          pure
            [ "checks/" ++ checkName ++ "/default.nix: could not infer corresponding check template"
            ]
        Just matchedCheckTemplateName ->
          case checkTemplateSpecByName matchedCheckTemplateName of
            Nothing ->
              pure
                [ "checks/" ++ checkName ++ "/default.nix: unsupported check template " ++ matchedCheckTemplateName
                ]
            Just checkTemplateSpec ->
              (++)
                <$> validateCheckPackageAssociation checkName checkTemplatePath matchedCheckTemplateName
                <*> validateCheckTemplate checkName checkTemplatePath checkTemplateSpec
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
    "haskell_property_testing_check" -> withSuffix "-property-testing" [HaskellPackage]
    "python_coverage_check" -> withSuffix "_coverage" [PythonPackage, PythonLatexPackage]
    "python_profile_check" -> withSuffix "_profile" [PythonPackage, PythonLatexPackage]
    "python_property_testing_check" -> withSuffix "_property_testing" [PythonPackage, PythonLatexPackage]
    "rust_coverage_check" -> withSuffix "-coverage" [RustPackage]
    "rust_profile_check" -> withSuffix "-profile" [RustPackage]
    "rust_property_testing_check" -> withSuffix "-property-testing" [RustPackage]
    "rust_mutation_testing_check" -> withSuffix "-mutation-testing" [RustPackage]
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
  hasMainHaskellFile <- packageFileExists "Main.hs"
  hasCargoTomlFile <- packageFileExists "Cargo.toml"
  hasIndexHtmlFile <- packageFileExists "index.html"
  hasMainPythonFile <- packageFileExists "main.py"
  hasManuscriptTexFile <- packageFileExists "ms.tex"
  hasMainCFile <- packageFileExists "main.c"
  hasMainTerraformFile <- packageFileExists "main.tf"
  maybePackageDefaultNixSource <- readTextFileIfExists (packageRootDirectory </> "default.nix")
  let isPythonPyPIPackage =
        case maybePackageDefaultNixSource of
          Nothing -> False
          Just packageDefaultNixSource ->
            let packageDefaultNixSourceString = T.unpack packageDefaultNixSource
             in ("buildPythonPackage" `isInfixOf` packageDefaultNixSourceString || "buildPythonApplication" `isInfixOf` packageDefaultNixSourceString)
                  && not ("src = ./.;" `isInfixOf` packageDefaultNixSourceString)
                  && ("fetchPypi" `isInfixOf` packageDefaultNixSourceString || "fetchurl" `isInfixOf` packageDefaultNixSourceString)
  let packageKind
        | hasMainHaskellFile = HaskellPackage
        | hasCargoTomlFile = RustPackage
        | hasIndexHtmlFile = HtmlPackage
        | hasMainPythonFile && hasManuscriptTexFile = PythonLatexPackage
        | hasMainPythonFile = PythonPackage
        | isPythonPyPIPackage = PythonPyPIPackage
        | hasMainCFile = CPackage
        | hasMainTerraformFile = TerraformPackage
        | hasManuscriptTexFile = LatexPackage
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
discoverPythonUnitTestNames :: FilePath -> PackageKind -> IO [String]
discoverPythonUnitTestNames packageName packageKind =
  if packageKind `notElem` [PythonPackage, PythonLatexPackage]
    then pure []
    else do
      let mainPythonPath = "packages" </> packageName </> "main.py"
      maybeMainPythonSourceText <- readTextFileIfExists mainPythonPath
      case maybeMainPythonSourceText of
        Nothing -> pure []
        Just mainPythonSourceText -> pure (discoverPythonUnitTestNamesFromSource (T.unpack mainPythonSourceText))
discoverPythonUnitTestNamesFromSource :: String -> [String]
discoverPythonUnitTestNamesFromSource pythonSource =
  let extractedPythonUnitTestNames =
        [ functionName
        | sourceLine <- lines pythonSource,
          Just functionName <- [extractPythonUnitTestName sourceLine]
        ]
   in sort (Set.toList (Set.fromList extractedPythonUnitTestNames))
extractPythonUnitTestName :: String -> Maybe String
extractPythonUnitTestName sourceLine =
  let trimmedSourceLine = dropWhile (== ' ') sourceLine
      defPrefix :: String
      defPrefix = "def test_"
   in if defPrefix `isPrefixOf` trimmedSourceLine
        then
          let testFunctionName = takeWhile (\character -> character /= '(' && character /= ' ' && character /= ':') (drop 4 trimmedSourceLine)
           in if null testFunctionName then Nothing else Just testFunctionName
        else Nothing
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
              hasHUnitTestRunner = "runTestTT" `isInfixOf` haskellSource
              hasNamedTestSuite =
                "hUnitPackageTests" `isInfixOf` haskellSource
                  || "getAllFormattingTests" `isInfixOf` haskellSource
          pure $
            catMaybes
              [ if hasHUnitTestRunner
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: must run HUnit tests with runTestTT"),
                if hasNamedTestSuite
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: missing discoverable HUnit test suite")
              ]
discoverHaskellUnitTestNames :: FilePath -> PackageKind -> IO [String]
discoverHaskellUnitTestNames packageName packageKind =
  if packageKind /= HaskellPackage
    then pure []
    else do
      let mainHaskellPath = "packages" </> packageName </> "Main.hs"
      maybeMainHaskellSourceText <- readTextFileIfExists mainHaskellPath
      case maybeMainHaskellSourceText of
        Nothing -> pure []
        Just mainHaskellSourceText -> pure (discoverHaskellUnitTestNamesFromSource (T.unpack mainHaskellSourceText))
discoverHaskellUnitTestNamesFromSource :: String -> [String]
discoverHaskellUnitTestNamesFromSource haskellSource =
  let haskellSourceLines = lines haskellSource
      labelsFromMakeFormattingTest = extractMakeFormattingTestLabels haskellSourceLines
      labelsFromAssertEqual = extractAssertEqualTestLabels haskellSourceLines
      discoveredHaskellUnitTestNames = labelsFromMakeFormattingTest ++ labelsFromAssertEqual
      meaningfulHaskellUnitTestNames = filter isMeaningfulTestLabel discoveredHaskellUnitTestNames
   in sort (Set.toList (Set.fromList meaningfulHaskellUnitTestNames))
isMeaningfulTestLabel :: String -> Bool
isMeaningfulTestLabel label =
  let trimmedLabel = T.unpack (T.strip (T.pack label))
   in case trimmedLabel of
        firstCharacter : _ -> isAlphaNum firstCharacter && any isAlphaNum trimmedLabel
        [] -> False
extractAssertEqualTestLabels :: [String] -> [String]
extractAssertEqualTestLabels = go False
  where
    go _ [] = []
    go awaitingAssertEqualLabel (line : rest)
      | "assertEqual" `isInfixOf` line = go True rest
      | awaitingAssertEqualLabel =
          let trimmed = dropWhile (== ' ') line
           in if null trimmed
                then go True rest
                else
                  if startsWithQuote trimmed && not ("++" `isInfixOf` trimmed)
                    then case firstQuotedToken trimmed of
                      Just label -> label : go False rest
                      Nothing -> go False rest
                    else go False rest
      | otherwise = go False rest
    startsWithQuote [] = False
    startsWithQuote (ch : _) = ch == '"'
extractMakeFormattingTestLabels :: [String] -> [String]
extractMakeFormattingTestLabels = go False
  where
    go _ [] = []
    go awaitingMakeFormattingTestLabel (line : rest)
      | isMakeFormattingTestInvocationLine line = go True rest
      | awaitingMakeFormattingTestLabel =
          let trimmed = dropWhile (== ' ') line
           in if null trimmed
                then go True rest
                else case firstQuotedToken line of
                  Just label -> label : go False rest
                  Nothing -> go False rest
      | otherwise = go False rest
    isMakeFormattingTestInvocationLine line =
      let trimmed = dropWhile (== ' ') line
       in ", makeFormattingTest" `isPrefixOf` trimmed || "makeFormattingTest" `isPrefixOf` trimmed
firstQuotedToken :: String -> Maybe String
firstQuotedToken inputText =
  case dropWhile (/= '"') inputText of
    _ : rest ->
      let token = takeWhile (/= '"') rest
       in if null token then Nothing else Just token
    _ -> Nothing
checkRustTestConventions :: FilePath -> PackageKind -> IO [String]
checkRustTestConventions packageName packageKind =
  if packageKind /= RustPackage
    then pure []
    else do
      let mainRustPath = "packages" </> packageName </> "src/main.rs"
      mainRustFileExists <- doesFileExist mainRustPath
      mainRustSource <-
        if mainRustFileExists
          then T.unpack <$> TIO.readFile mainRustPath
          else pure ""
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
discoverRustUnitTestNames :: FilePath -> PackageKind -> IO [String]
discoverRustUnitTestNames packageName packageKind =
  if packageKind /= RustPackage
    then pure []
    else do
      let mainRustPath = "packages" </> packageName </> "src/main.rs"
      maybeMainRustSourceText <- readTextFileIfExists mainRustPath
      case maybeMainRustSourceText of
        Nothing -> pure []
        Just mainRustSourceText -> pure (discoverRustUnitTestNamesFromSource (T.unpack mainRustSourceText))
discoverRustUnitTestNamesFromSource :: String -> [String]
discoverRustUnitTestNamesFromSource rustSource =
  extractRustUnitTestNames (lines rustSource)
extractRustUnitTestNames :: [String] -> [String]
extractRustUnitTestNames sourceLines = sort (Set.toList (Set.fromList (go False sourceLines)))
  where
    go _ [] = []
    go awaitingFunctionAfterTestAttribute (line : rest) =
      let trimmed = dropWhile (== ' ') line
       in if "#[test]" `isPrefixOf` trimmed
            then go True rest
            else
              if awaitingFunctionAfterTestAttribute && "fn " `isPrefixOf` trimmed
                then
                  let functionName = takeWhile (\character -> character /= '(' && character /= ' ') (drop 3 trimmed)
                   in [functionName | not (null functionName)] ++ go False rest
                else go False rest
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
  cargoTomlFileExists <- doesFileExist cargoTomlPath
  if not cargoTomlFileExists
    then pure []
    else do
      cargoTomlContents <- TIO.readFile cargoTomlPath
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
  let step (currentTomlSectionHeader, normalizedLinesSoFar) sourceLine =
        let trimmedLine = T.strip sourceLine
         in if isTomlSectionHeader trimmedLine
              then
                if isCargoDependencySectionHeader trimmedLine
                  then (Just trimmedLine, normalizedLinesSoFar)
                  else (Just trimmedLine, normalizedLinesSoFar ++ [trimmedLine])
              else case currentTomlSectionHeader of
                Just header | isCargoDependencySectionHeader header -> (currentTomlSectionHeader, normalizedLinesSoFar)
                _ | T.null trimmedLine -> (currentTomlSectionHeader, normalizedLinesSoFar)
                Just "[package]" | isTomlNameAssignment trimmedLine -> (currentTomlSectionHeader, normalizedLinesSoFar ++ [normalizedNameLine])
                Just "[package]" | isTomlDescriptionAssignment trimmedLine -> (currentTomlSectionHeader, normalizedLinesSoFar)
                Just "[package]" | isTomlKeywordsAssignment trimmedLine -> (currentTomlSectionHeader, normalizedLinesSoFar)
                Just "[[bin]]" | isTomlNameAssignment trimmedLine -> (currentTomlSectionHeader, normalizedLinesSoFar ++ [normalizedNameLine])
                _ -> (currentTomlSectionHeader, normalizedLinesSoFar ++ [trimmedLine])
      (_, normalizedLines) = foldl' step (Nothing, []) (T.lines tomlContents)
      normalizedNameLine = "name = \"" <> T.pack packageName <> "\""
   in T.unlines normalizedLines
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
  T.length tomlLine >= 3 && T.head tomlLine == '[' && T.last tomlLine == ']'
lookupTomlString :: T.Text -> T.Text -> Maybe T.Text
lookupTomlString tomlKey sectionContents =
  let keyPrefix = tomlKey <> " = "
      maybeMatchingFieldLine = listToMaybe [T.strip sectionLine | sectionLine <- T.lines sectionContents, keyPrefix `T.isPrefixOf` T.strip sectionLine]
   in do
        matchingLine <- maybeMatchingFieldLine
        quotedValue <- T.stripPrefix keyPrefix matchingLine
        T.stripPrefix "\"" quotedValue >>= T.stripSuffix "\""
checkCabalFile :: FilePath -> IO [String]
checkCabalFile packageName = do
  let cabalFilePath = "packages" </> packageName </> packageName <.> "cabal"
  cabalFileExists <- doesFileExist cabalFilePath
  if not cabalFileExists
    then pure []
    else do
      cabalContents <- TIO.readFile cabalFilePath
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
  let step (insideBuildDependsSection, insideIgnoredMetadataField, normalizedLinesSoFar) sourceLine =
        let trimmedLine = T.strip sourceLine
            normalizedLine = normalizeCabalLineForBaselineComparison packageName trimmedLine
         in if insideBuildDependsSection
              then
                if T.null trimmedLine
                  then (True, False, normalizedLinesSoFar)
                  else case T.breakOn ":" trimmedLine of
                    (_, "") -> (True, False, normalizedLinesSoFar)
                    _ -> (False, False, normalizedLinesSoFar ++ [normalizedLine])
              else
                if insideIgnoredMetadataField && isCabalIndentedContinuationLine sourceLine
                  then (False, True, normalizedLinesSoFar)
                  else
                    if "build-depends:" `T.isPrefixOf` trimmedLine
                      then (True, False, normalizedLinesSoFar)
                      else
                        if T.null trimmedLine
                          then (False, False, normalizedLinesSoFar)
                          else
                            if isCabalSynopsisField trimmedLine || isCabalDescriptionField trimmedLine
                              then (False, True, normalizedLinesSoFar)
                              else (False, False, normalizedLinesSoFar ++ [normalizedLine])
      (_, _, normalizedLines) = foldl' step (False, False, []) (T.lines cabalContents)
   in T.unlines normalizedLines
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
      go [] = Nothing
      go (sourceLine : remainingLines)
        | fieldPrefix `T.isPrefixOf` T.strip sourceLine =
            let strippedValue = T.strip (T.drop (T.length fieldPrefix) (T.strip sourceLine))
             in Just $
                  if T.null strippedValue
                    then T.intercalate "\n" (map T.strip (takeWhile isCabalIndentedContinuationLine remainingLines))
                    else stripCabalQuotedValue strippedValue
        | otherwise = go remainingLines
   in go (T.lines cabalContents)
stripCabalQuotedValue :: T.Text -> T.Text
stripCabalQuotedValue quotedValue =
  fromMaybe quotedValue (T.stripPrefix "\"" quotedValue >>= T.stripSuffix "\"")
comparePackageDefaultNixWithTemplate :: FilePath -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
comparePackageDefaultNixWithTemplate packageName subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys maybeTemplateBaselineSourceOverride = do
  packageKind <- detectPackageKindForPackage packageName
  let ignoredTopLevelFunctionParams :: Set.Set T.Text
      ignoredTopLevelFunctionParams =
        case packageKind of
          CPackage -> Set.singleton "inputs"
          _ -> Set.empty
  compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys maybeTemplateBaselineSourceOverride
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
compareNixFileWithTemplate :: Set.Set T.Text -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateBaselineNixPath allowedNixDifferenceKeys maybeTemplateBaselineSourceOverride = do
  subjectNixParseResult <- parseNixExprFromFile subjectNixPath
  templateBaselineParseResult <-
    case maybeTemplateBaselineSourceOverride of
      Just templateBaselineText -> parseNixExprFromText templateBaselineText
      Nothing -> parseNixExprFromFile templateBaselineNixPath
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
  (temporaryNixPath, temporaryNixHandle) <- openTempFile "/tmp" "check-repository-template-override.nix"
  TIO.hPutStr temporaryNixHandle nixSource
  hClose temporaryNixHandle
  parseNixExprFromFile temporaryNixPath
    `finally` removeFileIfExists temporaryNixPath
parseNixExprFromFile :: FilePath -> IO (Either String NExprLoc)
parseNixExprFromFile nixFilePath =
  fmap (either (Left . show) Right) (parseNixFileLoc (Path nixFilePath))
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  when fileExists (removeFile filePath)
inferTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferTemplateName packageName nixSource = do
  maybeMatchedTemplateNames <- forM templateSpecs $ \templateSpec -> do
    matched <- templateMatches templateSpec packageName nixSource
    pure (if matched then Just (templateName templateSpec) else Nothing)
  pure (listToMaybe (catMaybes maybeMatchedTemplateNames))
inferCheckTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferCheckTemplateName checkName nixSource = do
  maybeMatchedCheckTemplateNames <- forM checkTemplateSpecs $ \checkTemplateSpec -> do
    matched <- checkTemplateMatches checkTemplateSpec checkName nixSource
    pure (if matched then Just (checkTemplateName checkTemplateSpec) else Nothing)
  pure (listToMaybe (catMaybes maybeMatchedCheckTemplateNames))
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
  filter (\(VarName paramName, _) -> not (Set.member paramName ignoredTopLevelFunctionParams))
sortNixParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortNixParams = sortBy (\(VarName leftName, _) (VarName rightName, _) -> compare leftName rightName)
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
    case nixKeyNameText bindingKey of
      Just keyText -> Set.member keyText allowedNixDifferenceKeys
      Nothing -> False
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
              packagePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings subjectNixExpr)
              templatePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings templateBaselineExpr)
              letBindingDifferenceLines =
                if Map.null packageLetBindingMap && Map.null templateLetBindingMap
                  then []
                  else formatBindingMapDifferences "let key" packageLetBindingMap templateLetBindingMap
              primaryBindingDifferenceLines =
                if Map.null packagePrimaryBindingMap && Map.null templatePrimaryBindingMap
                  then []
                  else formatBindingMapDifferences "key" packagePrimaryBindingMap templatePrimaryBindingMap
              differenceDetailLines = letBindingDifferenceLines ++ primaryBindingDifferenceLines
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
      sharedBindingKeys = Map.keys (Map.intersection packageBindingMap templateBindingMap)
      changedBindingKeys =
        [ bindingKey
        | bindingKey <- sharedBindingKeys,
          Map.lookup bindingKey packageBindingMap /= Map.lookup bindingKey templateBindingMap
        ]
   in map (formatNixBindingDifferenceLine ("missing " ++ keyLabel)) missingBindingKeys
        ++ map (formatNixBindingDifferenceLine ("unexpected " ++ keyLabel)) unexpectedBindingKeys
        ++ map
          ( \bindingKey ->
              let expectedValue = compactTextToSingleLine (fromMaybe "" (Map.lookup bindingKey templateBindingMap))
                  actualValue = compactTextToSingleLine (fromMaybe "" (Map.lookup bindingKey packageBindingMap))
               in "  - changed "
                    ++ keyLabel
                    ++ ": "
                    ++ T.unpack bindingKey
                    ++ "\n    expected: "
                    ++ truncateDiagnosticValue expectedValue
                    ++ "\n    actual:   "
                    ++ truncateDiagnosticValue actualValue
          )
          changedBindingKeys
formatNixBindingDifferenceLine :: String -> T.Text -> String
formatNixBindingDifferenceLine differenceKind bindingKey = "  - " ++ differenceKind ++ ": " ++ T.unpack bindingKey
compactTextToSingleLine :: T.Text -> String
compactTextToSingleLine textValue =
  let compactText = T.unwords (T.words textValue)
   in T.unpack compactText
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
    | (lineNumber, (actualLine, expectedLine)) <- zip [1 ..] (zip actualLines expectedLines),
      actualLine /= expectedLine
    ]
extractPrimaryNixBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractPrimaryNixBindings nixExpression = do
  bindingGroups <- collectNixSetBindingGroups nixExpression
  primaryBindingGroup <- maximumByLength bindingGroups
  pure (Map.fromList primaryBindingGroup)
extractOutermostLetBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractOutermostLetBindings (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NAbs _ body -> extractOutermostLetBindings body
    NLet bindings _ -> Just (Map.fromList (extractNamedNixBindings bindings))
    _ -> Nothing
maximumByLength :: [[a]] -> Maybe [a]
maximumByLength [] = Nothing
maximumByLength bindingGroups = Just (maximumBy (comparing length) bindingGroups)
collectNixSetBindingGroups :: NExprLoc -> Maybe [[(T.Text, T.Text)]]
collectNixSetBindingGroups (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NSet _ bindings ->
      let currentSetBindings = extractNamedNixBindings bindings
          nestedBindings = concatMap collectNixSetBindingGroupsFromBinding bindings
       in Just (currentSetBindings : nestedBindings)
    NLet bindings body ->
      let nestedFromBindings = concatMap collectNixSetBindingGroupsFromBinding bindings
          nestedFromBodyExpr = fromMaybe [] (collectNixSetBindingGroups body)
       in Just (nestedFromBindings ++ nestedFromBodyExpr)
    NAbs _ body -> collectNixSetBindingGroups body
    otherNixExpr ->
      Just (concatMap (fromMaybe [] . collectNixSetBindingGroups) otherNixExpr)
collectNixSetBindingGroupsFromBinding :: Binding NExprLoc -> [[(T.Text, T.Text)]]
collectNixSetBindingGroupsFromBinding (NamedVar _ bindingValue _) = fromMaybe [] (collectNixSetBindingGroups bindingValue)
collectNixSetBindingGroupsFromBinding (Inherit maybeBoundNixExpr _ _) = maybe [] (fromMaybe [] . collectNixSetBindingGroups) maybeBoundNixExpr
extractNamedNixBindings :: [Binding NExprLoc] -> [(T.Text, T.Text)]
extractNamedNixBindings bindings =
  [ (T.intercalate "." (mapMaybe nixKeyNameText (NE.toList keyPath)), normalizeRenderedNixBindingValue (T.intercalate "." (mapMaybe nixKeyNameText (NE.toList keyPath))) (renderNixExpr bindingValue))
  | NamedVar keyPath bindingValue _ <- bindings
  ]
normalizeRenderedNixBindingValue :: T.Text -> T.Text -> T.Text
normalizeRenderedNixBindingValue bindingKey renderedBindingValue
  | bindingKey == "meta" = stripMetaDescriptionAssignment (T.pack (compactTextToSingleLine renderedBindingValue))
  | otherwise = T.pack (compactTextToSingleLine renderedBindingValue)
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
runPackageTests = runPackageTestSuite hUnitPackageTests
runPackageEndToEndTests :: IO ()
runPackageEndToEndTests = runPackageTestSuite commandLineEndToEndTests
runPackageTestSuite :: Test -> IO ()
runPackageTestSuite packageTestSuite = do
  hUnitCounts <- runTestTT packageTestSuite
  if errors hUnitCounts == 0 && failures hUnitCounts == 0
    then putStrLn "test ... ok"
    else exitFailure
hUnitPackageTests :: Test
hUnitPackageTests =
  TestList
    [ templateInferenceTests,
      checkTemplateTests,
      metadataAndDiscoveryTests,
      repositoryPolicyTests,
      createRepositoryTests,
      addPackageTests
    ]
commandLineEndToEndTests :: Test
commandLineEndToEndTests =
  TestList
    [ TestCase commandLineHelpEndToEndTest,
      TestCase initHomeEndToEndTest,
      TestCase checkLocationRoutingEndToEndTest,
      TestCase addSummaryAndCheckEndToEndTest,
      TestCase invalidAddEndToEndTest
    ]
commandLineHelpEndToEndTest :: IO ()
commandLineHelpEndToEndTest =
  withTemporaryPackageRepository "command-line-help" $ \temporaryDirectory -> do
    (helpExit, helpStdout, helpStderr) <- runEndToEndCommandIn temporaryDirectory ["--help"]
    assertEqual "The top-level help command succeeds." ExitSuccess helpExit
    assertBool "The top-level help command prints usage to stdout." ("usage: git canonicalization" `isPrefixOf` helpStdout)
    assertEqual "The top-level help command leaves stderr empty." "" helpStderr
    (addHelpExit, addHelpStdout, addHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["add", "--help"]
    assertEqual "Command-specific help succeeds." ExitSuccess addHelpExit
    assertBool "Command-specific help prints the add usage to stdout." ("usage: git canonicalization add" `isPrefixOf` addHelpStdout)
    assertEqual "Command-specific help leaves stderr empty." "" addHelpStderr
    (checkHelpExit, checkHelpStdout, checkHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["check", "--help"]
    assertEqual "Check help succeeds." ExitSuccess checkHelpExit
    assertBool
      "Check help requires a location and explains home routing."
      ("<location>" `isInfixOf` checkHelpStdout && ".gitmodules" `isInfixOf` checkHelpStdout)
    assertEqual "Check help leaves stderr empty." "" checkHelpStderr
    (initHelpExit, initHelpStdout, initHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["init", "--help"]
    assertEqual "Init help succeeds." ExitSuccess initHelpExit
    assertBool
      "Init help explains the home Git repository and .gitignore policy."
      ("$HOME" `isInfixOf` initHelpStdout && ".gitignore" `isInfixOf` initHelpStdout)
    assertEqual "Init help leaves stderr empty." "" initHelpStderr
    (rmHelpExit, rmHelpStdout, rmHelpStderr) <- runEndToEndCommandIn temporaryDirectory ["rm", "--help"]
    assertEqual "Rm help succeeds." ExitSuccess rmHelpExit
    assertBool
      "Rm help explains the local repository and .gitmodules behavior."
      ("<local-repository>" `isInfixOf` rmHelpStdout && ".gitmodules" `isInfixOf` rmHelpStdout)
    assertEqual "Rm help leaves stderr empty." "" rmHelpStderr
    (missingCommandExit, missingCommandStdout, missingCommandStderr) <- runEndToEndCommandIn temporaryDirectory []
    assertEqual "An omitted command exits unsuccessfully." (ExitFailure 1) missingCommandExit
    assertEqual "An omitted command leaves stdout empty." "" missingCommandStdout
    assertBool "An omitted command prints usage to stderr." ("usage: git canonicalization" `isPrefixOf` missingCommandStderr)
    (invalidCommandExit, invalidCommandStdout, invalidCommandStderr) <- runEndToEndCommandIn temporaryDirectory ["unknown-command"]
    assertEqual "An invalid command uses Git's usage exit status." usageExitCode invalidCommandExit
    assertEqual "An invalid command leaves stdout empty." "" invalidCommandStdout
    assertBool "An invalid command prints usage to stderr." ("usage: git canonicalization" `isPrefixOf` invalidCommandStderr)
    (missingCheckLocationExit, missingCheckLocationStdout, missingCheckLocationStderr) <-
      runEndToEndCommandIn temporaryDirectory ["check"]
    assertEqual "Check requires a location." usageExitCode missingCheckLocationExit
    assertEqual "A missing check location leaves stdout empty." "" missingCheckLocationStdout
    assertBool "A missing check location prints check usage to stderr." ("usage: git canonicalization check <location>" `isPrefixOf` missingCheckLocationStderr)
    (legacyCloneExit, legacyCloneStdout, legacyCloneStderr) <-
      runEndToEndCommandIn temporaryDirectory ["clone", "https://example.test/owner/demo"]
    assertEqual "The legacy clone command is rejected." usageExitCode legacyCloneExit
    assertEqual "The legacy clone command leaves stdout empty." "" legacyCloneStdout
    assertBool "The legacy clone command prints usage to stderr." ("usage: git canonicalization" `isPrefixOf` legacyCloneStderr)
    (parameterizedInitExit, parameterizedInitStdout, parameterizedInitStderr) <-
      runEndToEndCommandIn temporaryDirectory ["init", "https://example.test/owner/demo"]
    assertEqual "Parameterized init is rejected." usageExitCode parameterizedInitExit
    assertEqual "Parameterized init leaves stdout empty." "" parameterizedInitStdout
    assertBool "Parameterized init prints init usage to stderr." ("usage: git canonicalization init" `isPrefixOf` parameterizedInitStderr)
initHomeEndToEndTest :: IO ()
initHomeEndToEndTest = do
  withTemporaryPackageRepository "init-home-end-to-end" $ \temporaryHome ->
    withEnvironmentVariable "HOME" temporaryHome $ do
      (initExit, _initStdout, _initStderr) <- runEndToEndCommandIn temporaryHome ["init"]
      assertEqual "Home initialization succeeds." ExitSuccess initExit
      doesDirectoryExist (temporaryHome </> ".git")
        >>= assertBool "Home initialization creates a Git repository."
      TIO.readFile (temporaryHome </> ".gitignore")
        >>= assertEqual "Home initialization creates the canonical ignore rule." "*\n"
      (reinitExit, _reinitStdout, _reinitStderr) <- runEndToEndCommandIn temporaryHome ["init"]
      assertEqual "Home initialization may safely reinitialize a compatible repository." ExitSuccess reinitExit
  withTemporaryPackageRepository "init-home-conflict-end-to-end" $ \temporaryHome ->
    withEnvironmentVariable "HOME" temporaryHome $ do
      TIO.writeFile (temporaryHome </> ".gitignore") "*.tmp\n"
      (initExit, initStdout, initStderr) <- runEndToEndCommandIn temporaryHome ["init"]
      assertEqual "An incompatible home .gitignore fails before Git initialization." (ExitFailure 1) initExit
      assertEqual "An incompatible home .gitignore leaves stdout empty." "" initStdout
      assertBool "An incompatible home .gitignore is reported on stderr." ("existing file must contain only *" `isInfixOf` initStderr)
      doesPathExist (temporaryHome </> ".git")
        >>= assertBool "A rejected home initialization does not create a Git repository." . not
checkLocationRoutingEndToEndTest :: IO ()
checkLocationRoutingEndToEndTest =
  withTemporaryPackageRepository "check-routing-home" $ \temporaryHome -> do
    initializeGitRepositoryFixture temporaryHome
    TIO.writeFile (temporaryHome </> ".gitmodules") ""
    let homeChild = temporaryHome </> "not-a-repository" </> "child"
    createDirectoryIfMissing True homeChild
    withEnvironmentVariable "HOME" temporaryHome $ do
      (homeCheckExit, homeCheckStdout, homeCheckStderr) <- runEndToEndCommandIn temporaryHome ["check", homeChild]
      assertEqual "A home descendant without its own repository uses the home check." ExitSuccess homeCheckExit
      assertEqual "A successful home check leaves stdout empty." "" homeCheckStdout
      assertEqual "A successful home check leaves stderr empty." "" homeCheckStderr
      TIO.writeFile (temporaryHome </> ".gitmodules") "path = owner/repository\n"
      (failedHomeExit, failedHomeStdout, failedHomeStderr) <- runEndToEndCommandIn temporaryHome ["check", homeChild]
      assertEqual "A malformed home submodule path fails." (ExitFailure 1) failedHomeExit
      assertEqual "A malformed home check leaves stdout empty." "" failedHomeStdout
      assertBool "A malformed home path is reported on stderr." ("must be exactly <host>/<owner>/<repo>" `isInfixOf` failedHomeStderr)
      let nestedRepository = temporaryHome </> "example.test" </> "owner" </> "demo"
      initializeGitRepositoryFixture nestedRepository
      TIO.writeFile (nestedRepository </> "flake.nix") "{}"
      TIO.writeFile (nestedRepository </> "flake.lock") "{}"
      (nestedCheckExit, nestedCheckStdout, nestedCheckStderr) <- runEndToEndCommandIn temporaryHome ["check", nestedRepository]
      unless (nestedCheckExit == ExitSuccess) $
        assertFailure
          ( "A nested repository beneath home should use canonical repository checks, but exited with "
              ++ show nestedCheckExit
              ++ ": "
              ++ nestedCheckStderr
          )
      assertEqual "A successful nested repository check leaves stdout empty." "" nestedCheckStdout
      assertEqual "A successful nested repository check leaves stderr empty." "" nestedCheckStderr
addSummaryAndCheckEndToEndTest :: IO ()
addSummaryAndCheckEndToEndTest =
  withTemporaryPackageRepository "add-summary-check-end-to-end" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    TIO.writeFile (temporaryRepository </> "flake.nix") "{}"
    TIO.writeFile (temporaryRepository </> "flake.lock") "{}"
    let nestedDirectory = temporaryRepository </> "packages"
    createDirectoryIfMissing True nestedDirectory
    (addExit, addStdout, addStderr) <-
      runEndToEndCommandIn
        nestedDirectory
        ["add", "python", "demo", "Demo package", "--coverage", "--property-testing"]
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
            temporaryRepository </> "checks/demo_coverage/default.nix",
            temporaryRepository </> "checks/demo_property_testing/default.nix"
          ]
    assertBool "The installed CLI creates the package and requested checks on disk." generatedFilesExist
    (summaryExit, summaryStdout, summaryStderr) <- runEndToEndCommandIn temporaryRepository ["summary"]
    assertEqual "Text summary succeeds for the generated repository." ExitSuccess summaryExit
    assertBool
      "Text summary reports generated metadata, checks, and discovered tests."
      ( all
          (`isInfixOf` summaryStdout)
          [ "packageName: demo",
            "packageType: python",
            "description: Demo package",
            "coverage",
            "property-testing",
            "test_canonicalize_label_examples"
          ]
      )
    assertEqual "A successful text summary leaves stderr empty." "" summaryStderr
    (jsonExit, jsonStdout, jsonStderr) <- runEndToEndCommandIn temporaryRepository ["summary", "--json"]
    assertEqual "JSON summary succeeds for the generated repository." ExitSuccess jsonExit
    assertBool
      "JSON summary reports the generated package and enabled checks."
      ( all
          (`isInfixOf` jsonStdout)
          [ "\"name\": \"demo\"",
            "\"packageType\": \"python\"",
            "\"description\": \"Demo package\"",
            "\"coverage\": true",
            "\"property-testing\": true"
          ]
      )
    assertEqual "A successful JSON summary leaves stderr empty." "" jsonStderr
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryRepository ["check", "."]
    assertEqual "Checking the generated repository succeeds." ExitSuccess checkExit
    assertEqual "A successful check produces no stdout." "" checkStdout
    assertEqual "A successful check produces no stderr." "" checkStderr
    TIO.writeFile (temporaryRepository </> "packages/demo/default.nix") "not valid nix template"
    (failedCheckExit, failedCheckStdout, failedCheckStderr) <- runEndToEndCommandIn temporaryRepository ["check", "."]
    assertEqual "Checking a corrupted package fails." (ExitFailure 1) failedCheckExit
    assertEqual "A failed check leaves stdout empty." "" failedCheckStdout
    assertBool
      "A failed check reports its phase and affected file."
      ( "canonicalization check failed at phase: file-compliance" `isInfixOf` failedCheckStderr
          && "packages/demo/default.nix:" `isInfixOf` failedCheckStderr
      )
    (rmExit, rmStdout, rmStderr) <- runEndToEndCommandIn temporaryRepository ["rm", "demo"]
    assertEqual "Removing the generated package succeeds." ExitSuccess rmExit
    assertEqual "A successful package removal produces no stdout." "" rmStdout
    assertEqual "A successful package removal produces no stderr." "" rmStderr
    removedPackageExists <- doesPathExist (temporaryRepository </> "packages/demo")
    removedCoverageCheckExists <- doesPathExist (temporaryRepository </> "checks/demo_coverage")
    removedPropertyTestingCheckExists <- doesPathExist (temporaryRepository </> "checks/demo_property_testing")
    assertBool "Package removal deletes the package directory." (not removedPackageExists)
    assertBool "Package removal deletes its coverage check." (not removedCoverageCheckExists)
    assertBool "Package removal deletes its property-testing check." (not removedPropertyTestingCheckExists)
invalidAddEndToEndTest :: IO ()
invalidAddEndToEndTest =
  withTemporaryPackageRepository "invalid-add-end-to-end" $ \temporaryRepository -> do
    initializeGitRepositoryFixture temporaryRepository
    TIO.writeFile (temporaryRepository </> "flake.nix") "{}"
    TIO.writeFile (temporaryRepository </> "flake.lock") "{}"
    (unknownOptionExit, unknownOptionStdout, unknownOptionStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "python", "demo", "--unknown"]
    assertEqual "An unknown add option uses Git's usage exit status." usageExitCode unknownOptionExit
    assertEqual "An unknown add option leaves stdout empty." "" unknownOptionStdout
    assertBool "An unknown add option prints add usage to stderr." ("usage: git canonicalization add" `isPrefixOf` unknownOptionStderr)
    (invalidNameExit, invalidNameStdout, invalidNameStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "python", "demo-python"]
    assertEqual "An invalid package name fails." (ExitFailure 1) invalidNameExit
    assertEqual "An invalid package name leaves stdout empty." "" invalidNameStdout
    assertBool "An invalid package name reports its convention." ("must use snake_case" `isInfixOf` invalidNameStderr)
    (unsupportedCheckExit, unsupportedCheckStdout, unsupportedCheckStderr) <-
      runEndToEndCommandIn temporaryRepository ["add", "html", "demo", "--coverage"]
    assertEqual "An unsupported check selection fails." (ExitFailure 1) unsupportedCheckExit
    assertEqual "An unsupported check selection leaves stdout empty." "" unsupportedCheckStdout
    assertBool "An unsupported check selection reports the rejected option." ("unsupported checks for package type html: --coverage" `isInfixOf` unsupportedCheckStderr)
    packageDirectoryExists <- doesDirectoryExist (temporaryRepository </> "packages/demo")
    assertBool "Rejected add commands do not leave a partial package directory." (not packageDirectoryExists)
runEndToEndCommandIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runEndToEndCommandIn workingDirectory arguments = do
  executablePath <- requireEndToEndExecutable
  withCurrentWorkingDirectory workingDirectory (readProcessWithExitCode executablePath arguments "")
requireEndToEndExecutable :: IO FilePath
requireEndToEndExecutable =
  findExecutable "git-canonicalization" >>= \case
    Nothing -> assertFailure "git-canonicalization must be available on PATH for command-line end-to-end tests"
    Just executablePath -> pure executablePath
initializeGitRepositoryFixture :: FilePath -> IO ()
initializeGitRepositoryFixture repositoryPath =
  findExecutable "git" >>= \case
    Nothing -> assertFailure "git is required for command-line end-to-end tests"
    Just _ -> do
      (gitInitExit, _gitInitStdout, gitInitStderr) <- readProcessWithExitCode "git" ["init", "--quiet", repositoryPath] ""
      unless (gitInitExit == ExitSuccess) $
        assertFailure ("Failed to initialize Git repository fixture: " ++ gitInitStderr)
runGitFixtureCommand :: [String] -> IO ()
runGitFixtureCommand arguments = do
  (gitExit, _gitStdout, gitStderr) <- readProcessWithExitCode "git" arguments ""
  unless (gitExit == ExitSuccess) $
    assertFailure ("Git fixture command failed: git " ++ unwords arguments ++ if null gitStderr then "" else ": " ++ gitStderr)
templateInferenceTests :: Test
templateInferenceTests =
  TestList
    [ TestCase $ do
        inferred <- inferTemplateName "test" uncommentNixFixture
        assertEqual
          "Infers the uncomment template."
          (Just "uncomment_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" rustNixFixture
        assertEqual
          "Infers the Rust package template."
          (Just "rust_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" haskellNixFixture
        assertEqual
          "Infers the Haskell package template."
          (Just "haskell_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonNixFixture
        assertEqual
          "Infers the Python template."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonPyPINixFixture
        assertEqual
          "Infers the Python PyPI template."
          (Just "python_pypi_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" binaryReleaseNixFixture
        assertEqual
          "Infers the binary release template."
          (Just "binary_release_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonLatexNixFixture
        assertEqual
          "Infers the Python template for the python-latex fixture."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "deploy_host_template" deployHostNixFixture
        assertEqual
          "Infers the deploy host template."
          (Just "deploy_host_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" cNixFixture
        assertEqual
          "Infers the C template."
          (Just "c_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" latexNixFixture
        assertEqual
          "Infers the LaTeX template."
          (Just "latex_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" htmlNixFixture
        assertEqual
          "Infers the HTML template."
          (Just "html_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" unknownNixFixture
        assertEqual
          "Returns no template for unknown input."
          Nothing
          inferred,
      TestCase $ do
        inferred <- inferCheckTemplateName "python_template_coverage" (T.unpack pythonCoverageCheckBaselineNixSource)
        assertEqual
          "Infers the Python coverage check template."
          (Just "python_coverage_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckTemplateName "git-canonicalization-profile" (T.unpack haskellProfileCheckBaselineNixSource)
        assertEqual
          "Infers the Haskell profile check template."
          (Just "haskell_profile_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckTemplateName "remove-empty-lines-mutation-testing" (T.unpack rustMutationTestingCheckBaselineNixSource)
        assertEqual
          "Infers the Rust mutation-testing check template."
          (Just "rust_mutation_testing_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckTemplateName "laptopVmWithDisko" (T.unpack defaultVmWithDiskoCheckBaselineNixSource)
        assertEqual
          "Infers the generic VM-with-disko check template."
          (Just "default_vm_with_disko_check")
          inferred,
      TestCase $
        assertEqual
          "Parses the canonical git+ssh repository URL form."
          ( Just
              ( RepositoryLocation
                  "example.test"
                  "owner"
                  "demo"
                  "git+ssh://git@example.test/owner/demo"
              )
          )
          (parseRepositoryUrl "git+ssh://git@example.test/owner/demo")
    ]
checkTemplateTests :: Test
checkTemplateTests =
  TestList
    [ TestCase $ do
        let matched = isCPackageVmCheckShape CPackage cPackageVmCheckFixture
        assertBool
          "Matches a generic C-package VM check."
          matched,
      TestCase $ do
        let validationIssues =
              validateCPackageVmCheckSource
                CPackage
                "c_template"
                "checks/c_template/default.nix"
                cPackageVmCheckFixture
        assertEqual
          "Validates a generic C-package VM check."
          []
          validationIssues,
      TestCase $ do
        (tempPath, tempHandle) <- openTempFile "/tmp" "python-template-custom-description.nix"
        TIO.hPutStr tempHandle (renderPythonTemplateBaselineNixSourceWith "Custom Python package" defaultPythonPackageAttribute)
        hClose tempHandle
        issues <-
          comparePackageDefaultNixWithTemplate
            "demo"
            tempPath
            "packages/python_template/default.nix"
            (Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"])
            (Just pythonTemplateBaselineNixSource)
        removeFileIfExists tempPath
        assertEqual
          "Allows Python package descriptions to differ from the template baseline."
          []
          issues,
      TestCase $ do
        let customPythonVersionSource =
              T.unlines
                [ "{",
                  "  inputs,",
                  "  pkgs ? import <nixpkgs> { },",
                  "}:",
                  "let",
                  "  python = pkgs.python311;",
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
                  "    description = \"Custom Python package.\";",
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
        (tempPath, tempHandle) <- openTempFile "/tmp" "python-template-custom-version.nix"
        TIO.hPutStr tempHandle customPythonVersionSource
        hClose tempHandle
        issues <-
          comparePackageDefaultNixWithTemplate
            "demo"
            tempPath
            "packages/python_template/default.nix"
            (Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"])
            (Just pythonTemplateBaselineNixSource)
        removeFileIfExists tempPath
        assertEqual
          "Allows Python package interpreter selection to differ from the template baseline."
          []
          issues,
      TestCase $ do
        runNixOsTestParseResult <-
          parseNixExprFromText
            ( T.unlines
                [ "{ pkgs, ... }:",
                  "pkgs.testers.runNixOSTest {",
                  "  name = \"example\";",
                  "}"
                ]
            )
        case runNixOsTestParseResult of
          Right runNixOsTestExpr ->
            assertEqual
              "Extracts primary bindings from runNixOSTest application shapes without crashing."
              (Just (Map.fromList [("name", "\"example\"")]))
              (extractPrimaryNixBindings runNixOsTestExpr)
          Left parseError -> assertFailure ("Failed to parse runNixOSTest fixture: " ++ parseError),
      TestCase orphanCheckPackageAssociationTest
    ]
orphanCheckPackageAssociationTest :: IO ()
orphanCheckPackageAssociationTest =
  withTemporaryPackageRepository "orphan-check-package-association" $ \tempRepository ->
    withCurrentWorkingDirectory tempRepository $ do
      let checkDirectory = "checks" </> "orphan_coverage"
          checkPath = checkDirectory </> "default.nix"
      createDirectoryIfMissing True checkDirectory
      TIO.writeFile checkPath pythonCoverageCheckBaselineNixSource
      orphanIssues <- checkTemplateWith "orphan_coverage"
      addPackageResult <-
        addPackageToCurrentRepositoryWith
          defaultCanonicalizationSettings
          PythonPackage
          "orphan"
          Nothing
          Set.empty
      associatedIssues <- checkTemplateWith "orphan_coverage"
      assertEqual
        "Rejects a language check without its corresponding package."
        ["checks/orphan_coverage/default.nix: python_coverage_check requires corresponding python or python-latex package packages/orphan"]
        orphanIssues
      assertBool "Creates the corresponding package fixture." (case addPackageResult of Right _ -> True; Left _ -> False)
      assertEqual "Accepts the check once its corresponding package exists." [] associatedIssues
metadataAndDiscoveryTests :: Test
metadataAndDiscoveryTests =
  TestList
    [TestCase metadataAndDiscoveryTest]
metadataAndDiscoveryTest :: IO ()
metadataAndDiscoveryTest =
  withTemporaryPackageRepository "python-package-summary" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        addPackageToCurrentRepositoryWith defaultCanonicalizationSettings PythonPackage "demo" (Just "Demo package") Set.empty
          >> TIO.writeFile
            ("packages" </> "demo" </> "main.py")
            ( T.unlines
                [ "def helper_function():",
                  "    return None",
                  "",
                  "def test_beta_case():",
                  "    return None",
                  "",
                  "def test_alpha_case():",
                  "    return None"
                ]
            )
          >> summarizeRepositoryPackage (Set.fromList ["demo_coverage"]) "demo"
          >>= \RepositoryPackageSummary
                 { repositoryPackageName = packageName,
                   repositoryPackageType = packageType,
                   repositoryPackageDescription = packageDescription,
                   repositoryPackageTestNames = packageTestNames,
                   repositoryPackageChecks =
                     RepositoryPackageChecksSummary
                       { repositoryPackageHasCheck = hasCheck,
                         repositoryPackageHasCoverageCheck = hasCoverageCheck,
                         repositoryPackageHasProfileCheck = hasProfileCheck,
                         repositoryPackageHasPropertyTestingCheck = hasPropertyTestingCheck,
                         repositoryPackageHasMutationTestingCheck = hasMutationTestingCheck
                       }
                 } ->
              assertBool
                "Summarizes generated package metadata and discovered tests."
                ( packageName == "demo"
                    && packageType == "python"
                    && packageDescription == Just "Demo package"
                    && packageTestNames == ["test_alpha_case", "test_beta_case"]
                    && not hasCheck
                    && hasCoverageCheck
                    && not hasProfileCheck
                    && not hasPropertyTestingCheck
                    && not hasMutationTestingCheck
                )
repositoryPolicyTests :: Test
repositoryPolicyTests =
  TestList
    [ TestCase gitRepositoryRootDiscoveryTest,
      TestCase repositoryRequiredRootFilesTest,
      TestCase repositoryRequiredRootFilesPrecedenceTest,
      TestCase repositoryPolicyHaskellComplianceTest,
      TestCase repositoryPolicyDirectoryStructureTest,
      TestCase repositoryPolicyFileComplianceTest
    ]
repositoryRequiredRootFilesTest :: IO ()
repositoryRequiredRootFilesTest =
  withTemporaryPackageRepository "required-root-files" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $ do
        missingBothIssues <- checkRequiredRepositoryRootFiles
        TIO.writeFile "flake.nix" "{}"
        missingLockIssues <- checkRequiredRepositoryRootFiles
        TIO.writeFile "flake.lock" "{}"
        noMissingIssues <- checkRequiredRepositoryRootFiles
        assertEqual
          "Reports every missing required repository root file in canonical order."
          ["missing required file: flake.nix", "missing required file: flake.lock"]
          missingBothIssues
        assertEqual
          "Reports a missing flake.lock after flake.nix is present."
          ["missing required file: flake.lock"]
          missingLockIssues
        assertEqual
          "Accepts both required repository root files."
          []
          noMissingIssues
repositoryRequiredRootFilesPrecedenceTest :: IO ()
repositoryRequiredRootFilesPrecedenceTest =
  withTemporaryPackageRepository "required-root-files-precedence" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        createDirectoryIfMissing True "unexpected"
          >> TIO.writeFile ("unexpected" </> "file.txt") "bad"
          >> collectRepositoryComplianceWith defaultCanonicalizationSettings
          >>= assertEqual
            "Reports missing root files before traversing or validating repository structure."
            ( Left
                ( "required-root-files",
                  ["missing required file: flake.nix", "missing required file: flake.lock"]
                )
            )
gitRepositoryRootDiscoveryTest :: IO ()
gitRepositoryRootDiscoveryTest =
  findExecutable "git" >>= \case
    Nothing -> pure ()
    Just _ ->
      withTemporaryPackageRepository "git-repository-root-discovery" $
        \tempRepository -> do
          (gitInitExit, _gitInitStdout, gitInitStderr) <- readProcessWithExitCode "git" ["init", "--quiet", tempRepository] ""
          unless (gitInitExit == ExitSuccess) $
            assertFailure ("Failed to initialize Git repository fixture: " ++ gitInitStderr)
          let nestedDirectory = tempRepository </> "packages" </> "demo"
          createDirectoryIfMissing True nestedDirectory
          canonicalRepositoryRoot <- canonicalizePath tempRepository
          runInGitRepositoryRoot nestedDirectory $
            getCurrentDirectory
              >>= assertEqual
                "Uses the discovered Git repository root for an explicit nested directory."
                canonicalRepositoryRoot
          withCurrentWorkingDirectory nestedDirectory $
            runInGitRepositoryRoot "." $
              getCurrentDirectory
                >>= assertEqual
                  "Uses the discovered Git repository root when invoked from a nested directory."
                  canonicalRepositoryRoot
repositoryPolicyHaskellComplianceTest :: IO ()
repositoryPolicyHaskellComplianceTest =
  withTemporaryPackageRepository "haskell-repository-compliance" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        TIO.writeFile "flake.nix" "{}"
          >> TIO.writeFile "flake.lock" "{}"
          >> addPackageToCurrentRepositoryWith defaultCanonicalizationSettings HaskellPackage "demo" Nothing Set.empty
          >> collectRepositoryComplianceWith defaultCanonicalizationSettings
          >>= \case
            Right
              RepositoryComplianceSuccess
                { repositoryCompliancePackageNames = ["demo"],
                  repositoryComplianceCheckNames = []
                } ->
                assertBool
                  "Recognizes a generated Haskell package as repository-compliant."
                  True
            otherResult ->
              assertFailure ("Expected repository-compliant result, got: " ++ show otherResult)
repositoryPolicyDirectoryStructureTest :: IO ()
repositoryPolicyDirectoryStructureTest =
  withTemporaryPackageRepository "directory-structure-repository-validation" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        TIO.writeFile "flake.nix" "{}"
          >> TIO.writeFile "flake.lock" "{}"
          >> createDirectoryIfMissing True "unexpected"
          >> TIO.writeFile ("unexpected" </> "file.txt") "bad"
          >> collectRepositoryComplianceWith defaultCanonicalizationSettings
          >>= \case
            Left ("directory-structure", repositoryStructureIssues) ->
              assertBool
                "Rejects non-canonical files at the repository root."
                ("unexpected/file.txt: is not allowed" `elem` repositoryStructureIssues)
            otherResult ->
              assertFailure ("Expected directory-structure failure, got: " ++ show otherResult)
repositoryPolicyFileComplianceTest :: IO ()
repositoryPolicyFileComplianceTest =
  withTemporaryPackageRepository "file-compliance-repository-validation" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        TIO.writeFile "flake.nix" "{}"
          >> TIO.writeFile "flake.lock" "{}"
          >> addPackageToCurrentRepositoryWith defaultCanonicalizationSettings PythonPackage "demo" Nothing Set.empty
          >> TIO.writeFile ("packages" </> "demo" </> "default.nix") "not valid nix template"
          >> collectRepositoryComplianceWith defaultCanonicalizationSettings
          >>= \case
            Left ("file-compliance", fileComplianceIssues) ->
              assertBool
                "Rejects malformed package files after scaffolding."
                (any ("packages/demo/default.nix:" `isPrefixOf`) fileComplianceIssues)
            otherResult ->
              assertFailure ("Expected file-compliance failure, got: " ++ show otherResult)
createRepositoryTests :: Test
createRepositoryTests =
  TestList
    [ TestCase $ do
        assertEqual
          "Parses paths and preserves their first occurrences."
          ["github.com/owner/one", "gitlab.com/owner/two"]
          ( parseHomeGitmodulePathEntries
              ( T.unlines
                  [ "[submodule \"one\"]",
                    "  path = github.com/owner/one  ",
                    "  url = git@example.test:owner/one",
                    "path = github.com/owner/one",
                    "pathname = gitlab.com/owner/ignored",
                    "path = gitlab.com/owner/two",
                    "path =    "
                  ]
              )
          ),
      TestCase $ do
        forM_ ["github.com/owner/repository", "host/a/b"] $ \pathEntry ->
          assertBool ("Accepts canonical home path " ++ T.unpack pathEntry) (isCompatibleHomeGitmodulePath pathEntry)
        forM_
          [ "repository",
            "owner/repository",
            "host/owner/repository/extra",
            "/host/owner",
            "host//repository",
            "host/./repository",
            "host/../repository"
          ]
          $ \pathEntry ->
            assertBool ("Rejects noncanonical home path " ++ T.unpack pathEntry) (not (isCompatibleHomeGitmodulePath pathEntry)),
      TestCase $
        withTemporaryPackageRepository "home-gitmodules-check" $ \tempHome -> do
          missingResult <- checkHomeGitmodules tempHome
          assertBool "Reports a missing home .gitmodules file." (either ("missing file:" `isPrefixOf`) (const False) missingResult)
          TIO.writeFile (tempHome </> ".gitmodules") "[submodule \"demo\"]\n  path = github.com/owner/demo\n"
          checkHomeGitmodules tempHome >>= assertEqual "Accepts a canonical home .gitmodules file." (Right ()),
      TestCase $
        assertBool
          "Only a single star rule with an optional trailing newline is compatible."
          (homeGitignoreIsCompatible "*" && homeGitignoreIsCompatible "*\n" && not (homeGitignoreIsCompatible "*.tmp\n*\n")),
      TestCase $
        assertEqual
          "Repository adds delegate to git submodule add with the canonical home-relative path."
          (Right ["-C", "/home/test", "submodule", "add", "--force", "https://example.test/owner/demo.git", "example.test/owner/demo"])
          ( gitSubmoduleAddArguments
              "/home/test"
              (RepositoryLocation "example.test" "owner" "demo" "https://example.test/owner/demo.git")
          ),
      TestCase removeRepositorySubmoduleTest
    ]
removeRepositorySubmoduleTest :: IO ()
removeRepositorySubmoduleTest =
  findExecutable "git" >>= \case
    Nothing -> pure ()
    Just _ ->
      withTemporaryPackageRepository "remove-repository-home" $ \tempHome ->
        withTemporaryPackageRepository "remove-repository-remote" $ \tempRemote -> do
          initializeGitRepositoryFixture tempRemote
          runGitFixtureCommand ["-C", tempRemote, "config", "user.name", "Canonicalization Tests"]
          runGitFixtureCommand ["-C", tempRemote, "config", "user.email", "canonicalization@example.test"]
          TIO.writeFile (tempRemote </> "README.md") "test repository\n"
          runGitFixtureCommand ["-C", tempRemote, "add", "README.md"]
          runGitFixtureCommand ["-C", tempRemote, "commit", "--quiet", "-m", "Initial commit"]
          initializeGitRepositoryFixture tempHome
          runGitFixtureCommand ["-C", tempHome, "config", "user.name", "Canonicalization Tests"]
          runGitFixtureCommand ["-C", tempHome, "config", "user.email", "canonicalization@example.test"]
          let repositoryPathEntry = "example.test" </> "owner" </> "demo"
              repositoryPath = tempHome </> repositoryPathEntry
          runGitFixtureCommand ["-c", "protocol.file.allow=always", "-C", tempHome, "submodule", "add", "--quiet", tempRemote, repositoryPathEntry]
          runGitFixtureCommand ["-C", tempHome, "commit", "--quiet", "-m", "Add test submodule"]
          removeResult <- removeGitRepositoryInHome tempHome repositoryPath
          repositoryPathExists <- doesPathExist repositoryPath
          gitmodulesExists <- doesFileExist (tempHome </> ".gitmodules")
          gitmodulesSource <- if gitmodulesExists then TIO.readFile (tempHome </> ".gitmodules") else pure ""
          (trackedPathExit, trackedPathStdout, _trackedPathStderr) <- readProcessWithExitCode "git" ["-C", tempHome, "ls-files", "--", repositoryPathEntry] ""
          retainedGitDirectoryExists <- doesDirectoryExist (tempHome </> ".git" </> "modules" </> repositoryPathEntry)
          assertEqual "Removing a repository submodule succeeds." (Right ()) removeResult
          assertBool "The removed submodule work tree no longer exists." (not repositoryPathExists)
          assertBool "The removed submodule is no longer tracked." (trackedPathExit == ExitSuccess && null trackedPathStdout)
          assertBool "The removed submodule no longer has a .gitmodules entry." (not (T.pack repositoryPathEntry `T.isInfixOf` gitmodulesSource))
          assertBool "Git retains the removed submodule's Git directory for history." retainedGitDirectoryExists
addPackageTests :: Test
addPackageTests =
  TestList
    [ TestCase addPackagePythonScaffoldTest,
      TestCase addPackageRepositoryChecksTest,
      TestCase addPackageHtmlDefaultCheckTest,
      TestCase addPackageDescriptionsTest,
      TestCase nixDescriptionInterpolationEscapingTest,
      TestCase repositoryPackageNamingConventionTest,
      TestCase addPackageUnsupportedChecksTest,
      TestCase removePackageAndChecksTest
    ]
addPackagePythonScaffoldTest :: IO ()
addPackagePythonScaffoldTest =
  withTemporaryPackageRepository "python-package-scaffold" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        addPackageToCurrentRepositoryWith
          defaultCanonicalizationSettings
          PythonPackage
          "demo"
          (Just "Custom Python package")
          Set.empty
          >>= \addPackageResult ->
            detectPackageKindForPackage "demo"
              >>= \packageDetectedKind -> do
                assertEqual
                  "Adds a Python package scaffold end to end."
                  (Right ["packages/demo/.gitignore", "packages/demo/default.nix", "packages/demo/main.py"])
                  addPackageResult
                assertEqual
                  "Detects the generated Python package from its on-disk files."
                  PythonPackage
                  packageDetectedKind
addPackageRepositoryChecksTest :: IO ()
addPackageRepositoryChecksTest =
  withTemporaryPackageRepository "repository-check-creation" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        TIO.writeFile "flake.nix" "{}"
          >> TIO.writeFile "flake.lock" "{}"
          >> addPackageToCurrentRepositoryWith
            defaultCanonicalizationSettings
            HaskellPackage
            "demo"
            Nothing
            (Set.fromList [RepositoryCoverageCheck, RepositoryProfileCheck])
          >>= \addPackageResult ->
            collectRepositoryComplianceWith defaultCanonicalizationSettings
              >>= \repositoryComplianceResult -> do
                assertEqual
                  "Adds package scaffolding and requested checks together."
                  ( Right
                      [ "packages/demo/.gitignore",
                        "packages/demo/default.nix",
                        "packages/demo/Main.hs",
                        "packages/demo/demo.cabal",
                        "checks/demo-coverage/default.nix",
                        "checks/demo-profile/default.nix"
                      ]
                  )
                  addPackageResult
                case repositoryComplianceResult of
                  Right
                    RepositoryComplianceSuccess
                      { repositoryCompliancePackageNames = ["demo"],
                        repositoryComplianceCheckNames = ["demo-coverage", "demo-profile"]
                      } ->
                      assertBool
                        "Recognizes the generated repository and checks as compliant."
                        True
                  otherResult ->
                    assertFailure ("Expected repository compliance success, got: " ++ show otherResult)
addPackageHtmlDefaultCheckTest :: IO ()
addPackageHtmlDefaultCheckTest =
  withTemporaryPackageRepository "html-default-check-creation" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $ do
        TIO.writeFile "flake.nix" "{}"
        TIO.writeFile "flake.lock" "{}"
        addPackageResult <-
          addPackageToCurrentRepositoryWith
            defaultCanonicalizationSettings
            HtmlPackage
            "demo_page"
            Nothing
            (Set.singleton RepositoryDefaultCheck)
        repositoryComplianceResult <- collectRepositoryComplianceWith defaultCanonicalizationSettings
        assertEqual
          "Adds a same-name HTML default check."
          ( Right
              [ "packages/demo_page/.gitignore",
                "packages/demo_page/default.nix",
                "packages/demo_page/index.html",
                "packages/demo_page/script.js",
                "packages/demo_page/style.css",
                "checks/demo_page/default.nix"
              ]
          )
          addPackageResult
        assertEqual
          "Recognizes an HTML default check generated for an arbitrary valid package name."
          (Right (RepositoryComplianceSuccess ["demo_page"] ["demo_page"]))
          repositoryComplianceResult
addPackageDescriptionsTest :: IO ()
addPackageDescriptionsTest =
  withTemporaryPackageRepository "package-scaffold-descriptions" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $ do
        let customDescription :: String
            customDescription = "Custom package description"
            scaffoldCases :: [(PackageKind, FilePath)]
            scaffoldCases =
              [ (HaskellPackage, "demo-haskell"),
                (RustPackage, "demo-rust"),
                (HtmlPackage, "demo_html"),
                (PythonLatexPackage, "demo_python_latex"),
                (PythonPackage, "demo_python"),
                (CPackage, "demo_c"),
                (LatexPackage, "demo_latex")
              ]
        TIO.writeFile "flake.nix" "{}"
        TIO.writeFile "flake.lock" "{}"
        addResults <-
          forM scaffoldCases $ \(packageKind, packageName) ->
            addPackageToCurrentRepositoryWith
              defaultCanonicalizationSettings
              packageKind
              packageName
              (Just customDescription)
              Set.empty
        packageDescriptions <-
          forM scaffoldCases $ \(_, packageName) ->
            repositoryPackageDescription <$> summarizeRepositoryPackage Set.empty packageName
        haskellDescriptionIssues <- checkCabalFile "demo-haskell"
        rustDescriptionIssues <- checkCargoToml "demo-rust"
        nixDescriptionIssues <-
          concat
            <$> mapM
              ( \(packageName, templateNameValue, allowedDifferenceKeys, templateSource) ->
                  comparePackageDefaultNixWithTemplate
                    packageName
                    ("packages" </> packageName </> "default.nix")
                    ("packages" </> templateNameValue </> "default.nix")
                    allowedDifferenceKeys
                    (Just templateSource)
              )
              [ ("demo_html", "html_template", Set.insert "text" defaultAllowedNixDifferenceKeys, htmlTemplateBaselineNixSource),
                ("demo_python_latex", "python_latex_template", Set.fromList ["meta", "propagatedBuildInputs", "version"], pythonLatexTemplateBaselineNixSource),
                ("demo_python", "python_template", Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"], pythonTemplateBaselineNixSource),
                ("demo_c", "c_template", Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]), cTemplateBaselineNixSource),
                ("demo_latex", "latex_template", defaultAllowedNixDifferenceKeys, latexTemplateBaselineNixSource)
              ]
        assertBool
          "Adds every supported package type with a custom description."
          (all (\case Right _ -> True; Left _ -> False) addResults)
        assertEqual
          "Persists add descriptions for every supported package type."
          (replicate (length scaffoldCases) (Just customDescription))
          packageDescriptions
        assertEqual
          "Accepts every generated custom description during metadata validation."
          []
          (haskellDescriptionIssues ++ rustDescriptionIssues ++ nixDescriptionIssues)
nixDescriptionInterpolationEscapingTest :: IO ()
nixDescriptionInterpolationEscapingTest = do
  let description :: String
      description = "Literal ${builtins.abort \"must not evaluate\"} text"
      renderedPythonTemplate = renderPythonTemplateBaselineNixSourceWith description defaultPythonPackageAttribute
  assertBool
    "Escapes Nix interpolation openers in scaffold descriptions."
    ("description = \"Literal \\${builtins.abort \\\"must not evaluate\\\"} text\";" `T.isInfixOf` renderedPythonTemplate)
  parseNixExprFromText renderedPythonTemplate >>= \case
    Right _ -> pure ()
    Left parseError -> assertFailure ("Escaped description did not produce valid Nix: " ++ parseError)
repositoryPackageNamingConventionTest :: IO ()
repositoryPackageNamingConventionTest =
  withTemporaryPackageRepository "repository-package-name-convention" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $ do
        let invalidPackageName :: FilePath
            invalidPackageName = "demo-html"
            packageRootDirectory = "packages" </> invalidPackageName
        forM_ (renderScaffoldFilesWith defaultCanonicalizationSettings HtmlPackage invalidPackageName Nothing) $ \scaffoldFile -> do
          let scaffoldPath = packageRootDirectory </> scaffoldFileRelativePath scaffoldFile
          createDirectoryIfMissing True (takeDirectory scaffoldPath)
          TIO.writeFile scaffoldPath (scaffoldFileContents scaffoldFile)
        repositoryStructureIssues <- checkRepositoryStructure
        assertBool
          "Applies package-type naming conventions to existing repository packages."
          ("packages/demo-html: package name must use snake_case for html packages" `elem` repositoryStructureIssues)
addPackageUnsupportedChecksTest :: IO ()
addPackageUnsupportedChecksTest =
  withTemporaryPackageRepository "unsupported-repository-check-creation" $
    \tempRepository ->
      withCurrentWorkingDirectory tempRepository $
        addPackageToCurrentRepositoryWith
          defaultCanonicalizationSettings
          HtmlPackage
          "demo"
          Nothing
          (Set.fromList [RepositoryCoverageCheck])
          >>= \addPackageResult ->
            assertEqual
              "Rejects unsupported checks when adding a package."
              (Left "unsupported checks for package type html: --coverage")
              addPackageResult
removePackageAndChecksTest :: IO ()
removePackageAndChecksTest =
  findExecutable "git" >>= \case
    Nothing -> pure ()
    Just _ ->
      withTemporaryPackageRepository "remove-package-and-checks" $ \tempRepository ->
        withCurrentWorkingDirectory tempRepository $ do
          initializeGitRepositoryFixture tempRepository
          addResult <-
            addPackageToCurrentRepositoryWith
              defaultCanonicalizationSettings
              PythonPackage
              "demo"
              Nothing
              (Set.fromList [RepositoryCoverageCheck, RepositoryPropertyTestingCheck])
          assertBool "The package fixture was created before removal." (either (const False) (not . null) addResult)
          removeResult <- removePackageFromCurrentRepository "demo"
          packageExists <- doesPathExist ("packages" </> "demo")
          coverageCheckExists <- doesPathExist ("checks" </> "demo_coverage")
          propertyTestingCheckExists <- doesPathExist ("checks" </> "demo_property_testing")
          assertEqual "Removing a package and its corresponding checks succeeds." (Right ()) removeResult
          assertBool "The package directory is removed." (not packageExists)
          assertBool "The package's coverage check is removed." (not coverageCheckExists)
          assertBool "The package's property-testing check is removed." (not propertyTestingCheckExists)
withTemporaryPackageRepository :: String -> (FilePath -> IO a) -> IO a
withTemporaryPackageRepository tempDirName action = do
  (temporaryPath, temporaryHandle) <- openTempFile "/tmp" tempDirName
  hClose temporaryHandle
  removeFile temporaryPath
  createDirectoryIfMissing True temporaryPath
  action temporaryPath `finally` removePathForcibly temporaryPath
withCurrentWorkingDirectory :: FilePath -> IO a -> IO a
withCurrentWorkingDirectory workingDirectory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory workingDirectory
  action `finally` setCurrentDirectory previousDirectory
withEnvironmentVariable :: String -> String -> IO a -> IO a
withEnvironmentVariable variableName variableValue action = do
  previousValue <- lookupEnv variableName
  setEnv variableName variableValue
  action
    `finally` case previousValue of
      Nothing -> unsetEnv variableName
      Just value -> setEnv variableName value
rustNixFixture :: String
rustNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.rustPlatform.buildRustPackage {\n"
    ++ "  cargoHash = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\";\n"
    ++ "}\n"
uncommentNixFixture :: String
uncommentNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  nativeBuildInputs = [ pkgs.autoPatchelfHook ];\n"
    ++ "  src = pkgs.fetchurl { url = \"https://github.com/Goldziher/${pname}/releases/download/v${version}/${pname}-x86_64-unknown-linux-gnu.tar.gz\"; sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
haskellNixFixture :: String
haskellNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.haskellPackages.mkDerivation rec {\n"
    ++ "  pname = baseNameOf ./.;\n"
    ++ "}\n"
pythonNixFixture :: String
pythonNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "let python = pkgs.python3; in\n"
    ++ "python.pkgs.buildPythonPackage rec {\n"
    ++ "  src = ./.;\n"
    ++ "}\n"
pythonLatexNixFixture :: String
pythonLatexNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "let python = pkgs.python3; in\n"
    ++ "python.pkgs.buildPythonPackage rec {\n"
    ++ "  installPhase = '' latexmk -cd -pdf tmp/ms.tex '';\n"
    ++ "}\n"
pythonPyPINixFixture :: String
pythonPyPINixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "let python = pkgs.python3; in\n"
    ++ "python.pkgs.buildPythonPackage rec {\n"
    ++ "  src = python.pkgs.fetchPypi { pname = \"x\"; version = \"1.0.0\"; hash = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
deployHostNixFixture :: String
deployHostNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeShellApplication {\n"
    ++ "  runtimeInputs = [ pkgs.opentofu ];\n"
    ++ "  text = \"echo agenix-shell\";\n"
    ++ "}\n"
cNixFixture :: String
cNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  buildPhase = '' cc -o ${pname} main.c -std=c89 '';\n"
    ++ "}\n"
latexNixFixture :: String
latexNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  buildPhase = '' latexmk -pdf ms.tex '';\n"
    ++ "}\n"
htmlNixFixture :: String
htmlNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeShellApplication rec {\n"
    ++ "  meta.description = \"An HTML, CSS, and JavaScript template package.\";\n"
    ++ "  name = baseNameOf ./.;\n"
    ++ "  runtimeInputs = [\n"
    ++ "    pkgs.http-server\n"
    ++ "  ];\n"
    ++ "  text = ''\n"
    ++ "    exec ${pkgs.http-server}/bin/http-server ${./.} \"$@\"\n"
    ++ "  '';\n"
    ++ "}\n"
cPackageVmCheckFixture :: String
cPackageVmCheckFixture =
  "{ inputs, pkgs, ... }:\n"
    ++ "pkgs.testers.runNixOSTest {\n"
    ++ "  name = builtins.baseNameOf ./.;\n"
    ++ "  nodes.machine = { pkgs, ... }: {\n"
    ++ "    environment.systemPackages = [\n"
    ++ "      (inputs.self.packages.${pkgs.stdenv.system}.${name}.overrideAttrs (_: {\n"
    ++ "        NIX_CFLAGS_COMPILE = \"-O1 -g3 -fsanitize=address,undefined\";\n"
    ++ "      }))\n"
    ++ "      pkgs.xterm\n"
    ++ "    ];\n"
    ++ "  };\n"
    ++ "  testScript = ''\n"
    ++ "    machine.succeed(\"true\")\n"
    ++ "  '';\n"
    ++ "}\n"
unknownNixFixture :: String
unknownNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeText \"x\" \"y\"\n"
binaryReleaseNixFixture :: String
binaryReleaseNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  sourceRoot = \".\";\n"
    ++ "  installPhase = ''\n"
    ++ "    install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname} $out/bin/${pname}\n"
    ++ "  '';\n"
    ++ "  src = pkgs.fetchurl { url = \"https://example.invalid/tool.tar.gz\"; sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
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
      "module Main (main) where",
      "import System.Exit (exitFailure)",
      "import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertEqual, runTestTT)",
      "",
      "renderMessage :: String",
      "renderMessage = \"Hello World Haskell\"",
      "",
      "runPackageTests :: IO ()",
      "runPackageTests = do",
      "  counts <- runTestTT hUnitPackageTests",
      "  if errors counts == 0 && failures counts == 0",
      "    then putStrLn \"test ... ok\"",
      "    else exitFailure",
      "",
      "hUnitPackageTests :: Test",
      "hUnitPackageTests =",
      "  TestList",
      "    [ TestCase $ do",
      "        assertEqual \"renders the sample message\" \"Hello World Haskell\" renderMessage",
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
      "      testGhc",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    mkdir -p \"$workspace/coverage/html\" \"$workspace/hpc\"",
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
      "    hpc markup \"$workspace/coverage/${packageName}.tix\" --hpcdir=\"$workspace/hpc\" --destdir=\"$workspace/coverage/html\"",
      "    hpc report \"$workspace/coverage/${packageName}.tix\" --hpcdir=\"$workspace/hpc\" | tee \"$workspace/coverage/summary.txt\"",
      "    touch \"$out\"",
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
      "      profileGhc",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    packageName=\"${packageName}\"",
      "    mkdir -p \"$workspace\"",
      "    cd \"$workspace\"",
      "    cat > \"$workspace/TestMain.hs\" <<EOF",
      "    module TestMain (main) where",
      "    import qualified Main as PackageMain",
      "    main :: IO ()",
      "    main = PackageMain.runPackageTests",
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
      "    \"$workspace/$packageName\" +RTS -p -RTS",
      "    cat \"$workspace/$packageName.prof\"",
      "    touch \"$out\"",
      "  ''"
    ]
haskellPropertyTestingCheckBaselineNixSource :: T.Text
haskellPropertyTestingCheckBaselineNixSource =
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
      "  packageName = pkgs.lib.removeSuffix \"-property-testing\" checkName;",
      "  testGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = [",
      "      testGhc",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    packageName=\"${packageName}\"",
      "    mkdir -p \"$workspace\"",
      "    cd \"$workspace\"",
      "    cat > TestMain.hs <<EOF",
      "    module TestMain (main) where",
      "    import qualified Main as PackageMain",
      "    main :: IO ()",
      "    main = PackageMain.runPackageTests",
      "    EOF",
      "    \"${testGhc}/bin/ghc\" \\",
      "      -O2 \\",
      "      -main-is TestMain.main \\",
      "      -i\"$src\" \\",
      "      -outputdir \"$workspace\" \\",
      "      -odir \"$workspace\" \\",
      "      -hidir \"$workspace\" \\",
      "      -o \"$workspace/$packageName\" \\",
      "      \"$workspace/TestMain.hs\" \\",
      "      \"$src/Main.hs\"",
      "    \"$workspace/$packageName\"",
      "    touch \"$out\"",
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
      "  pythonEnv = packageDrv.python.withPackages (",
      "    _:",
      "    packageDrv.propagatedBuildInputs",
      "    ++ [",
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
      "    coverageDir=\"$PWD/coverage\"",
      "    mkdir -p \"$coverageDir\"",
      "    python -m pytest --cov=\"$src\" --cov-report term --cov-report \"html:$coverageDir/html\" \"$src/main.py\"",
      "    touch \"$out\"",
      "  ''"
    ]
pythonProfileCheckBaselineNixSource :: T.Text
pythonProfileCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"_profile\" checkName;",
      "  pythonEnv = packageDrv.python.withPackages (",
      "    _:",
      "    packageDrv.propagatedBuildInputs",
      "    ++ [",
      "      packageDrv.python.pkgs.pyinstrument",
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
      "    PYTHONWARNINGS=error pyinstrument \"$src/main.py\"",
      "    touch \"$out\"",
      "  ''"
    ]
pythonPropertyTestingCheckBaselineNixSource :: T.Text
pythonPropertyTestingCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"_property_testing\" checkName;",
      "  pythonEnv = packageDrv.python.withPackages (",
      "    _:",
      "    packageDrv.propagatedBuildInputs",
      "    ++ [",
      "      packageDrv.python.pkgs.hypothesis",
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
      "    export PYTHONWARNINGS=error",
      "    workspace=\"$PWD/workspace\"",
      "    rm -rf \"$workspace\"",
      "    mkdir -p \"$workspace\"",
      "    cp -R --no-preserve=mode \"$src\"/. \"$workspace\"",
      "    cd \"$workspace\"",
      "    PYTHONPATH=\"$workspace\" python -m pytest -v main.py -k property",
      "    touch \"$out\"",
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
      "    cargo llvm-cov",
      "    touch \"$out\"",
      "  ''"
    ]
rustMutationTestingCheckBaselineNixSource :: T.Text
rustMutationTestingCheckBaselineNixSource =
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
      "  packageName = pkgs.lib.removeSuffix \"-mutation-testing\" checkName;",
      "  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = rustBaseInputs ++ [",
      "      pkgs.cargo-mutants",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    workspace=\"$PWD/workspace\"",
      "    cp -R --no-preserve=mode \"$src\" \"$workspace\"",
      "    install -Dm644 \"${cargoDeps}/.cargo/config.toml\" \"$workspace/.cargo/config.toml\"",
      "    substituteInPlace \"$workspace/.cargo/config.toml\" \\",
      "      --replace-fail \"@vendor@\" \"${cargoDeps}\"",
      "    cd \"$workspace\"",
      "    cargo mutants || mutation_status=$?",
      "    if [ \"''${mutation_status:-0}\" != 0 ] && [ \"''${mutation_status:-0}\" != 2 ]; then",
      "      exit \"$mutation_status\"",
      "    fi",
      "    touch \"$out\"",
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
      "  checkName = builtins.baseNameOf ./.;",
      "  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};",
      "  packageName = pkgs.lib.removeSuffix \"-profile\" checkName;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = [",
      "      packageDrv",
      "      pkgs.perf",
      "    ];",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    workspace=\"$PWD/workspace\"",
      "    mkdir -p \"$workspace\"",
      "    printf 'line1\\n\\nline2\\n' > \"$workspace/test.txt\"",
      "    run_cmd() {",
      "      remove-empty-lines \"$workspace\"",
      "    }",
      "    if perf stat -e cpu-clock true >/dev/null 2>&1; then",
      "      perf record --no-buildid-mmap --call-graph dwarf -e cpu-clock -o perf.data -- \\",
      "        run_cmd",
      "      perf report --stdio -i perf.data",
      "    else",
      "      echo \"perf is unavailable in this environment; running without profiling.\"",
      "      run_cmd",
      "    fi",
      "    touch \"$out\"",
      "  ''"
    ]
rustPropertyTestingCheckBaselineNixSource :: T.Text
rustPropertyTestingCheckBaselineNixSource =
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
      "  packageName = pkgs.lib.removeSuffix \"-property-testing\" checkName;",
      "  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = rustBaseInputs;",
      "    src = ../.. + \"/packages/${packageName}\";",
      "  }",
      "  ''",
      "    workspace=\"$PWD/workspace\"",
      "    cp -R --no-preserve=mode \"$src\" \"$workspace\"",
      "    install -Dm644 \"${cargoDeps}/.cargo/config.toml\" \"$workspace/.cargo/config.toml\"",
      "    substituteInPlace \"$workspace/.cargo/config.toml\" \\",
      "      --replace-fail \"@vendor@\" \"${cargoDeps}\"",
      "    cd \"$workspace\"",
      "    cargo test --locked",
      "    touch \"$out\"",
      "  ''"
    ]
htmlTemplateCheckBaselineNixSource :: T.Text
htmlTemplateCheckBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "pkgs.testers.runNixOSTest rec {",
      "  name = builtins.baseNameOf ./.;",
      "  nodes.machine.environment.systemPackages = [",
      "    inputs.self.packages.${pkgs.stdenv.system}.${name}",
      "    pkgs.curl",
      "  ];",
      "  testScript = ''",
      "    machine.succeed(\"${name} -p 8080 >/tmp/${name}.log 2>&1 &\")",
      "    machine.wait_until_succeeds(\"curl -fsS http://127.0.0.1:8080 | grep -F 'Hello World!'\")",
      "  '';",
      "}"
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
