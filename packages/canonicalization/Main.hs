{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main, runPackageTests) where
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, nub, sort, sortBy, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
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
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, getCurrentDirectory, getHomeDirectory, listDirectory, removeFile, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.FilePath ((<.>), (</>))
import System.FilePath.Posix (splitDirectories, takeBaseName, takeDirectory, takeFileName)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertBool, assertEqual, assertFailure, runTestTT)
import Test.QuickCheck qualified as QC
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
type DefaultNixTemplateSpec :: Type
data DefaultNixTemplateSpec = DefaultNixTemplateSpec
  { defaultNixTemplateName :: FilePath,
    defaultNixTemplateMatches :: FilePath -> String -> IO Bool,
    defaultNixTemplateAllowedDifferenceKeys :: Set.Set T.Text,
    defaultNixTemplateBaselineSource :: Maybe T.Text
  }
type CheckDefaultNixTemplateSpec :: Type
data CheckDefaultNixTemplateSpec = CheckDefaultNixTemplateSpec
  { checkDefaultNixTemplateName :: FilePath,
    checkDefaultNixTemplateMatches :: FilePath -> String -> IO Bool,
    checkDefaultNixTemplateAllowedDifferenceKeys :: Set.Set T.Text,
    checkDefaultNixTemplateBaselineSource :: T.Text,
    checkDefaultNixTemplateComparisonMode :: CheckDefaultNixTemplateComparisonMode
  }
type CheckDefaultNixTemplateComparisonMode :: Type
data CheckDefaultNixTemplateComparisonMode
  = ExactCheckTemplate
  | StructuralCPackageVmCheck
defaultNixTemplateSpecs :: [DefaultNixTemplateSpec]
defaultNixTemplateSpecs =
  [ DefaultNixTemplateSpec
      { defaultNixTemplateName = "haskell_package_baseline",
        defaultNixTemplateMatches = \_ nixSource -> pure ("haskellPackages.mkDerivation" `isInfixOf` nixSource),
        defaultNixTemplateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        defaultNixTemplateBaselineSource = Just haskellTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "rust_package_baseline",
        defaultNixTemplateMatches = \_ nixSource -> pure ("rustPlatform.buildRustPackage" `isInfixOf` nixSource),
        defaultNixTemplateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedNixDifferenceKeys,
        defaultNixTemplateBaselineSource = Just rustTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "html_template",
        defaultNixTemplateMatches = \_ nixSource -> pure ("writeShellApplication" `isInfixOf` nixSource && "http-server" `isInfixOf` nixSource),
        defaultNixTemplateAllowedDifferenceKeys = Set.insert "text" defaultAllowedNixDifferenceKeys,
        defaultNixTemplateBaselineSource = Just htmlTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "python_latex_template",
        defaultNixTemplateMatches = matchesPythonLatexTemplate,
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "version"],
        defaultNixTemplateBaselineSource = Just pythonLatexTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "python_pypi_application_template",
        defaultNixTemplateMatches = matchesPythonPypiApplicationTemplate,
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        defaultNixTemplateBaselineSource = Just pythonPypiApplicationTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "python_pypi_template",
        defaultNixTemplateMatches = matchesPythonPypiTemplate,
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["format", "installCheckPhase", "meta", "nativeBuildInputs", "propagatedBuildInputs", "python", "src", "version"],
        defaultNixTemplateBaselineSource = Just pythonPypiTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "binary_release_template",
        defaultNixTemplateMatches = matchesBinaryReleaseTemplate,
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "src", "version"],
        defaultNixTemplateBaselineSource = Just binaryReleaseTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "python_template",
        defaultNixTemplateMatches = \_ nixSource -> pure ("buildPythonPackage" `isInfixOf` nixSource),
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["meta", "propagatedBuildInputs", "python", "shellHook", "version"],
        defaultNixTemplateBaselineSource = Just pythonTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "deploy_host_template",
        defaultNixTemplateMatches = \_ nixSource ->
          pure
            ( "writeShellApplication" `isInfixOf` nixSource
                && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource)
            ),
        defaultNixTemplateAllowedDifferenceKeys = Set.insert "meta.description" defaultAllowedNixDifferenceKeys,
        defaultNixTemplateBaselineSource = Just deployHostTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "latex_template",
        defaultNixTemplateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "latexmk -pdf ms.tex" `isInfixOf` nixSource
            ),
        defaultNixTemplateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
        defaultNixTemplateBaselineSource = Just latexTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "c_template",
        defaultNixTemplateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "cc -o ${pname} main.c -std=c89" `isInfixOf` nixSource
            ),
        defaultNixTemplateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]),
        defaultNixTemplateBaselineSource = Just cTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "uncomment_template",
        defaultNixTemplateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "autoPatchelfHook" `isInfixOf` nixSource
                && "Goldziher" `isInfixOf` nixSource
            ),
        defaultNixTemplateAllowedDifferenceKeys = Set.union defaultAllowedNixDifferenceKeys (Set.fromList ["pname", "src"]),
        defaultNixTemplateBaselineSource = Just uncommentTemplateBaselineNixSource
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
matchesPythonPypiTemplateLike :: String -> FilePath -> String -> IO Bool
matchesPythonPypiTemplateLike buildFunction _ nixSource =
  pure
    ( buildFunction `isInfixOf` nixSource
        && not ("src = ./.;" `isInfixOf` nixSource)
        && ("fetchPypi" `isInfixOf` nixSource || "fetchurl" `isInfixOf` nixSource)
    )
matchesPythonPypiApplicationTemplate :: FilePath -> String -> IO Bool
matchesPythonPypiApplicationTemplate = matchesPythonPypiTemplateLike "buildPythonApplication"
matchesPythonPypiTemplate :: FilePath -> String -> IO Bool
matchesPythonPypiTemplate = matchesPythonPypiTemplateLike "buildPythonPackage"
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
defaultNixTemplateSpecByName :: FilePath -> Maybe DefaultNixTemplateSpec
defaultNixTemplateSpecByName defaultNixTemplateNameToFind = find ((== defaultNixTemplateNameToFind) . defaultNixTemplateName) defaultNixTemplateSpecs
checkDefaultNixTemplateSpecs :: [CheckDefaultNixTemplateSpec]
checkDefaultNixTemplateSpecs =
  [ CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "haskell_coverage_check",
        checkDefaultNixTemplateMatches = matchesHaskellCoverageCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = haskellCoverageCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "haskell_profile_check",
        checkDefaultNixTemplateMatches = matchesHaskellProfileCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = haskellProfileCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "haskell_property_testing_check",
        checkDefaultNixTemplateMatches = matchesHaskellPropertyTestingCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = haskellPropertyTestingCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "python_coverage_check",
        checkDefaultNixTemplateMatches = matchesPythonCoverageCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = pythonCoverageCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "python_profile_check",
        checkDefaultNixTemplateMatches = matchesPythonProfileCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = pythonProfileCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "python_property_testing_check",
        checkDefaultNixTemplateMatches = matchesPythonPropertyTestingCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = pythonPropertyTestingCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "python_mutation_testing_check",
        checkDefaultNixTemplateMatches = matchesPythonMutationTestingCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = pythonMutationTestingCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "rust_coverage_check",
        checkDefaultNixTemplateMatches = matchesRustCoverageCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = rustCoverageCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "rust_profile_check",
        checkDefaultNixTemplateMatches = matchesRustProfileCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = rustProfileCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "rust_property_testing_check",
        checkDefaultNixTemplateMatches = matchesRustPropertyTestingCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = rustPropertyTestingCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "rust_mutation_testing_check",
        checkDefaultNixTemplateMatches = matchesRustMutationTestingCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = rustMutationTestingCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "html_template_check",
        checkDefaultNixTemplateMatches = \checkName _ -> pure (checkName == "html_template"),
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = htmlTemplateCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "c_template_check",
        checkDefaultNixTemplateMatches = \checkName _ -> pure (checkName == "c_template"),
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "c_package_vm_check",
        checkDefaultNixTemplateMatches = matchesCPackageVmCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = cTemplateCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = StructuralCPackageVmCheck
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "default_vm_with_disko_check",
        checkDefaultNixTemplateMatches = matchesDefaultVmWithDiskoCheck,
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = defaultVmWithDiskoCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      },
    CheckDefaultNixTemplateSpec
      { checkDefaultNixTemplateName = "host_default_check",
        checkDefaultNixTemplateMatches = \checkName _ -> pure (checkName == "host_default"),
        checkDefaultNixTemplateAllowedDifferenceKeys = Set.empty,
        checkDefaultNixTemplateBaselineSource = hostDefaultCheckBaselineNixSource,
        checkDefaultNixTemplateComparisonMode = ExactCheckTemplate
      }
  ]
checkDefaultNixTemplateSpecByName :: FilePath -> Maybe CheckDefaultNixTemplateSpec
checkDefaultNixTemplateSpecByName checkDefaultNixTemplateNameToFind = find ((== checkDefaultNixTemplateNameToFind) . checkDefaultNixTemplateName) checkDefaultNixTemplateSpecs
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
matchesPythonMutationTestingCheck :: FilePath -> String -> IO Bool
matchesPythonMutationTestingCheck = matchesCheckNameSuffixAndSourceContains "_mutation_testing" ["cosmic-ray"]
matchesRustCoverageCheck :: FilePath -> String -> IO Bool
matchesRustCoverageCheck = matchesCheckNameSuffixAndSourceContains "-coverage" ["cargo llvm-cov"]
matchesRustProfileCheck :: FilePath -> String -> IO Bool
matchesRustProfileCheck = matchesCheckNameSuffixAndSourceContains "-profile" ["pkgs.perf", "perf record"]
matchesRustPropertyTestingCheck :: FilePath -> String -> IO Bool
matchesRustPropertyTestingCheck = matchesCheckNameSuffixAndSourceContains "-property-testing" ["cargo test --locked"]
matchesRustMutationTestingCheck :: FilePath -> String -> IO Bool
matchesRustMutationTestingCheck = matchesCheckNameSuffixAndSourceContains "-mutation-testing" ["cargo mutants"]
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
main :: IO ()
main = getArgs >>= runCli defaultCanonicalizationSettings
runCli :: CanonicalizationSettings -> [String] -> IO ()
runCli canonicalizationSettings commandLineArgs =
  case commandLineArgs of
    ["check-repository"] ->
      runInGitRepositoryRoot "." $
        collectRepositoryComplianceWith canonicalizationSettings >>= \case
          Left (checkPhaseName, checkPhaseIssues) -> do
            reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
            exitFailure
          Right _ -> pure ()
    ["check-repository", repositoryDirectory] ->
      runInGitRepositoryRoot repositoryDirectory $
        collectRepositoryComplianceWith canonicalizationSettings >>= \case
          Left (checkPhaseName, checkPhaseIssues) -> do
            reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
            exitFailure
          Right _ -> pure ()
    ["describe-repository", "--json"] -> runInGitRepositoryRoot "." (runDescribeRepositoryJsonModeWith canonicalizationSettings)
    ["describe-repository", "--json", repositoryDirectory] -> runInGitRepositoryRoot repositoryDirectory (runDescribeRepositoryJsonModeWith canonicalizationSettings)
    ["describe-repository"] -> runInGitRepositoryRoot "." (runDescribeRepositoryModeWith canonicalizationSettings)
    ["describe-repository", repositoryDirectory] -> runInGitRepositoryRoot repositoryDirectory (runDescribeRepositoryModeWith canonicalizationSettings)
    ["check-gitmodules"] -> runCheckGitSubmodulesMode
    createRepositoryArgs ->
      case parseCreateRepositoryArgs createRepositoryArgs of
        Just (repositoryDirectory, packageKindName, packageName, requestedCheckKinds) ->
          runInGitRepositoryRoot repositoryDirectory (runCreateRepositoryMode canonicalizationSettings packageKindName packageName requestedCheckKinds)
        Nothing ->
          case createRepositoryArgs of
            "create-repository" : _ -> printUsageAndExit
            _ ->
              case parseCreatePackageArgs createRepositoryArgs of
                Just (repositoryDirectory, packageKindName, packageName, packageDescription) ->
                  runInGitRepositoryRoot repositoryDirectory (runCreatePackageMode canonicalizationSettings packageKindName packageName packageDescription)
                Nothing -> printUsageAndExit
printUsageAndExit :: IO a
printUsageAndExit = do
  putStrLn "Usage: canonicalization check-repository [git-directory]"
  putStrLn "       canonicalization describe-repository [git-directory]"
  putStrLn "       canonicalization describe-repository --json [git-directory]"
  putStrLn "       canonicalization create-package [git-directory] <package-type> <package-name> [description]"
  putStrLn "       canonicalization create-repository [git-directory] <package-type> <package-name> [--check] [--coverage] [--profile] [--property-testing] [--mutation-testing]"
  putStrLn "       canonicalization check-gitmodules"
  exitFailure
parseCreatePackageArgs :: [String] -> Maybe (FilePath, String, FilePath, Maybe String)
parseCreatePackageArgs commandLineArgs =
  case commandLineArgs of
    "create-package" : firstArgument : secondArgument : remainingArguments
      | isJust (parseSupportedCreatePackageKind firstArgument) ->
          Just (".", firstArgument, secondArgument, case remainingArguments of [] -> Nothing; args -> Just (unwords args))
      | isJust (parseSupportedCreatePackageKind secondArgument) ->
          case remainingArguments of
            packageName : packageDescriptionArguments ->
              Just (firstArgument, secondArgument, packageName, case packageDescriptionArguments of [] -> Nothing; args -> Just (unwords args))
            [] -> Nothing
    _ -> Nothing
parseCreateRepositoryArgs :: [String] -> Maybe (FilePath, String, FilePath, Set.Set RepositoryCheckKind)
parseCreateRepositoryArgs commandLineArgs = do
  (repositoryDirectory, packageKindName, packageName, flagArguments) <-
    case commandLineArgs of
      ["create-repository", packageKindName, packageName] ->
        Just (".", packageKindName, packageName, [])
      "create-repository" : packageKindName : packageName : remainingArgs
        | all ("--" `isPrefixOf`) remainingArgs ->
            Just (".", packageKindName, packageName, remainingArgs)
      "create-repository" : repositoryDirectory : packageKindName : packageName : remainingArgs ->
        Just (repositoryDirectory, packageKindName, packageName, remainingArgs)
      _ -> Nothing
  requestedCheckKinds <-
    Set.fromList
      <$> mapM
        ( \repositoryCheckFlag ->
            lookup
              repositoryCheckFlag
              [ ("--check", RepositoryDefaultCheck),
                ("--coverage", RepositoryCoverageCheck),
                ("--profile", RepositoryProfileCheck),
                ("--property-testing", RepositoryPropertyTestingCheck),
                ("--mutation-testing", RepositoryMutationTestingCheck)
              ]
        )
        flagArguments
  pure (repositoryDirectory, packageKindName, packageName, requestedCheckKinds)
runInGitRepositoryRoot :: FilePath -> IO a -> IO a
runInGitRepositoryRoot repositoryDirectory action = do
  isDirectory <- doesDirectoryExist repositoryDirectory
  unless isDirectory $ do
    putStrLn ("not a directory: " ++ repositoryDirectory)
    exitFailure
  (insideWorkTreeExit, insideWorkTreeStdout, _insideWorkTreeStderr) <- readProcessWithExitCode "git" ["-C", repositoryDirectory, "rev-parse", "--is-inside-work-tree"] ""
  unless (insideWorkTreeExit == ExitSuccess && T.unpack (T.strip (T.pack insideWorkTreeStdout)) == "true") $ do
    putStrLn ("not a git directory: " ++ repositoryDirectory)
    exitFailure
  (repositoryRootExit, repositoryRootStdout, _repositoryRootStderr) <- readProcessWithExitCode "git" ["-C", repositoryDirectory, "rev-parse", "--show-toplevel"] ""
  unless (repositoryRootExit == ExitSuccess) $ do
    putStrLn ("not a git directory: " ++ repositoryDirectory)
    exitFailure
  canonicalInputDirectory <- canonicalizePath repositoryDirectory
  canonicalRepositoryRoot <- canonicalizePath (T.unpack (T.strip (T.pack repositoryRootStdout)))
  unless (canonicalInputDirectory == canonicalRepositoryRoot) $ do
    putStrLn ("not a git repository root directory: " ++ repositoryDirectory)
    exitFailure
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory canonicalInputDirectory
  action `finally` setCurrentDirectory previousDirectory
runCreatePackageMode :: CanonicalizationSettings -> String -> FilePath -> Maybe String -> IO ()
runCreatePackageMode canonicalizationSettings packageKindName packageName packageDescription =
  case parseSupportedCreatePackageKind packageKindName of
    Nothing -> do
      putStrLn ("unsupported package type: " ++ packageKindName)
      putStrLn ("supported package types: " ++ intercalate ", " (map fst supportedCreatePackageKinds))
      exitFailure
    Just packageKind ->
      case validateCreatePackageName packageName of
        Just validationError -> do
          putStrLn validationError
          exitFailure
        Nothing -> do
          createResult <- createPackageInCurrentRepositoryWith canonicalizationSettings packageKind packageName packageDescription
          case createResult of
            Left createError -> do
              putStrLn createError
              exitFailure
            Right createdFilePaths -> do
              putStrLn ("created package packages/" ++ packageName ++ " (" ++ renderPackageKind packageKind ++ ")")
              putStrLn ("created " ++ show (length createdFilePaths) ++ " files")
runCreateRepositoryMode :: CanonicalizationSettings -> String -> FilePath -> Set.Set RepositoryCheckKind -> IO ()
runCreateRepositoryMode canonicalizationSettings packageKindName packageName requestedCheckKinds =
  case parseSupportedCreatePackageKind packageKindName of
    Nothing -> do
      putStrLn ("unsupported package type: " ++ packageKindName)
      putStrLn ("supported package types: " ++ intercalate ", " (map fst supportedCreatePackageKinds))
      exitFailure
    Just packageKind ->
      case validateCreatePackageName packageName of
        Just validationError -> do
          putStrLn validationError
          exitFailure
        Nothing -> do
          createResult <- createRepositoryInCurrentRepositoryWith canonicalizationSettings packageKind packageName requestedCheckKinds
          case createResult of
            Left createError -> do
              putStrLn createError
              exitFailure
            Right createdFilePaths -> do
              putStrLn ("created repository scaffold for packages/" ++ packageName ++ " (" ++ renderPackageKind packageKind ++ ")")
              putStrLn ("created " ++ show (length createdFilePaths) ++ " files")
collectRepositoryCompliance :: IO (Either (String, [String]) RepositoryComplianceSuccess)
collectRepositoryCompliance = collectRepositoryComplianceWith defaultCanonicalizationSettings
collectRepositoryComplianceWith :: CanonicalizationSettings -> IO (Either (String, [String]) RepositoryComplianceSuccess)
collectRepositoryComplianceWith canonicalizationSettings = do
  repositoryStructureIssues <- checkRepositoryStructure
  if not (null repositoryStructureIssues)
    then pure (Left ("directory-structure", repositoryStructureIssues))
    else do
      packageNames <- listSubdirectoryNames "packages"
      packageChecks <- forM packageNames (checkPackageWith canonicalizationSettings [])
      checkNames <- listSubdirectoryNames "checks"
      checkComplianceIssues <- concat <$> forM checkNames (checkCheckWith canonicalizationSettings)
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
  putStrLn ("check-repository failed at phase: " ++ checkPhaseName)
  forM_ checkPhaseIssues $ \issue ->
    putStrLn ("- [" ++ checkPhaseName ++ "] " ++ issue)
  case checkPhaseName of
    "directory-structure" ->
      putStrLn "hint: fix directory and required-file layout under packages/, hosts/, checks/, and repository root."
    "file-compliance" ->
      putStrLn "hint: align package files with the expected internal templates and language-specific policy checks."
    _ -> pure ()
runCheckGitSubmodulesMode :: IO ()
runCheckGitSubmodulesMode = do
  homeDirectory <- getHomeDirectory
  let gitSubmodulesFilePath = homeDirectory </> ".gitmodules"
  fileExists <- doesFileExist gitSubmodulesFilePath
  unless fileExists $ do
    putStrLn ("missing file: " ++ gitSubmodulesFilePath)
    exitFailure
  gitSubmodulesContents <- T.unpack <$> TIO.readFile gitSubmodulesFilePath
  let gitSubmodulePathEntries = parseGitSubmodulePathEntries gitSubmodulesContents
      gitSubmoduleRepositories = map (buildGitSubmoduleRepository homeDirectory) gitSubmodulePathEntries
      invalidGitSubmodulePathEntries =
        [ gitSubmoduleRepositoryPathEntry gitSubmoduleRepository
        | gitSubmoduleRepository <- gitSubmoduleRepositories,
          not (gitSubmoduleRepositoryIsCompatible gitSubmoduleRepository)
        ]
  if null invalidGitSubmodulePathEntries
    then putStrLn "all .gitmodules path entries comply with go-style naming (<host>/<owner>/<repo>)"
    else do
      forM_ invalidGitSubmodulePathEntries $ \invalidGitSubmodulePathEntry ->
        putStrLn (invalidGitSubmodulePathEntry ++ ": must be exactly <host>/<owner>/<repo>")
      exitFailure
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
runDescribeRepositoryModeWith :: CanonicalizationSettings -> IO ()
runDescribeRepositoryModeWith canonicalizationSettings =
  collectRepositoryComplianceWith canonicalizationSettings >>= \case
    Left (checkPhaseName, checkPhaseIssues) -> do
      reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
      exitFailure
    Right (RepositoryComplianceSuccess packageNames checkNames) -> do
      let repositoryCheckNames = Set.fromList checkNames
      packageSummaries <- forM packageNames (summarizeRepositoryPackage repositoryCheckNames)
      putStr (renderRepositoryPackageSummariesText packageSummaries)
runDescribeRepositoryJsonModeWith :: CanonicalizationSettings -> IO ()
runDescribeRepositoryJsonModeWith canonicalizationSettings =
  collectRepositoryComplianceWith canonicalizationSettings >>= \case
    Left (checkPhaseName, checkPhaseIssues) -> do
      reportCheckRepositoryFailures checkPhaseName checkPhaseIssues
      exitFailure
    Right (RepositoryComplianceSuccess packageNames checkNames) -> do
      let repositoryCheckNames = Set.fromList checkNames
      packageSummaries <- forM packageNames (summarizeRepositoryPackage repositoryCheckNames)
      putStr (renderRepositoryPackageSummariesJson packageSummaries)
renderRepositoryPackageSummariesText :: [RepositoryPackageSummary] -> String
renderRepositoryPackageSummariesText packageSummaries =
  intercalate
    "\n\n"
    [ unlines
        ( let packageChecks = repositoryPackageChecks packageSummary
           in [ "package: " ++ repositoryPackageName packageSummary,
                "packageType: " ++ repositoryPackageType packageSummary,
                "description: " ++ fromMaybe "(none)" (repositoryPackageDescription packageSummary),
                "checks:"
              ]
                ++ [ "  " ++ checkName ++ ": " ++ bool "false" "true" isEnabled
                   | (checkName, isEnabled) <-
                       [ ("check", repositoryPackageHasCheck packageChecks),
                         ("coverage", repositoryPackageHasCoverageCheck packageChecks),
                         ("profile", repositoryPackageHasProfileCheck packageChecks),
                         ("property-testing", repositoryPackageHasPropertyTestingCheck packageChecks),
                         ("mutation-testing", repositoryPackageHasMutationTestingCheck packageChecks)
                       ]
                   ]
                ++ ["tests:"]
                ++ case repositoryPackageTestNames packageSummary of
                  [] -> ["  (none)"]
                  testNames -> ["  - " ++ testName | testName <- testNames]
        )
    | packageSummary <- packageSummaries
    ]
    ++ if null packageSummaries then "" else "\n"
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
      | (checkName, isEnabled) <-
          [ ("check", repositoryPackageHasCheck packageChecks),
            ("coverage", repositoryPackageHasCoverageCheck packageChecks),
            ("profile", repositoryPackageHasProfileCheck packageChecks),
            ("property-testing", repositoryPackageHasPropertyTestingCheck packageChecks),
            ("mutation-testing", repositoryPackageHasMutationTestingCheck packageChecks)
          ]
      ]
    ++ " }"
summarizeRepositoryPackage :: Set.Set FilePath -> FilePath -> IO RepositoryPackageSummary
summarizeRepositoryPackage repositoryCheckNames packageName = do
  packageKind <- detectPackageKindForPackage packageName
  let packageRoot = "packages" </> packageName
  repositoryPackageDescriptionValue <-
    if packageKind `elem` [PythonPackage, PythonLatexPackage, PythonPypiPackage]
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
summarizeRepositoryPackageChecks :: Set.Set FilePath -> PackageKind -> FilePath -> RepositoryPackageChecksSummary
summarizeRepositoryPackageChecks repositoryCheckNames packageKind packageName =
  RepositoryPackageChecksSummary
    { repositoryPackageHasCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryDefaultCheck),
      repositoryPackageHasCoverageCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryCoverageCheck),
      repositoryPackageHasProfileCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryProfileCheck),
      repositoryPackageHasPropertyTestingCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryPropertyTestingCheck),
      repositoryPackageHasMutationTestingCheck = maybe False (`Set.member` repositoryCheckNames) (repositoryCheckNameForKind packageKind packageName RepositoryMutationTestingCheck)
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
  let projectSection = extractTomlSection "project" pyprojectTomlContents
      poetrySection = extractTomlSection "tool.poetry" pyprojectTomlContents
   in T.unpack <$> lookupTomlString "description" projectSection
        <|> T.unpack <$> lookupTomlString "description" poetrySection
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
    PythonPypiPackage -> "python-pypi"
    CPackage -> "c"
    TerraformPackage -> "terraform"
    LatexPackage -> "latex"
    BinaryReleasePackage -> "binary-release"
    UnknownPackage -> "unknown"
supportedCreatePackageKinds :: [(String, PackageKind)]
supportedCreatePackageKinds =
  [ ("haskell", HaskellPackage),
    ("rust", RustPackage),
    ("html", HtmlPackage),
    ("python", PythonPackage),
    ("python-latex", PythonLatexPackage),
    ("c", CPackage),
    ("latex", LatexPackage)
  ]
parseSupportedCreatePackageKind :: String -> Maybe PackageKind
parseSupportedCreatePackageKind packageKindName = lookup packageKindName supportedCreatePackageKinds
validateCreatePackageName :: FilePath -> Maybe String
validateCreatePackageName packageName
  | null packageName = Just "package name must not be empty"
  | packageName `elem` [".", ".."] = Just "package name must not be '.' or '..'"
  | any isPathSeparator packageName = Just "package name must not contain path separators"
  | not (all isAllowedPackageNameCharacter packageName) = Just "package name must contain only letters, digits, '.', '-', or '_'"
  | otherwise = Nothing
  where
    isAllowedPackageNameCharacter character = isAlphaNum character || character `elem` ("._-" :: String)
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
createPackageInCurrentRepository :: PackageKind -> FilePath -> Maybe String -> IO (Either String [FilePath])
createPackageInCurrentRepository = createPackageInCurrentRepositoryWith defaultCanonicalizationSettings
createPackageInCurrentRepositoryWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Maybe String -> IO (Either String [FilePath])
createPackageInCurrentRepositoryWith canonicalizationSettings packageKind packageName packageDescription = do
  let packageRootDirectory = "packages" </> packageName
      scaffoldFiles = renderScaffoldFilesWith canonicalizationSettings packageKind packageName packageDescription
  if null scaffoldFiles
    then pure (Left ("unsupported package type: " ++ renderPackageKind packageKind))
    else do
      packageRootExists <- doesPathExist packageRootDirectory
      if packageRootExists
        then pure (Left ("package already exists: " ++ packageRootDirectory))
        else do
          createDirectoryIfMissing True packageRootDirectory
          forM_ scaffoldFiles $ \scaffoldFile -> do
            let relativePath = scaffoldFileRelativePath scaffoldFile
                absolutePath = packageRootDirectory </> relativePath
            createDirectoryIfMissing True (takeDirectory absolutePath)
            TIO.writeFile absolutePath (scaffoldFileContents scaffoldFile)
          pure (Right [packageRootDirectory </> scaffoldFileRelativePath scaffoldFile | scaffoldFile <- scaffoldFiles])
createRepositoryInCurrentRepository :: PackageKind -> FilePath -> Set.Set RepositoryCheckKind -> IO (Either String [FilePath])
createRepositoryInCurrentRepository = createRepositoryInCurrentRepositoryWith defaultCanonicalizationSettings
createRepositoryInCurrentRepositoryWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Set.Set RepositoryCheckKind -> IO (Either String [FilePath])
createRepositoryInCurrentRepositoryWith canonicalizationSettings packageKind packageName requestedCheckKinds =
  case validateRepositoryCheckSelection packageKind requestedCheckKinds of
    Just validationError -> pure (Left validationError)
    Nothing -> do
      let packageRootDirectory = "packages" </> packageName
          packageScaffoldFiles =
            [ RepositoryScaffoldFile
                (packageRootDirectory </> scaffoldFileRelativePath scaffoldFile)
                (scaffoldFileContents scaffoldFile)
            | scaffoldFile <- renderScaffoldFilesWith canonicalizationSettings packageKind packageName Nothing
            ]
          checkScaffoldFiles = renderRepositoryCheckScaffoldFilesWith canonicalizationSettings packageKind packageName requestedCheckKinds
          scaffoldFiles = packageScaffoldFiles ++ checkScaffoldFiles
          scaffoldPaths = map repositoryScaffoldFilePath scaffoldFiles
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
                      RepositoryDefaultCheck -> "--check"
                      RepositoryCoverageCheck -> "--coverage"
                      RepositoryProfileCheck -> "--profile"
                      RepositoryPropertyTestingCheck -> "--property-testing"
                      RepositoryMutationTestingCheck -> "--mutation-testing"
                  | repositoryCheckKind <- unsupportedCheckKinds
                  ]
            )
renderRepositoryCheckScaffoldFilesWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Set.Set RepositoryCheckKind -> [RepositoryScaffoldFile]
renderRepositoryCheckScaffoldFilesWith canonicalizationSettings packageKind packageName requestedCheckKinds =
  catMaybes
    [ do
        checkName <- repositoryCheckNameForKind packageKind packageName requestedCheckKind
        checkDefaultNixSource <- repositoryCheckBaselineSourceWith canonicalizationSettings packageKind requestedCheckKind
        pure
          ( RepositoryScaffoldFile
              ("checks" </> checkName </> "default.nix")
              checkDefaultNixSource
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
    (PythonPackage, RepositoryMutationTestingCheck) -> Just (packageName ++ "_mutation_testing")
    (PythonLatexPackage, RepositoryCoverageCheck) -> Just (packageName ++ "_coverage")
    (PythonLatexPackage, RepositoryProfileCheck) -> Just (packageName ++ "_profile")
    (PythonLatexPackage, RepositoryPropertyTestingCheck) -> Just (packageName ++ "_property_testing")
    (PythonLatexPackage, RepositoryMutationTestingCheck) -> Just (packageName ++ "_mutation_testing")
    (HtmlPackage, RepositoryDefaultCheck) -> Just packageName
    (CPackage, RepositoryDefaultCheck) -> Just packageName
    _ -> Nothing
repositoryCheckBaselineSourceWith :: CanonicalizationSettings -> PackageKind -> RepositoryCheckKind -> Maybe T.Text
repositoryCheckBaselineSourceWith canonicalizationSettings packageKind repositoryCheckKind =
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
    (PythonPackage, RepositoryMutationTestingCheck) -> Just (pythonMutationTestingCheckBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
    (PythonLatexPackage, RepositoryCoverageCheck) -> Just pythonCoverageCheckBaselineNixSource
    (PythonLatexPackage, RepositoryProfileCheck) -> Just pythonProfileCheckBaselineNixSource
    (PythonLatexPackage, RepositoryPropertyTestingCheck) -> Just pythonPropertyTestingCheckBaselineNixSource
    (PythonLatexPackage, RepositoryMutationTestingCheck) -> Just (pythonMutationTestingCheckBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
    (HtmlPackage, RepositoryDefaultCheck) -> Just htmlTemplateCheckBaselineNixSource
    (CPackage, RepositoryDefaultCheck) -> Just cTemplateCheckBaselineNixSource
    _ -> Nothing
renderScaffoldFiles :: PackageKind -> FilePath -> Maybe String -> [ScaffoldFile]
renderScaffoldFiles = renderScaffoldFilesWith defaultCanonicalizationSettings
renderScaffoldFilesWith :: CanonicalizationSettings -> PackageKind -> FilePath -> Maybe String -> [ScaffoldFile]
renderScaffoldFilesWith canonicalizationSettings packageKind packageName packageDescription =
  case packageKind of
    HaskellPackage ->
      [ ScaffoldFile ".gitignore" haskellGitignoreSource,
        ScaffoldFile "default.nix" haskellTemplateBaselineNixSource,
        ScaffoldFile "Main.hs" haskellMainSource,
        ScaffoldFile (packageName <.> "cabal") (renderScaffoldHaskellCabal packageName)
      ]
    RustPackage ->
      [ ScaffoldFile ".gitignore" rustGitignoreSource,
        ScaffoldFile "default.nix" rustTemplateBaselineNixSource,
        ScaffoldFile "Cargo.toml" (renderScaffoldCargoToml packageName),
        ScaffoldFile "src/main.rs" rustMainSource
      ]
    HtmlPackage ->
      [ ScaffoldFile ".gitignore" htmlGitignoreSource,
        ScaffoldFile "default.nix" htmlTemplateBaselineNixSource,
        ScaffoldFile "index.html" htmlIndexSource,
        ScaffoldFile "script.js" htmlScriptSource,
        ScaffoldFile "style.css" htmlStyleSource
      ]
    PythonLatexPackage ->
      [ ScaffoldFile ".gitignore" pythonLatexGitignoreSource,
        ScaffoldFile "default.nix" pythonLatexTemplateBaselineNixSource,
        ScaffoldFile "main.py" pythonLatexMainSource,
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
    PythonPackage ->
      [ ScaffoldFile ".gitignore" pythonGitignoreSource,
        ScaffoldFile "default.nix" (renderPythonTemplateBaselineNixSourceWith (fromMaybe defaultPythonTemplateDescription packageDescription) (canonicalizationPythonPackageAttribute canonicalizationSettings)),
        ScaffoldFile "main.py" pythonMainSource
      ]
    CPackage ->
      [ ScaffoldFile ".gitignore" cGitignoreSource,
        ScaffoldFile "default.nix" cTemplateBaselineNixSource,
        ScaffoldFile "main.c" cMainSource
      ]
    LatexPackage ->
      [ ScaffoldFile ".gitignore" latexGitignoreSource,
        ScaffoldFile "default.nix" latexTemplateBaselineNixSource,
        ScaffoldFile "ms.tex" latexMsTexSource,
        ScaffoldFile "ms.bib" latexMsBibSource
      ]
    _ -> []
defaultPythonTemplateDescription :: String
defaultPythonTemplateDescription = "A Python template package."
defaultPythonPackageAttribute :: String
defaultPythonPackageAttribute = "python312"
escapeNixDoubleQuotedString :: String -> String
escapeNixDoubleQuotedString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar otherChar = [otherChar]
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
renderScaffoldCargoToml :: FilePath -> T.Text
renderScaffoldCargoToml packageName =
  T.unlines
    [ line
    | sourceLine <- T.lines removeEmptyLinesCargoTomlFixture,
      let line =
            case sourceLine of
              "name = \"remove-empty-lines\"" -> T.pack ("name = \"" ++ packageName ++ "\"")
              "description = \"A CLI tool to remove empty lines from text files.\"" -> "description = \"Generated Rust package.\""
              otherLine -> otherLine,
      line /= "repository = \"https://github.com/pbizopoulos/canonicalization\"",
      line /= "readme = \"../../README\"",
      line /= "keywords = [\"cleanup\", \"formatter\"]",
      line /= "categories = [\"development-tools\"]"
    ]
renderScaffoldHaskellCabal :: FilePath -> T.Text
renderScaffoldHaskellCabal packageName =
  T.unlines
    [ case sourceLine of
        "name:          haskell-template" -> T.pack ("name:          " ++ packageName)
        "synopsis:      Canonical Haskell package template" -> "synopsis:      Generated Haskell package"
        "executable haskell-template" -> T.pack ("executable " ++ packageName)
        otherLine -> otherLine
    | sourceLine <- T.lines haskellCabalBaseline
    ]
type GitSubmoduleRepository :: Type
data GitSubmoduleRepository = GitSubmoduleRepository
  { gitSubmoduleRepositoryHost :: String,
    gitSubmoduleRepositoryOwner :: String,
    gitSubmoduleRepositoryName :: String,
    gitSubmoduleRepositoryPathEntry :: FilePath,
    gitSubmoduleRepositoryPath :: FilePath,
    gitSubmoduleRepositoryIsCompatible :: Bool
  }
buildGitSubmoduleRepository :: FilePath -> FilePath -> GitSubmoduleRepository
buildGitSubmoduleRepository homeDirectory gitSubmodulePathEntry =
  let localGitSubmoduleRepositoryPath = homeDirectory </> gitSubmodulePathEntry
      gitSubmodulePathSegments = splitDirectories gitSubmodulePathEntry
   in case gitSubmodulePathSegments of
        [hostSegment, ownerSegment, repositorySegment] ->
          GitSubmoduleRepository
            { gitSubmoduleRepositoryHost = hostSegment,
              gitSubmoduleRepositoryOwner = ownerSegment,
              gitSubmoduleRepositoryName = repositorySegment,
              gitSubmoduleRepositoryPathEntry = gitSubmodulePathEntry,
              gitSubmoduleRepositoryPath = localGitSubmoduleRepositoryPath,
              gitSubmoduleRepositoryIsCompatible = True
            }
        _ ->
          GitSubmoduleRepository
            { gitSubmoduleRepositoryHost = "",
              gitSubmoduleRepositoryOwner = "",
              gitSubmoduleRepositoryName = takeFileName gitSubmodulePathEntry,
              gitSubmoduleRepositoryPathEntry = gitSubmodulePathEntry,
              gitSubmoduleRepositoryPath = localGitSubmoduleRepositoryPath,
              gitSubmoduleRepositoryIsCompatible = False
            }
parseGitSubmodulePathEntries :: String -> [FilePath]
parseGitSubmodulePathEntries gitSubmodulesContents =
  nub
    [ T.unpack (T.strip (T.pack rawPathEntry))
    | gitSubmodulesLine <- lines gitSubmodulesContents,
      let trimmedLine = T.unpack (T.strip (T.pack gitSubmodulesLine)),
      "path" `isPrefixOf` trimmedLine,
      "=" `isInfixOf` trimmedLine,
      let rawPathEntry = drop 1 (dropWhile (/= '=') trimmedLine),
      not (null (T.unpack (T.strip (T.pack rawPathEntry))))
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
  pure (missingPackageDefaultNixIssues ++ missingHostConfigurationIssues ++ missingCabalForMainHaskellIssues ++ misnamedCabalFileIssues ++ ambiguousPackageMarkerIssues ++ disallowedPathIssues)
type PackageKind :: Type
data PackageKind
  = HaskellPackage
  | RustPackage
  | HtmlPackage
  | PythonLatexPackage
  | PythonPackage
  | PythonPypiPackage
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
  let basePackagePathRegexes = ["^" ++ packageRootDirectory ++ "/default\\.nix$", "^" ++ packageRootDirectory ++ "/\\.gitignore$"]
      withBasePackagePathRegexes additionalPathRegexes = basePackagePathRegexes ++ additionalPathRegexes
   in case packageKind of
        HaskellPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/Main\\.hs$", "^" ++ packageRootDirectory ++ "/" ++ packageDirectoryName ++ "\\.cabal$"]
        RustPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/Cargo\\.toml$", "^" ++ packageRootDirectory ++ "/Cargo\\.lock$", "^" ++ packageRootDirectory ++ "/src/main\\.rs$"]
        HtmlPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/index\\.html$", "^" ++ packageRootDirectory ++ "/script\\.js$", "^" ++ packageRootDirectory ++ "/style\\.css$"]
        PythonLatexPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/main\\.py$", "^" ++ packageRootDirectory ++ "/ms\\.tex$", "^" ++ packageRootDirectory ++ "/ms\\.bib$", "^" ++ packageRootDirectory ++ "/refs\\.bib$", "^" ++ packageRootDirectory ++ "/figures(/.*)?$"]
        PythonPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/main\\.py$"]
        PythonPypiPackage -> basePackagePathRegexes
        CPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/main\\.c$"]
        TerraformPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/main\\.tf$", "^" ++ packageRootDirectory ++ "/\\.terraform(/.*)?$", "^" ++ packageRootDirectory ++ "/\\.terraform\\.lock\\.hcl$"]
        LatexPackage -> withBasePackagePathRegexes ["^" ++ packageRootDirectory ++ "/ms\\.tex$", "^" ++ packageRootDirectory ++ "/ms\\.bib$"]
        BinaryReleasePackage -> basePackagePathRegexes
        UnknownPackage -> basePackagePathRegexes
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
  (defaultNixTemplateIssues, _) <-
    if not packageDefaultNixExists
      then pure ([], Nothing)
      else do
        packageDefaultNixSource <- TIO.readFile packageDefaultNixPath
        inferredDefaultNixTemplateName <- inferDefaultNixTemplateName packageName (T.unpack packageDefaultNixSource)
        case inferredDefaultNixTemplateName of
          Nothing ->
            pure
              ( [ "packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"
                ],
                Nothing
              )
          Just matchedDefaultNixTemplateName -> do
            case defaultNixTemplateSpecByName matchedDefaultNixTemplateName of
              Nothing ->
                pure
                  ( [ "packages/" ++ packageName ++ "/default.nix: unsupported template " ++ matchedDefaultNixTemplateName
                    ],
                    Just matchedDefaultNixTemplateName
                  )
              Just defaultNixTemplateSpec ->
                case defaultNixTemplateBaselineSource defaultNixTemplateSpec of
                  Just defaultNixTemplateSource ->
                    do
                      let allowedNixDifferenceKeysForPackage =
                            if packageName == "c_template" && matchedDefaultNixTemplateName == "c_template"
                              then defaultAllowedNixDifferenceKeys
                              else defaultNixTemplateAllowedDifferenceKeys defaultNixTemplateSpec
                          templateDefaultNixSourceOverride =
                            case matchedDefaultNixTemplateName of
                              "python_template" ->
                                Just (renderPythonTemplateBaselineNixSourceWith defaultPythonTemplateDescription (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              "python_pypi_template" ->
                                Just (pythonPypiTemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              "python_pypi_application_template" ->
                                Just (pythonPypiApplicationTemplateBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings))
                              _ -> Just defaultNixTemplateSource
                      defaultNixTemplateComparisonIssues <- comparePackageDefaultNixWithTemplate packageName packageDefaultNixPath ("packages" </> matchedDefaultNixTemplateName </> "default.nix") allowedNixDifferenceKeysForPackage templateDefaultNixSourceOverride
                      pure (defaultNixTemplateComparisonIssues, Just matchedDefaultNixTemplateName)
                  Nothing ->
                    pure
                      ( [ "packages/"
                            ++ packageName
                            ++ "/default.nix: internal error: missing embedded template baseline for "
                            ++ matchedDefaultNixTemplateName
                        ],
                        Just matchedDefaultNixTemplateName
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
          ++ defaultNixTemplateIssues
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
          ++ defaultNixTemplateIssues
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
checkCheckWith :: CanonicalizationSettings -> FilePath -> IO [String]
checkCheckWith canonicalizationSettings checkName = do
  let checkDefaultNixPath = "checks" </> checkName </> "default.nix"
  maybeCheckDefaultNixText <- readTextFileIfExists checkDefaultNixPath
  case maybeCheckDefaultNixText of
    Nothing -> pure []
    Just checkDefaultNixText -> do
      inferredCheckDefaultNixTemplateName <- inferCheckDefaultNixTemplateName checkName (T.unpack checkDefaultNixText)
      case inferredCheckDefaultNixTemplateName of
        Nothing ->
          pure
            [ "checks/" ++ checkName ++ "/default.nix: could not infer corresponding check template"
            ]
        Just matchedCheckDefaultNixTemplateName ->
          case checkDefaultNixTemplateSpecByName matchedCheckDefaultNixTemplateName of
            Nothing ->
              pure
                [ "checks/" ++ checkName ++ "/default.nix: unsupported check template " ++ matchedCheckDefaultNixTemplateName
                ]
            Just checkDefaultNixTemplateSpec ->
              validateCheckDefaultNixWith
                canonicalizationSettings
                checkName
                checkDefaultNixPath
                matchedCheckDefaultNixTemplateName
                checkDefaultNixTemplateSpec
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
  let isPythonPypiPackage =
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
        | isPythonPypiPackage = PythonPypiPackage
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
          hasLegacyMainProgram = "\n  mainProgram = pname;" `isInfixOf` defaultNixSource
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
              [ if packageKind == HaskellPackage && not hasLegacyMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: Haskell packages must set mainProgram = pname;")
                  else Nothing,
                if packageKind == HaskellPackage && hasMetaMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: Haskell packages must not set meta.mainProgram = pname;")
                  else Nothing,
                if expectsMetaMainProgram && not hasMetaMainProgram
                  then Just ("packages/" ++ packageName ++ "/default.nix: package kind requires meta.mainProgram = pname;")
                  else Nothing,
                if expectsMetaMainProgram && hasLegacyMainProgram
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
          pythonPath <- findExecutable "python"
          case python3Path <|> pythonPath of
            Nothing ->
              pure
                [ "packages/"
                    ++ packageName
                    ++ "/main.py: missing Python interpreter (tried python3, python)"
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
                "hUnitDebugTests" `isInfixOf` haskellSource
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
      labelsFromHUnitTilde =
        [ label
        | sourceLine <- haskellSourceLines,
          Just label <- [extractHUnitTildeTestLabel sourceLine]
        ]
      labelsFromAssertEqual = extractAssertEqualTestLabels haskellSourceLines
      fallbackHUnitTestNames =
        [ "Unnamed HUnit test case #" ++ show i
        | i <- [1 .. length [() | sourceLine <- haskellSourceLines, "TestCase" `isInfixOf` sourceLine]]
        ]
      discoveredHaskellUnitTestNames =
        if null labelsFromMakeFormattingTest && null labelsFromHUnitTilde && null labelsFromAssertEqual
          then fallbackHUnitTestNames
          else labelsFromMakeFormattingTest ++ labelsFromHUnitTilde ++ labelsFromAssertEqual
      meaningfulHaskellUnitTestNames = filter isMeaningfulTestLabel discoveredHaskellUnitTestNames
   in sort (Set.toList (Set.fromList meaningfulHaskellUnitTestNames))
isMeaningfulTestLabel :: String -> Bool
isMeaningfulTestLabel label =
  let trimmedLabel = T.unpack (T.strip (T.pack label))
   in case trimmedLabel of
        firstCharacter : _ -> isAlphaNum firstCharacter && any isAlphaNum trimmedLabel
        [] -> False
extractHUnitTildeTestLabel :: String -> Maybe String
extractHUnitTildeTestLabel sourceLine =
  case breakOnSubstring "~:" sourceLine of
    Nothing -> Nothing
    Just (beforeTilde, _) -> lastQuotedToken beforeTilde
breakOnSubstring :: String -> String -> Maybe (String, String)
breakOnSubstring needle = go []
  where
    go _prefixReversed [] = Nothing
    go prefixReversed rest
      | needle `isPrefixOf` rest = Just (prefix, drop (length needle) rest)
      | otherwise =
          case rest of
            ch : tailRest -> go (ch : prefixReversed) tailRest
      where
        prefix = reverse prefixReversed
lastQuotedToken :: String -> Maybe String
lastQuotedToken inputText =
  let go [] _currentQuote _currentToken acc = reverse acc
      go (ch : rest) currentQuote currentToken acc =
        case currentQuote of
          Nothing ->
            if ch == '"' || ch == '\''
              then go rest (Just ch) "" acc
              else go rest Nothing currentToken acc
          Just q ->
            if ch == q
              then go rest Nothing "" (if null currentToken then acc else currentToken : acc)
              else go rest (Just q) (currentToken ++ [ch]) acc
      tokens = go inputText Nothing "" []
   in case tokens of
        [] -> Nothing
        token : _ -> Just token
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
    startsWithQuote (ch : _) = ch == '"' || ch == '\''
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
  let firstTokenAfter quoteCharacter =
        case dropWhile (/= quoteCharacter) inputText of
          _ : rest ->
            let token = takeWhile (/= quoteCharacter) rest
             in if null token then Nothing else Just token
          _ -> Nothing
   in firstTokenAfter '"' <|> firstTokenAfter '\''
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
comparePackageDefaultNixWithTemplate packageName subjectNixPath templateDefaultNixPath allowedNixDifferenceKeys maybeTemplateDefaultNixSourceOverride = do
  packageKind <- detectPackageKindForPackage packageName
  let ignoredTopLevelFunctionParams :: Set.Set T.Text
      ignoredTopLevelFunctionParams =
        case packageKind of
          CPackage -> Set.singleton "inputs"
          _ -> Set.empty
  compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateDefaultNixPath allowedNixDifferenceKeys maybeTemplateDefaultNixSourceOverride
compareCheckDefaultNixWithTemplate :: FilePath -> T.Text -> IO [String]
compareCheckDefaultNixWithTemplate checkDefaultNixPath templateDefaultNixSource = do
  checkDefaultNixSource <- TIO.readFile checkDefaultNixPath
  let normalizedCheckDefaultNix = normalizePythonPackageAttributeReferences (T.strip checkDefaultNixSource)
      normalizedTemplateDefaultNix = normalizePythonPackageAttributeReferences (T.strip templateDefaultNixSource)
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
         in [ checkDefaultNixPath
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
validateCheckDefaultNixWith :: CanonicalizationSettings -> FilePath -> FilePath -> FilePath -> CheckDefaultNixTemplateSpec -> IO [String]
validateCheckDefaultNixWith canonicalizationSettings checkName checkDefaultNixPath checkTemplateName checkDefaultNixTemplateSpec =
  case checkDefaultNixTemplateComparisonMode checkDefaultNixTemplateSpec of
    ExactCheckTemplate ->
      compareCheckDefaultNixWithTemplate
        checkDefaultNixPath
        ( case checkTemplateName of
            "python_mutation_testing_check" ->
              pythonMutationTestingCheckBaselineNixSourceWith (canonicalizationPythonPackageAttribute canonicalizationSettings)
            _ ->
              checkDefaultNixTemplateBaselineSource checkDefaultNixTemplateSpec
        )
    StructuralCPackageVmCheck ->
      validateCPackageVmCheck checkName checkDefaultNixPath
validateCPackageVmCheck :: FilePath -> FilePath -> IO [String]
validateCPackageVmCheck checkName checkDefaultNixPath = do
  maybeCheckDefaultNixText <- readTextFileIfExists checkDefaultNixPath
  case maybeCheckDefaultNixText of
    Nothing -> pure []
    Just checkDefaultNixText -> do
      packageKind <- detectPackageKindForPackage checkName
      pure (validateCPackageVmCheckSource packageKind checkName checkDefaultNixPath (T.unpack checkDefaultNixText))
validateCPackageVmCheckSource :: PackageKind -> FilePath -> FilePath -> String -> [String]
validateCPackageVmCheckSource packageKind checkName checkDefaultNixPath checkDefaultNixSource =
  let hasCanonicalNameBinding =
        "name = builtins.baseNameOf ./.;" `isInfixOf` checkDefaultNixSource
          || "name = baseNameOf ./.;" `isInfixOf` checkDefaultNixSource
      hasRunNixOSTest = "pkgs.testers.runNixOSTest" `isInfixOf` checkDefaultNixSource
      hasMachineNode = "nodes.machine" `isInfixOf` checkDefaultNixSource
      hasTestScript = "testScript = ''" `isInfixOf` checkDefaultNixSource
      hasSameNamePackageReference =
        "inputs.self.packages.${pkgs.stdenv.system}.${name}" `isInfixOf` checkDefaultNixSource
          || ("inputs.self.packages.${pkgs.stdenv.system}." ++ checkName) `isInfixOf` checkDefaultNixSource
   in catMaybes
        [ if packageKind == CPackage
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks require a same-name C package under packages/"),
          if hasRunNixOSTest
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks must use pkgs.testers.runNixOSTest"),
          if hasCanonicalNameBinding
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks must bind name from ./."),
          if hasMachineNode
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks must define nodes.machine"),
          if hasTestScript
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks must define testScript"),
          if hasSameNamePackageReference
            then Nothing
            else Just (checkDefaultNixPath ++ ": generic C VM checks must install or override the same-name package from inputs.self.packages")
        ]
compareNixFileWithTemplate :: Set.Set T.Text -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
compareNixFileWithTemplate ignoredTopLevelFunctionParams subjectNixPath templateDefaultNixPath allowedNixDifferenceKeys maybeTemplateDefaultNixSourceOverride = do
  subjectNixParseResult <- parseNixExprFromFile subjectNixPath
  templateDefaultNixParseResult <-
    case maybeTemplateDefaultNixSourceOverride of
      Just templateDefaultNixSource -> parseNixExprFromText templateDefaultNixSource
      Nothing -> parseNixExprFromFile templateDefaultNixPath
  case (subjectNixParseResult, templateDefaultNixParseResult) of
    (Left parseError, _) ->
      pure [subjectNixPath ++ ": parse error: " ++ show parseError]
    (_, Left parseError) ->
      pure [templateDefaultNixPath ++ ": parse error: " ++ show parseError]
    (Right subjectNixExpr, Right templateDefaultNixExpr) ->
      let normalizedSubjectNixExpr = normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys subjectNixExpr
          normalizedTemplateDefaultNixExpr = normalizeNixExpr ignoredTopLevelFunctionParams allowedNixDifferenceKeys templateDefaultNixExpr
       in pure $
            formatNixTemplateDifferences
              subjectNixPath
              templateDefaultNixPath
              normalizedSubjectNixExpr
              normalizedTemplateDefaultNixExpr
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
inferDefaultNixTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferDefaultNixTemplateName packageName nixSource = do
  maybeMatchedDefaultNixTemplateNames <- forM defaultNixTemplateSpecs $ \defaultNixTemplateSpec -> do
    matched <- defaultNixTemplateMatches defaultNixTemplateSpec packageName nixSource
    pure (if matched then Just (defaultNixTemplateName defaultNixTemplateSpec) else Nothing)
  pure (listToMaybe (catMaybes maybeMatchedDefaultNixTemplateNames))
inferCheckDefaultNixTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferCheckDefaultNixTemplateName checkName nixSource = do
  maybeMatchedCheckDefaultNixTemplateNames <- forM checkDefaultNixTemplateSpecs $ \checkDefaultNixTemplateSpec -> do
    matched <- checkDefaultNixTemplateMatches checkDefaultNixTemplateSpec checkName nixSource
    pure (if matched then Just (checkDefaultNixTemplateName checkDefaultNixTemplateSpec) else Nothing)
  pure (listToMaybe (catMaybes maybeMatchedCheckDefaultNixTemplateNames))
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
formatDefaultNixTemplateDifferences :: FilePath -> FilePath -> NExprLoc -> NExprLoc -> [String]
formatDefaultNixTemplateDifferences packageName =
  formatNixTemplateDifferences ("packages/" ++ packageName ++ "/default.nix")
formatNixTemplateDifferences :: FilePath -> FilePath -> NExprLoc -> NExprLoc -> [String]
formatNixTemplateDifferences subjectNixPath templateDefaultNixPath subjectNixExpr templateDefaultNixExpr =
  let renderedPackageDefaultNix = renderNixExpr subjectNixExpr
      renderedTemplateDefaultNix = renderNixExpr templateDefaultNixExpr
   in if renderedPackageDefaultNix == renderedTemplateDefaultNix
        then []
        else
          let packageLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings subjectNixExpr)
              templateLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings templateDefaultNixExpr)
              packagePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings subjectNixExpr)
              templatePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings templateDefaultNixExpr)
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
                  ++ templateDefaultNixPath
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
runPackageTests = do
  hUnitCounts <- runTestTT hUnitDebugTests
  propertySuccess <- quickCheckDebugProperties
  if errors hUnitCounts == 0 && failures hUnitCounts == 0 && propertySuccess
    then putStrLn "test ... ok"
    else exitFailure
quickCheckDebugProperties :: IO Bool
quickCheckDebugProperties = do
  trimResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_trimStringIdempotent)
  gitSubmoduleParseResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_parseGitSubmodulePathEntriesPreservesFirstOccurrences)
  gitSubmoduleRepositoryAcceptanceResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_buildGitSubmoduleRepositoryAcceptsGoStylePathEntries)
  gitSubmoduleRepositoryRejectionResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_buildGitSubmoduleRepositoryRejectsMalformedPathEntries)
  pure (all isQuickCheckSuccess [trimResult, gitSubmoduleParseResult, gitSubmoduleRepositoryAcceptanceResult, gitSubmoduleRepositoryRejectionResult])
isQuickCheckSuccess :: QC.Result -> Bool
isQuickCheckSuccess QC.Success {} = True
isQuickCheckSuccess _ = False
prop_trimStringIdempotent :: String -> Bool
prop_trimStringIdempotent inputText =
  let trim = T.unpack . T.strip . T.pack
   in trim (trim inputText) == trim inputText
prop_parseGitSubmodulePathEntriesPreservesFirstOccurrences :: QC.Property
prop_parseGitSubmodulePathEntriesPreservesFirstOccurrences =
  QC.forAll gitSubmodulePathEntriesGen $ \gitSubmodulePathEntries ->
    let renderedGitSubmodulesContents =
          concatMap
            ( \gitSubmodulePathEntry ->
                "[submodule \"example\"]\n"
                  ++ "  path =  "
                  ++ gitSubmodulePathEntry
                  ++ "  \n"
                  ++ "  url = https://example.test/repo.git\n"
            )
            gitSubmodulePathEntries
            ++ "ignore = this-line\n"
     in parseGitSubmodulePathEntries renderedGitSubmodulesContents == nub gitSubmodulePathEntries
prop_buildGitSubmoduleRepositoryAcceptsGoStylePathEntries :: QC.Property
prop_buildGitSubmoduleRepositoryAcceptsGoStylePathEntries =
  QC.forAll goStylePathEntryGen $ \gitSubmodulePathEntry ->
    let gitSubmoduleRepository = buildGitSubmoduleRepository "/home/test" gitSubmodulePathEntry
        gitSubmodulePathSegments = splitDirectories gitSubmodulePathEntry
     in case gitSubmodulePathSegments of
          [hostSegment, ownerSegment, repositorySegment] ->
            gitSubmoduleRepositoryIsCompatible gitSubmoduleRepository
              && gitSubmoduleRepositoryPathEntry gitSubmoduleRepository == gitSubmodulePathEntry
              && gitSubmoduleRepositoryPath gitSubmoduleRepository == "/home/test" </> gitSubmodulePathEntry
              && gitSubmoduleRepositoryHost gitSubmoduleRepository == hostSegment
              && gitSubmoduleRepositoryOwner gitSubmoduleRepository == ownerSegment
              && gitSubmoduleRepositoryName gitSubmoduleRepository == repositorySegment
          _ -> False
prop_buildGitSubmoduleRepositoryRejectsMalformedPathEntries :: QC.Property
prop_buildGitSubmoduleRepositoryRejectsMalformedPathEntries =
  QC.forAll malformedPathEntryGen $ \gitSubmodulePathEntry ->
    let gitSubmoduleRepository = buildGitSubmoduleRepository "/home/test" gitSubmodulePathEntry
     in not (gitSubmoduleRepositoryIsCompatible gitSubmoduleRepository) && gitSubmoduleRepositoryPathEntry gitSubmoduleRepository == gitSubmodulePathEntry
gitSubmodulePathEntriesGen :: QC.Gen [FilePath]
gitSubmodulePathEntriesGen = QC.listOf goStylePathEntryGen
goStylePathEntryGen :: QC.Gen FilePath
goStylePathEntryGen = do
  hostSegment <- hostSegmentGen
  ownerSegment <- pathSegmentGen "-"
  repositorySegment <- pathSegmentGen "-_"
  pure (intercalate "/" [hostSegment, ownerSegment, repositorySegment])
hostSegmentGen :: QC.Gen String
hostSegmentGen = do
  firstCharacter <- QC.elements (['a' .. 'z'] ++ ['0' .. '9'])
  restCharacters <- QC.listOf (QC.elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "."))
  pure (firstCharacter : restCharacters)
malformedPathEntryGen :: QC.Gen FilePath
malformedPathEntryGen = do
  segmentCount <- QC.elements [0, 1, 2, 4, 5]
  segments <- QC.vectorOf segmentCount (pathSegmentGen "-_.")
  pure (intercalate "/" segments)
pathSegmentGen :: [Char] -> QC.Gen String
pathSegmentGen extraCharacters =
  QC.listOf1 (QC.elements (['a' .. 'z'] ++ ['0' .. '9'] ++ extraCharacters))
hUnitDebugTests :: Test
hUnitDebugTests =
  TestList
    [ templateInferenceDebugTests,
      checkTemplateDebugTests,
      metadataAndDiscoveryDebugTests,
      repositoryPolicyDebugTests,
      createPackageDebugTests
    ]
templateInferenceDebugTests :: Test
templateInferenceDebugTests =
  TestList
    [ TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" uncommentNixFixture
        assertEqual
          "Infers the uncomment template."
          (Just "uncomment_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" rustNixFixture
        assertEqual
          "Infers the Rust package template."
          (Just "rust_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" haskellNixFixture
        assertEqual
          "Infers the Haskell package template."
          (Just "haskell_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" pythonNixFixture
        assertEqual
          "Infers the Python template."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" pythonPypiNixFixture
        assertEqual
          "Infers the PyPI Python template."
          (Just "python_pypi_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" binaryReleaseNixFixture
        assertEqual
          "Infers the binary release template."
          (Just "binary_release_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" pythonLatexNixFixture
        assertEqual
          "Infers the Python template for the python-latex fixture."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "deploy_host_template" deployHostNixFixture
        assertEqual
          "Infers the deploy host template."
          (Just "deploy_host_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" cNixFixture
        assertEqual
          "Infers the C template."
          (Just "c_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" latexNixFixture
        assertEqual
          "Infers the LaTeX template."
          (Just "latex_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" htmlNixFixture
        assertEqual
          "Infers the HTML template."
          (Just "html_template")
          inferred,
      TestCase $ do
        inferred <- inferDefaultNixTemplateName "test" unknownNixFixture
        assertEqual
          "Returns no template for unknown input."
          Nothing
          inferred,
      TestCase $ do
        inferred <- inferCheckDefaultNixTemplateName "python_template_coverage" (T.unpack pythonCoverageCheckBaselineNixSource)
        assertEqual
          "Infers the Python coverage check template."
          (Just "python_coverage_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckDefaultNixTemplateName "canonicalization-profile" (T.unpack haskellProfileCheckBaselineNixSource)
        assertEqual
          "Infers the Haskell profile check template."
          (Just "haskell_profile_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckDefaultNixTemplateName "remove-empty-lines-mutation-testing" (T.unpack rustMutationTestingCheckBaselineNixSource)
        assertEqual
          "Infers the Rust mutation-testing check template."
          (Just "rust_mutation_testing_check")
          inferred,
      TestCase $ do
        inferred <- inferCheckDefaultNixTemplateName "laptopVmWithDisko" (T.unpack defaultVmWithDiskoCheckBaselineNixSource)
        assertEqual
          "Infers the generic VM-with-disko check template."
          (Just "default_vm_with_disko_check")
          inferred
    ]
checkTemplateDebugTests :: Test
checkTemplateDebugTests =
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
        legacyPythonTemplateParseResult <- parseNixExprFromText (T.pack legacyPythonTemplateNixFixture)
        templatePythonParseResult <- parseNixExprFromText pythonTemplateBaselineNixSource
        case (legacyPythonTemplateParseResult, templatePythonParseResult) of
          (Right legacyPythonTemplateExpr, Right templatePythonExpr) ->
            assertBool
              "Reports legacy Python template let-binding differences explicitly."
              ( any
                  ("unexpected let key: pyPkgs" `isInfixOf`)
                  (formatDefaultNixTemplateDifferences "test" "packages/python_template/default.nix" legacyPythonTemplateExpr templatePythonExpr)
              )
          (Left parseError, _) -> assertFailure ("Failed to parse legacy Python template fixture: " ++ parseError)
          (_, Left parseError) -> assertFailure ("Failed to parse Python template baseline fixture: " ++ parseError),
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
        assertBool
          "Renders the Python template with an alternate interpreter binding."
          ("pkgs.python311" `isInfixOf` T.unpack (renderPythonTemplateBaselineNixSourceWith "Custom Python package" "python311")),
      TestCase $ do
        assertBool
          "Renders the Python PyPI template with an alternate interpreter binding."
          ("pkgs.python311" `isInfixOf` T.unpack (pythonPypiTemplateBaselineNixSourceWith "python311")),
      TestCase $ do
        assertBool
          "Renders the Python mutation-testing check with an alternate interpreter binding."
          ("pkgs.python311.withPackages" `isInfixOf` T.unpack (pythonMutationTestingCheckBaselineNixSourceWith "python311")),
      TestCase $ do
        (tempPath, tempHandle) <- openTempFile "/tmp" "python-mutation-check-custom-version.nix"
        TIO.hPutStr tempHandle (pythonMutationTestingCheckBaselineNixSourceWith "python311")
        hClose tempHandle
        issues <-
          compareCheckDefaultNixWithTemplate
            tempPath
            pythonMutationTestingCheckBaselineNixSource
        removeFileIfExists tempPath
        assertEqual
          "Allows Python mutation-testing checks to use a different interpreter attribute."
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
          Left parseError -> assertFailure ("Failed to parse runNixOSTest fixture: " ++ parseError)
    ]
metadataAndDiscoveryDebugTests :: Test
metadataAndDiscoveryDebugTests =
  TestList
    [ TestCase $ do
        assertEqual
          "Extracts the package section with extractTomlSection."
          "name = \"example-package\"\nversion = \"0.1.0\"\nedition = \"2021\"\ndescription = \"Example package fixture for TOML parsing.\"\nlicense = \"MIT\"\nrepository = \"https://github.com/pbizopoulos/canonicalization\"\nreadme = \"../../README\"\nkeywords = [\"check\", \"lint\", \"fixture\"]\ncategories = [\"development-tools\"]\n\n"
          (extractTomlSection "package" exampleCargoTomlFixture),
      TestCase $ do
        assertEqual
          "Parses the package name with lookupTomlString."
          (Just "remove-empty-lines")
          (lookupTomlString "name" (extractTomlSection "package" removeEmptyLinesCargoTomlFixture)),
      TestCase $ do
        assertEqual
          "Parses lints.rust.unsafe_code with lookupTomlString."
          (Just "forbid")
          (lookupTomlString "unsafe_code" (extractTomlSection "lints.rust" removeEmptyLinesCargoTomlFixture)),
      TestCase $ do
        assertEqual
          "Returns Nothing for a TOML key missing from the section."
          Nothing
          (lookupTomlString "missing_key" (extractTomlSection "package" removeEmptyLinesCargoTomlFixture)),
      TestCase $ do
        assertEqual
          "Discovers Python unit-test names from source."
          ["test_alpha_case", "test_beta_case"]
          ( discoverPythonUnitTestNamesFromSource
              ( unlines
                  [ "def helper_function():",
                    "    return None",
                    "def test_beta_case():",
                    "    return None",
                    "def test_alpha_case():",
                    "    return None"
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Discovers Haskell unit-test names from source."
          ["alpha test", "format one"]
          ( discoverHaskellUnitTestNamesFromSource
              ( unlines
                  [ "suite = TestList",
                    "  [ \"alpha test\" ~: assertEqual \"x\" 1 1",
                    "  , makeFormattingTest",
                    "      \"format one\"",
                    "      input",
                    "      output"
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Ignores quoted fixture lines that only mimic HUnit labels."
          ["alpha test", "format one"]
          ( discoverHaskellUnitTestNamesFromSource
              ( unlines
                  [ "suite = TestList",
                    "  [ \"alpha test\" ~: assertEqual \"x\" 1 1",
                    "  , makeFormattingTest",
                    "      \"format one\"",
                    "      input",
                    "      output",
                    "",
                    "fixture = unlines",
                    "  [ \"  , makeFormattingTest\",",
                    "    \"      \\\"fake label\\\"\",",
                    "    \"      input\",",
                    "    \"      output\"",
                    "  ]"
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Falls back to unnamed HUnit test cases when labels are absent."
          ["Unnamed HUnit test case #1", "Unnamed HUnit test case #2"]
          ( discoverHaskellUnitTestNamesFromSource
              ( unlines
                  [ "suite = TestList",
                    "  [ TestCase $ pure ()",
                    "  , TestCase $ pure ()"
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Discovers Rust unit-test names from source."
          ["alpha_case", "beta_case"]
          ( discoverRustUnitTestNamesFromSource
              ( unlines
                  [ "#[test]",
                    "fn beta_case() {",
                    "}",
                    "#[test]",
                    "fn alpha_case() {",
                    "}"
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Looks up a known default Nix template by name."
          True
          (maybe False ((== "python_template") . defaultNixTemplateName) (defaultNixTemplateSpecByName "python_template")),
      TestCase $ do
        assertBool
          "Returns Nothing for an unknown default Nix template name."
          (isNothing (defaultNixTemplateSpecByName "not-a-template")),
      TestCase $ do
        assertEqual
          "Extracts Python unit-test function names."
          (Just "test_example_case")
          (extractPythonUnitTestName "  def test_example_case():"),
      TestCase $ do
        assertEqual
          "Ignores non-test Python functions."
          Nothing
          (extractPythonUnitTestName "def helper_function():"),
      TestCase $ do
        assertEqual
          "Normalizes Cargo TOML names and drops dependency blocks."
          ( T.unlines
              [ "[[bin]]",
                "name = \"demo\"",
                "path = \"src/main.rs\"",
                "[package]",
                "name = \"demo\""
              ]
          )
          ( normalizeCargoTomlForBaselineComparison
              "demo"
              ( T.unlines
                  [ "[[bin]]",
                    "name = \"old-bin\"",
                    "path = \"src/main.rs\"",
                    "[dependencies]",
                    "a = \"1\"",
                    "",
                    "[package]",
                    "name = \"old-pkg\""
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Normalizes cabal executable and name lines."
          "name:          demo\nexecutable demo\n"
          ( T.unpack
              ( normalizeCabalForBaselineComparison
                  "demo"
                  ( T.unlines
                      [ "name: old-name",
                        "executable old-name",
                        "build-depends:",
                        "  base"
                      ]
                  )
              )
          ),
      TestCase $ do
        assertEqual
          "Normalizes Cabal while ignoring synopsis and description metadata."
          "name:          demo\nexecutable demo\n"
          ( T.unpack
              ( normalizeCabalForBaselineComparison
                  "demo"
                  ( T.unlines
                      [ "name: old-name",
                        "synopsis: custom synopsis",
                        "description: custom description",
                        "executable old-name"
                      ]
                  )
              )
          ),
      TestCase $ do
        assertEqual
          "Normalizes Cabal while ignoring multiline description metadata."
          "name:          demo\nexecutable demo\n"
          ( T.unpack
              ( normalizeCabalForBaselineComparison
                  "demo"
                  ( T.unlines
                      [ "name: old-name",
                        "description:",
                        "  line one",
                        "  line two",
                        "executable old-name"
                      ]
                  )
              )
          ),
      TestCase $ do
        assertEqual
          "Looks up Cabal fields."
          (Just "canonicalization")
          (lookupCabalField "name" (T.pack "name: canonicalization\nversion: 0.0.0\n")),
      TestCase $ do
        assertEqual
          "Returns Nothing for missing Cabal fields."
          Nothing
          (lookupCabalField "license" (T.pack "name: canonicalization\n")),
      TestCase $ do
        assertEqual
          "Extracts Haskell package description using cabal fields."
          (Just "Canonical Haskell package template")
          (extractHaskellPackageDescription haskellCabalBaseline),
      TestCase $ do
        assertEqual
          "Extracts multiline Cabal descriptions."
          (Just "Line one.\nLine two.")
          ( lookupCabalField
              "description"
              ( T.unlines
                  [ "name: demo",
                    "description:",
                    "  Line one.",
                    "  Line two.",
                    ""
                  ]
              )
          ),
      TestCase $ do
        assertEqual
          "Extracts Rust package description using Cargo package fields."
          (Just "A CLI tool to remove empty lines from text files.")
          (extractRustPackageDescription removeEmptyLinesCargoTomlFixture),
      TestCase $ do
        assertEqual
          "Extracts Python package description using pyproject.toml."
          (Just "Example Python package.")
          (extractPythonPackageDescriptionFromPyprojectToml pyprojectTomlDescriptionFixture),
      TestCase $ do
        assertEqual
          "Extracts Nix package descriptions from meta.description."
          (Just "A Python template package.")
          (extractDefaultNixPackageDescription pythonTemplateDefaultNixFixture),
      TestCase $ do
        assertEqual
          "Extracts C template descriptions from nested meta blocks."
          (Just "A C template package.")
          (extractDefaultNixPackageDescription cTemplateDefaultNixFixture),
      TestCase $ do
        assertEqual
          "Extracts Python and LaTeX template descriptions from nested meta blocks."
          (Just "A Python and LaTeX template package.")
          (extractDefaultNixPackageDescription pythonLatexTemplateDefaultNixFixture),
      TestCase $ do
        assertEqual
          "Summarizes repository checks for Python packages."
          ( RepositoryPackageChecksSummary
              { repositoryPackageHasCheck = False,
                repositoryPackageHasCoverageCheck = True,
                repositoryPackageHasProfileCheck = False,
                repositoryPackageHasPropertyTestingCheck = False,
                repositoryPackageHasMutationTestingCheck = True
              }
          )
          ( summarizeRepositoryPackageChecks
              (Set.fromList ["demo_coverage", "demo_mutation_testing"])
              PythonPackage
              "demo"
          )
    ]
repositoryPolicyDebugTests :: Test
repositoryPolicyDebugTests =
  TestList
    [ TestCase $ do
        assertEqual
          "checkOutcomeFromIssues marks empty issue list as passed."
          CheckPassed
          (checkOutcomeFromIssues ([] :: [String])),
      TestCase $ do
        assertEqual
          "checkOutcomeFromIssues marks non-empty issue list as failed."
          CheckFailed
          (checkOutcomeFromIssues ["issue" :: String] :: CheckOutcome),
      TestCase $ do
        assertEqual
          "Parses and deduplicates .gitmodules path entries."
          ["github.com/example/repo", "gitlab.com/org/project"]
          ( parseGitSubmodulePathEntries
              ( unlines
                  [ "[submodule \"one\"]",
                    "  path = github.com/example/repo",
                    "  path = github.com/example/repo",
                    "  url = https://example.test/repo.git",
                    "  path = gitlab.com/org/project",
                    "  path =    ",
                    "  path-without-equals github.com/ignored/repo"
                  ]
              )
          ),
      TestCase $ do
        let malformedRepository = buildGitSubmoduleRepository "/home/user" "github.com/example/repo/subdir"
        assertEqual
          "Malformed git-submodule paths are marked incompatible and keep leaf name."
          (False, "subdir")
          (gitSubmoduleRepositoryIsCompatible malformedRepository, gitSubmoduleRepositoryName malformedRepository),
      TestCase $ do
        assertEqual
          "toRelativePath drops leading ./ segments."
          ("packages" </> "canonicalization" </> "Main.hs")
          (toRelativePath ("." </> "packages" </> "canonicalization" </> "Main.hs")),
      TestCase $ do
        assertEqual
          "shouldTraverseDirectory rejects ignored directories."
          False
          (shouldTraverseDirectory ("packages" </> "remove-empty-lines" </> "target")),
      TestCase $ do
        assertEqual
          "shouldTraverseDirectory allows normal source directories."
          True
          (shouldTraverseDirectory ("packages" </> "canonicalization")),
      TestCase $ do
        assertEqual
          "isLeafPath detects non-leaf paths."
          False
          (isLeafPath ["packages", "packages/canonicalization", "packages/canonicalization/Main.hs"] "packages/canonicalization"),
      TestCase $ do
        assertEqual
          "isLeafPath detects leaf paths."
          True
          (isLeafPath ["packages", "packages/canonicalization", "packages/canonicalization/Main.hs"] "packages/canonicalization/Main.hs"),
      TestCase $ do
        assertEqual
          "packageRootPathFromRepositoryPath extracts package root."
          (Just "packages/canonicalization")
          (packageRootPathFromRepositoryPath "packages/canonicalization/Main.hs"),
      TestCase $ do
        assertEqual
          "hostRootPathFromRepositoryPath extracts host root."
          (Just "hosts/default")
          (hostRootPathFromRepositoryPath "hosts/default/configuration.nix"),
      TestCase $ do
        assertEqual
          "detectPackageKindFromMarkers prefers a unique non-binary marker."
          HaskellPackage
          (detectPackageKindFromMarkers [("Main.hs", HaskellPackage), ("binary-layout", BinaryReleasePackage)]),
      TestCase $ do
        assertEqual
          "detectPackageKindFromMarkers falls back to binary release marker."
          BinaryReleasePackage
          (detectPackageKindFromMarkers [("binary-layout", BinaryReleasePackage)]),
      TestCase $ do
        assertEqual
          "detectPackageKindFromMarkers marks conflicting markers as unknown."
          UnknownPackage
          (detectPackageKindFromMarkers [("Main.hs", HaskellPackage), ("main.py", PythonPackage)]),
      TestCase $ do
        assertEqual
          "Detects package markers for python-latex packages."
          [("main.py+ms.tex", PythonLatexPackage)]
          (detectPackageMarkers ["main.py", "ms.tex"]),
      TestCase $ do
        assertEqual
          "Reports ambiguity when multiple markers match."
          ["packages/example: has ambiguous project markers: Main.hs, main.py"]
          ( ambiguousPackageMarkerIssuesForPackage
              PackageInfo
                { packageRootPath = "packages/example",
                  packageRootDirectoryName = "example",
                  packageLeafPaths = ["Main.hs", "main.py"],
                  detectedPackageKind = UnknownPackage,
                  matchedPackageMarkers = ["Main.hs", "main.py"]
                }
          ),
      TestCase $ do
        assertEqual
          "Normalizes Cargo TOML while ignoring package description and keywords differences."
          ( unlines
              [ "[[bin]]",
                "name = \"demo\"",
                "path = \"src/main.rs\"",
                "[package]",
                "name = \"demo\"",
                "version = \"0.1.0\""
              ]
          )
          ( T.unpack
              ( normalizeCargoTomlForBaselineComparison
                  "demo"
                  ( T.unlines
                      [ "[[bin]]",
                        "name = \"old-bin\"",
                        "path = \"src/main.rs\"",
                        "[package]",
                        "name = \"old-pkg\"",
                        "version = \"0.1.0\"",
                        "description = \"Custom package description\"",
                        "keywords = [\"custom\", \"keywords\"]"
                      ]
                  )
              )
          ),
      TestCase $ do
        assertEqual
          "allowedPathRegexesForPackageKind includes expected Haskell paths."
          True
          ( let regexes = allowedPathRegexesForPackageKind "packages/demo" "demo" HaskellPackage
                mainHsPath :: String
                mainHsPath = "packages/demo/Main.hs"
                cabalPath :: String
                cabalPath = "packages/demo/demo.cabal"
             in any (mainHsPath =~) regexes
                  && any (cabalPath =~) regexes
          ),
      TestCase $ do
        assertEqual
          "allowedPathRegexesForPackageKind includes Terraform lockfile pattern."
          True
          ( let regexes = allowedPathRegexesForPackageKind "packages/demo" "demo" TerraformPackage
                terraformLockPath :: String
                terraformLockPath = "packages/demo/.terraform.lock.hcl"
             in any (terraformLockPath =~) regexes
          ),
      TestCase $ do
        assertEqual
          "Detects binary-layout marker when language markers are absent."
          [("binary-layout", BinaryReleasePackage)]
          (detectPackageMarkers ["README", "notes.txt"]),
      TestCase $ do
        assertEqual
          "Does not emit binary marker when Cargo.toml marker is present."
          [("Cargo.toml", RustPackage)]
          (detectPackageMarkers ["Cargo.toml", "README"]),
      TestCase $ do
        assertEqual
          "buildPackageInfo extracts leaf paths relative to package root."
          ["Main.hs", "demo.cabal"]
          ( sort
              ( packageLeafPaths
                  ( buildPackageInfo
                      (Set.fromList ["packages/demo/Main.hs", "packages/demo/demo.cabal", "packages/other/main.py"])
                      "packages/demo"
                  )
              )
          ),
      TestCase $ do
        assertEqual
          "Marks canonical go-style .gitmodules path as compatible."
          True
          (gitSubmoduleRepositoryIsCompatible (buildGitSubmoduleRepository "/home/user" "github.com/pbizopoulos/canonicalization")),
      TestCase $ do
        assertEqual
          "Rejects non go-style .gitmodules path with extra segments."
          False
          (gitSubmoduleRepositoryIsCompatible (buildGitSubmoduleRepository "/home/user" "github.com/pbizopoulos/canonicalization/subdir")),
      TestCase $
        withTemporaryPackageRepository "compliant-repository-validation" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            _ <- createPackageInCurrentRepository HaskellPackage "demo" Nothing
            repositoryComplianceResult <- collectRepositoryCompliance
            assertEqual
              "Collects package names for compliant repositories."
              ( Right
                  RepositoryComplianceSuccess
                    { repositoryCompliancePackageNames = ["demo"],
                      repositoryComplianceCheckNames = []
                    }
              )
              repositoryComplianceResult,
      TestCase $
        withTemporaryPackageRepository "structure-repository-validation" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            createDirectoryIfMissing True "unexpected"
            TIO.writeFile ("unexpected" </> "file.txt") "bad"
            repositoryComplianceResult <- collectRepositoryCompliance
            case repositoryComplianceResult of
              Left ("directory-structure", repositoryStructureIssues) ->
                assertBool
                  "Reports structure violations for non-canonical paths."
                  ("unexpected/file.txt: is not allowed" `elem` repositoryStructureIssues)
              otherResult ->
                assertFailure ("Expected directory-structure failure, got: " ++ show otherResult),
      TestCase $
        withTemporaryPackageRepository "file-compliance-repository-validation" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            _ <- createPackageInCurrentRepository PythonPackage "demo" Nothing
            TIO.writeFile ("packages" </> "demo" </> "default.nix") "not valid nix template"
            repositoryComplianceResult <- collectRepositoryCompliance
            case repositoryComplianceResult of
              Left ("file-compliance", fileComplianceIssues) ->
                assertBool
                  "Reports file-compliance violations for malformed package files."
                  (any ("packages/demo/default.nix:" `isPrefixOf`) fileComplianceIssues)
              otherResult ->
                assertFailure ("Expected file-compliance failure, got: " ++ show otherResult)
    ]
createPackageDebugTests :: Test
createPackageDebugTests =
  TestList
    [ TestCase $ do
        assertEqual
          "Parses create-package arguments with the current repository default."
          (Just (".", "python", "demo", Nothing))
          (parseCreatePackageArgs ["create-package", "python", "demo"]),
      TestCase $ do
        assertEqual
          "Parses create-package arguments with an explicit description."
          (Just (".", "python", "demo", Just "Custom Python package"))
          (parseCreatePackageArgs ["create-package", "python", "demo", "Custom", "Python", "package"]),
      TestCase $ do
        assertEqual
          "Parses create-repository arguments with the current repository default."
          (Just (".", "python", "demo", Set.fromList [RepositoryCoverageCheck, RepositoryMutationTestingCheck]))
          (parseCreateRepositoryArgs ["create-repository", "python", "demo", "--coverage", "--mutation-testing"]),
      TestCase $ do
        assertEqual
          "Parses create-repository arguments with an explicit repository."
          (Just ("repo", "rust", "demo", Set.fromList [RepositoryProfileCheck]))
          (parseCreateRepositoryArgs ["create-repository", "repo", "rust", "demo", "--profile"]),
      TestCase $ do
        assertEqual
          "Parses create-package arguments with an explicit repository."
          (Just ("repo", "rust", "demo", Nothing))
          (parseCreatePackageArgs ["create-package", "repo", "rust", "demo"]),
      TestCase $ do
        assertEqual
          "Rejects unsupported create-package argument arity."
          Nothing
          (parseCreatePackageArgs ["create-package", "python"]),
      TestCase $ do
        assertEqual
          "Rejects unknown create-repository flags."
          Nothing
          (parseCreateRepositoryArgs ["create-repository", "python", "demo", "--unknown"]),
      TestCase $ do
        assertEqual
          "Parses supported create-package kinds."
          (Just PythonLatexPackage)
          (parseSupportedCreatePackageKind "python-latex"),
      TestCase $ do
        assertEqual
          "Rejects unsupported create-package kinds."
          Nothing
          (parseSupportedCreatePackageKind "terraform"),
      TestCase $ do
        assertEqual
          "Rejects create-package names with path separators."
          (Just "package name must not contain path separators")
          (validateCreatePackageName "bad/name"),
      TestCase $ do
        assertEqual
          "Generates the expected Python scaffold file set."
          ["packages/demo/.gitignore", "packages/demo/default.nix", "packages/demo/main.py"]
          [ "packages/demo" </> scaffoldFileRelativePath scaffoldFile
          | scaffoldFile <- renderScaffoldFiles PythonPackage "demo" Nothing
          ],
      TestCase $ do
        assertEqual
          "Generates the expected Rust scaffold file set."
          ["packages/demo/.gitignore", "packages/demo/default.nix", "packages/demo/Cargo.toml", "packages/demo/src/main.rs"]
          [ "packages/demo" </> scaffoldFileRelativePath scaffoldFile
          | scaffoldFile <- renderScaffoldFiles RustPackage "demo" Nothing
          ],
      TestCase $ do
        assertEqual
          "Rewrites scaffold Cargo.toml metadata to the requested package name."
          ( T.unlines
              [ "[[bin]]",
                "name = \"demo\"",
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
                "name = \"demo\"",
                "version = \"0.1.0\"",
                "edition = \"2021\"",
                "description = \"Generated Rust package.\"",
                "license = \"MIT\"",
                ""
              ]
          )
          (renderScaffoldCargoToml "demo"),
      TestCase $ do
        assertEqual
          "Rewrites scaffold cabal metadata to the requested package name."
          ( T.unlines
              [ "name:          demo",
                "version:       0.0.0",
                "synopsis:      Generated Haskell package",
                "cabal-version: >=1.10",
                "build-type:    Simple",
                "executable demo",
                "  main-is:       Main.hs",
                "  build-depends:",
                "      aeson",
                "    , base",
                "    , bytestring",
                "    , HUnit",
                "  ghc-options:   -O2 -Weverything -Werror -threaded",
                ""
              ]
          )
          (renderScaffoldHaskellCabal "demo"),
      TestCase $ do
        let pythonScaffoldFiles = renderScaffoldFiles PythonPackage "demo" Nothing
        assertEqual
          "Uses the embedded Python baseline for scaffold default.nix."
          (Just pythonTemplateBaselineNixSource)
          (listToMaybe [scaffoldFileContents scaffoldFile | scaffoldFile <- pythonScaffoldFiles, scaffoldFileRelativePath scaffoldFile == "default.nix"]),
      TestCase $ do
        let pythonScaffoldFiles = renderScaffoldFiles PythonPackage "demo" (Just "Custom Python package")
        assertEqual
          "Lets package authors override the Python scaffold description."
          (Just (renderPythonTemplateBaselineNixSourceWith "Custom Python package" defaultPythonPackageAttribute))
          (listToMaybe [scaffoldFileContents scaffoldFile | scaffoldFile <- pythonScaffoldFiles, scaffoldFileRelativePath scaffoldFile == "default.nix"]),
      TestCase $ do
        assertEqual
          "Maps repository check names consistently for Python mutation testing."
          (Just "demo_mutation_testing")
          (repositoryCheckNameForKind PythonPackage "demo" RepositoryMutationTestingCheck),
      TestCase $ do
        assertEqual
          "Rejects unsupported repository checks for package type."
          (Just "unsupported checks for package type html: --mutation-testing")
          (validateRepositoryCheckSelection HtmlPackage (Set.fromList [RepositoryMutationTestingCheck])),
      TestCase $
        withTemporaryPackageRepository "python-create-package" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            createPackageResult <- createPackageInCurrentRepository PythonPackage "demo" Nothing
            assertEqual
              "Creates a Python package scaffold in the current repository."
              (Right ["packages/demo/.gitignore", "packages/demo/default.nix", "packages/demo/main.py"])
              createPackageResult
            packagePaths <- collectRepositoryPaths "packages"
            assertBool
              "Creates a recognizable Python package structure."
              ("packages/demo/main.py" `elem` packagePaths),
      TestCase $
        withTemporaryPackageRepository "existing-create-package" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            createDirectoryIfMissing True ("packages" </> "demo")
            createPackageResult <- createPackageInCurrentRepository RustPackage "demo" Nothing
            assertEqual
              "Fails fast when the target package already exists."
              (Left "package already exists: packages/demo")
              createPackageResult,
      TestCase $
        withTemporaryPackageRepository "haskell-create-package" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            _ <- createPackageInCurrentRepository HaskellPackage "demo" Nothing
            repositoryPaths <- collectRepositoryPaths "."
            let relativePaths = sort [path | path <- repositoryPaths, path /= "."]
                leafPaths = Set.fromList (filter (isLeafPath relativePaths) relativePaths)
                packageInfo = buildPackageInfo leafPaths ("packages" </> "demo")
            assertEqual
              "Creates a Haskell package structure that is recognized by package detection."
              HaskellPackage
              (detectedPackageKind packageInfo),
      TestCase $
        withTemporaryPackageRepository "python-create-repository" $ \tempRepository -> do
          withCurrentWorkingDirectory tempRepository $ do
            createRepositoryResult <-
              createRepositoryInCurrentRepository
                PythonPackage
                "demo"
                (Set.fromList [RepositoryCoverageCheck, RepositoryMutationTestingCheck])
            assertEqual
              "Creates a package scaffold together with the requested Python checks."
              ( Right
                  [ "packages/demo/.gitignore",
                    "packages/demo/default.nix",
                    "packages/demo/main.py",
                    "checks/demo_coverage/default.nix",
                    "checks/demo_mutation_testing/default.nix"
                  ]
              )
              createRepositoryResult
    ]
withTemporaryPackageRepository :: String -> (FilePath -> IO a) -> IO a
withTemporaryPackageRepository templateName action = do
  (temporaryPath, temporaryHandle) <- openTempFile "/tmp" templateName
  hClose temporaryHandle
  removeFile temporaryPath
  createDirectoryIfMissing True temporaryPath
  action temporaryPath
withCurrentWorkingDirectory :: FilePath -> IO a -> IO a
withCurrentWorkingDirectory workingDirectory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory workingDirectory
  action `finally` setCurrentDirectory previousDirectory
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
legacyPythonTemplateNixFixture :: String
legacyPythonTemplateNixFixture =
  "{\n"
    ++ "  inputs,\n"
    ++ "  pkgs ? import <nixpkgs> { },\n"
    ++ "}:\n"
    ++ "let\n"
    ++ "  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {\n"
    ++ "    secrets.secrets.file = ../../secrets/secrets.age;\n"
    ++ "  };\n"
    ++ "  pyPkgs = pkgs.python312Packages;\n"
    ++ "  python = pkgs.python312;\n"
    ++ "in\n"
    ++ "pyPkgs.buildPythonPackage rec {\n"
    ++ "  installPhase = ''\n"
    ++ "    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py\n"
    ++ "    install -Dm755 main.py $out/bin/${pname}\n"
    ++ "    if [ -d prm ]; then\n"
    ++ "      cp -r prm/ $out/${python.sitePackages}/\n"
    ++ "      cp -r prm/ $out/bin/\n"
    ++ "    fi\n"
    ++ "  '';\n"
    ++ "  meta.mainProgram = pname;\n"
    ++ "  pname = baseNameOf ./.;\n"
    ++ "  propagatedBuildInputs = [\n"
    ++ "    pyPkgs.hypothesis\n"
    ++ "  ];\n"
    ++ "  pyproject = false;\n"
    ++ "  shellHook = ''\n"
    ++ "    source ${pkgs.lib.getExe installationScript}\n"
    ++ "    export $secrets\n"
    ++ "  '';\n"
    ++ "  src = ./.;\n"
    ++ "  strictDeps = true;\n"
    ++ "  version = \"0.0.0\";\n"
    ++ "}\n"
pythonLatexNixFixture :: String
pythonLatexNixFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "let python = pkgs.python3; in\n"
    ++ "python.pkgs.buildPythonPackage rec {\n"
    ++ "  installPhase = '' latexmk -cd -pdf tmp/ms.tex '';\n"
    ++ "}\n"
pythonPypiNixFixture :: String
pythonPypiNixFixture =
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
exampleCargoTomlFixture :: T.Text
exampleCargoTomlFixture =
  T.unlines
    [ "[[bin]]",
      "name = \"example-package\"",
      "path = \"src/main.rs\"",
      "",
      "[dependencies]",
      "clap = {version = \"4.5\", features = [\"derive\"]}",
      "fd-lock = \"4.0\"",
      "git2 = \"0.19\"",
      "idna = \"1.1\"",
      "regex = \"1.10\"",
      "walkdir = \"2.5\"",
      "",
      "[package]",
      "name = \"example-package\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      "description = \"Example package fixture for TOML parsing.\"",
      "license = \"MIT\"",
      "repository = \"https://github.com/pbizopoulos/canonicalization\"",
      "readme = \"../../README\"",
      "keywords = [\"check\", \"lint\", \"fixture\"]",
      "categories = [\"development-tools\"]",
      ""
    ]
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
      "import System.Environment (getArgs, lookupEnv)",
      "import System.Exit (exitFailure)",
      "import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertEqual, runTestTT)",
      "",
      "renderMessage :: String",
      "renderMessage = \"Hello World Haskell\"",
      "",
      "runPackageTests :: IO ()",
      "runPackageTests = do",
      "  counts <- runTestTT hUnitDebugTests",
      "  if errors counts == 0 && failures counts == 0",
      "    then putStrLn \"test ... ok\"",
      "    else exitFailure",
      "",
      "hUnitDebugTests :: Test",
      "hUnitDebugTests =",
      "  TestList",
      "    [ TestCase $ do",
      "        assertEqual \"renders the sample message\" \"Hello World Haskell\" renderMessage",
      "    ]",
      "",
      "main :: IO ()",
      "main = do",
      "  _ <- getArgs",
      "  putStrLn renderMessage"
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
pyprojectTomlDescriptionFixture :: T.Text
pyprojectTomlDescriptionFixture =
  T.unlines
    [ "[project]",
      "name = \"example-package\"",
      "description = \"Example Python package.\"",
      "version = \"0.1.0\"",
      ""
    ]
pythonTemplateDefaultNixFixture :: T.Text
pythonTemplateDefaultNixFixture =
  T.unlines
    [ "python.pkgs.buildPythonPackage rec {",
      "  meta.description = \"A Python template package.\";",
      "}"
    ]
cTemplateDefaultNixFixture :: T.Text
cTemplateDefaultNixFixture =
  T.unlines
    [ "pkgs.stdenv.mkDerivation rec {",
      "  meta = {",
      "    description = \"A C template package.\";",
      "    mainProgram = pname;",
      "  };",
      "}"
    ]
pythonLatexTemplateDefaultNixFixture :: T.Text
pythonLatexTemplateDefaultNixFixture =
  T.unlines
    [ "python.pkgs.buildPythonPackage rec {",
      "  meta = {",
      "    description = \"A Python and LaTeX template package.\";",
      "    mainProgram = pname;",
      "  };",
      "}"
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
pythonPypiTemplateBaselineNixSource :: T.Text
pythonPypiTemplateBaselineNixSource = pythonPypiTemplateBaselineNixSourceWith defaultPythonPackageAttribute
pythonPypiTemplateBaselineNixSourceWith :: String -> T.Text
pythonPypiTemplateBaselineNixSourceWith pythonPackageAttribute =
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
pythonPypiApplicationTemplateBaselineNixSource :: T.Text
pythonPypiApplicationTemplateBaselineNixSource = pythonPypiApplicationTemplateBaselineNixSourceWith defaultPythonPackageAttribute
pythonPypiApplicationTemplateBaselineNixSourceWith :: String -> T.Text
pythonPypiApplicationTemplateBaselineNixSourceWith pythonPackageAttribute =
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
      "  debugGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);",
      "  packageDrv = import (../.. + \"/packages/${packageName}/default.nix\") {",
      "    inherit pkgs;",
      "  };",
      "  packageName = pkgs.lib.removeSuffix \"-coverage\" checkName;",
      "in",
      "pkgs.runCommand checkName",
      "  {",
      "    nativeBuildInputs = [",
      "      debugGhc",
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
      "  debugGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);",
      "  packageDrv = import (../.. + \"/packages/${packageName}/default.nix\") {",
      "    inherit pkgs;",
      "  };",
      "  packageName = pkgs.lib.removeSuffix \"-property-testing\" checkName;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = [",
      "      debugGhc",
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
      "    \"${debugGhc}/bin/ghc\" \\",
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
pythonMutationTestingCheckBaselineNixSource :: T.Text
pythonMutationTestingCheckBaselineNixSource = pythonMutationTestingCheckBaselineNixSourceWith defaultPythonPackageAttribute
pythonMutationTestingCheckBaselineNixSourceWith :: String -> T.Text
pythonMutationTestingCheckBaselineNixSourceWith pythonPackageAttribute =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs,",
      "  ...",
      "}:",
      "let",
      "  checkName = builtins.baseNameOf ./.;",
      "  packageName = pkgs.lib.removeSuffix \"_mutation_testing\" checkName;",
      "in",
      "pkgs.runCommand \"${checkName}\"",
      "  {",
      "    nativeBuildInputs = [",
      T.pack ("      (pkgs." ++ pythonPackageAttribute ++ ".withPackages ("),
      "        _: inputs.self.packages.${pkgs.stdenv.system}.${packageName}.propagatedBuildInputs",
      "      ))",
      "      inputs.self.packages.${pkgs.stdenv.system}.cosmic_ray",
      "    ];",
      "    src = ../../packages/${packageName};",
      "  }",
      "  ''",
      "    export HOME=\"$PWD\"",
      "    workspace=\"$PWD/workspace\"",
      "    rm -rf \"$workspace\"",
      "    mkdir -p \"$workspace\"",
      "    cp -R --no-preserve=mode \"$src\"/. \"$workspace\"",
      "    cd \"$workspace\"",
      "    cat > cosmic-ray.toml <<'EOF'",
      "    [cosmic-ray]",
      "    module-path = \"main.py\"",
      "    timeout = 10.0",
      "    excluded-modules = []",
      "    test-command = \"python3 -m pytest -v main.py\"",
      "    [cosmic-ray.distributor]",
      "    name = \"local\"",
      "    EOF",
      "    cosmic-ray init cosmic-ray.toml cosmic-ray.sqlite",
      "    cosmic-ray exec cosmic-ray.toml cosmic-ray.sqlite",
      "    cr-report cosmic-ray.sqlite",
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
