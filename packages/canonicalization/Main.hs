{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main) where
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (forM, forM_, unless, when)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, nub, sort, sortBy, stripPrefix)
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
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, listDirectory, removeFile, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
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
        defaultNixTemplateMatches = \_ nixSource -> pure ("writeShellScriptBin" `isInfixOf` nixSource),
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
      { defaultNixTemplateName = "python_pypi_template",
        defaultNixTemplateMatches = matchesPythonPypiTemplate,
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["nativeBuildInputs", "propagatedBuildInputs", "src", "version"],
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
        defaultNixTemplateAllowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "shellHook", "version"],
        defaultNixTemplateBaselineSource = Just pythonTemplateBaselineNixSource
      },
    DefaultNixTemplateSpec
      { defaultNixTemplateName = "deploy_host_template",
        defaultNixTemplateMatches = \_ nixSource ->
          pure
            ( "writeShellApplication" `isInfixOf` nixSource
                && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource)
            ),
        defaultNixTemplateAllowedDifferenceKeys = defaultAllowedNixDifferenceKeys,
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
matchesPythonPypiTemplate :: FilePath -> String -> IO Bool
matchesPythonPypiTemplate _ nixSource =
  pure
    ( "buildPythonPackage" `isInfixOf` nixSource
        && not ("src = ./.;" `isInfixOf` nixSource)
        && ("fetchPypi" `isInfixOf` nixSource || "fetchurl" `isInfixOf` nixSource)
    )
matchesBinaryReleaseTemplate :: FilePath -> String -> IO Bool
matchesBinaryReleaseTemplate _ nixSource =
  pure
    ( "stdenv.mkDerivation" `isInfixOf` nixSource
        && "src = pkgs.fetchurl" `isInfixOf` nixSource
        && "sourceRoot = \".\";" `isInfixOf` nixSource
        && "install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname}" `isInfixOf` nixSource
    )
defaultNixTemplateSpecByName :: FilePath -> Maybe DefaultNixTemplateSpec
defaultNixTemplateSpecByName defaultNixTemplateNameToFind = find ((== defaultNixTemplateNameToFind) . defaultNixTemplateName) defaultNixTemplateSpecs
type CheckOutcome :: Type
data CheckOutcome = CheckPassed | CheckFailed | CheckSkipped | CheckIncompatible deriving stock (Eq, Show)
checkOutcomeFromIssues :: [a] -> CheckOutcome
checkOutcomeFromIssues = \case [] -> CheckPassed; _ -> CheckFailed
type PackageTest :: Type
data PackageTest = PackageTest
  { packageTestName :: String,
    packageTestOutcome :: CheckOutcome,
    packageTestCases :: [PackageTestCase]
  }
type PackageTestCase :: Type
data PackageTestCase
  = PackageTestCase
  { packageTestCaseName :: String,
    packageTestCaseOutcome :: CheckOutcome,
    packageTestCaseIssues :: [String]
  }
type PackageCheck :: Type
data PackageCheck = PackageCheck
  { packageCheckName :: String,
    packageCheckKind :: PackageKind,
    packageCheckTests :: [PackageTest],
    packageCheckIssues :: [String]
  }
main :: IO ()
main = do
  debugMode <- lookupEnv "DEBUG"
  commandLineArgs <- getArgs
  case debugMode of
    Just "1" -> runDebugTests
    _ -> runCli commandLineArgs
runCli :: [String] -> IO ()
runCli commandLineArgs =
  case commandLineArgs of
    ["check-repository"] -> runInGitRepositoryRoot "." runCheckRepositoryMode
    ["check-repository", repositoryDirectory] -> runInGitRepositoryRoot repositoryDirectory runCheckRepositoryMode
    ["check-gitmodules"] -> runCheckGitSubmodulesMode
    _ -> do
      putStrLn "Usage: canonicalization check-repository [git-directory]"
      putStrLn "       canonicalization check-gitmodules"
      exitFailure
runInGitRepositoryRoot :: FilePath -> IO a -> IO a
runInGitRepositoryRoot repositoryDirectory action = do
  isDirectory <- doesDirectoryExist repositoryDirectory
  unless isDirectory $ do
    putStrLn ("not a directory: " ++ repositoryDirectory)
    exitFailure
  (insideWorkTreeExit, insideWorkTreeStdout, _insideWorkTreeStderr) <- readProcessWithExitCode "git" ["-C", repositoryDirectory, "rev-parse", "--is-inside-work-tree"] ""
  unless (insideWorkTreeExit == ExitSuccess && trimString insideWorkTreeStdout == "true") $ do
    putStrLn ("not a git directory: " ++ repositoryDirectory)
    exitFailure
  (repositoryRootExit, repositoryRootStdout, _repositoryRootStderr) <- readProcessWithExitCode "git" ["-C", repositoryDirectory, "rev-parse", "--show-toplevel"] ""
  unless (repositoryRootExit == ExitSuccess) $ do
    putStrLn ("not a git directory: " ++ repositoryDirectory)
    exitFailure
  canonicalInputDirectory <- canonicalizePath repositoryDirectory
  canonicalRepositoryRoot <- canonicalizePath (trimString repositoryRootStdout)
  unless (canonicalInputDirectory == canonicalRepositoryRoot) $ do
    putStrLn ("not a git repository root directory: " ++ repositoryDirectory)
    exitFailure
  setCurrentDirectory canonicalInputDirectory
  action
trimString :: String -> String
trimString = T.unpack . T.strip . T.pack
runCheckRepositoryMode :: IO ()
runCheckRepositoryMode = do
  repositoryStructureIssues <- checkRepositoryStructure
  unless (null repositoryStructureIssues) $ do
    reportCheckRepositoryFailures "directory-structure" repositoryStructureIssues
    exitFailure
  packageNames <- listPackageNames
  packageChecks <- forM packageNames (checkPackage [])
  let fileComplianceIssues = concatMap packageCheckIssues packageChecks
  unless (null fileComplianceIssues) $ do
    reportCheckRepositoryFailures "file-compliance" fileComplianceIssues
    exitFailure
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
  gitSubmoduleRepositories <- loadHomeGitSubmoduleRepositories
  let invalidGitSubmodulePathEntries = [gitSubmoduleRepositoryPathEntry gitSubmoduleRepository | gitSubmoduleRepository <- gitSubmoduleRepositories, not (gitSubmoduleRepositoryIsCompatible gitSubmoduleRepository)]
  if null invalidGitSubmodulePathEntries
    then putStrLn "all .gitmodules path entries comply with go-style naming (<host>/<owner>/<repo>)"
    else do
      forM_ invalidGitSubmodulePathEntries $ \invalidGitSubmodulePathEntry ->
        putStrLn (invalidGitSubmodulePathEntry ++ ": must be exactly <host>/<owner>/<repo>")
      exitFailure
type GitSubmoduleRepository :: Type
data GitSubmoduleRepository = GitSubmoduleRepository
  { gitSubmoduleRepositoryHost :: String,
    gitSubmoduleRepositoryOwner :: String,
    gitSubmoduleRepositoryName :: String,
    gitSubmoduleRepositoryPathEntry :: FilePath,
    gitSubmoduleRepositoryPath :: FilePath,
    gitSubmoduleRepositoryIsCompatible :: Bool
  }
loadHomeGitSubmoduleRepositories :: IO [GitSubmoduleRepository]
loadHomeGitSubmoduleRepositories = do
  homeDirectory <- getHomeDirectory
  let gitSubmodulesFilePath = homeDirectory </> ".gitmodules"
  fileExists <- doesFileExist gitSubmodulesFilePath
  unless fileExists $ do
    putStrLn ("missing file: " ++ gitSubmodulesFilePath)
    exitFailure
  gitSubmodulesContents <- T.unpack <$> TIO.readFile gitSubmodulesFilePath
  let gitSubmodulePathEntries = parseGitSubmodulePathEntries gitSubmodulesContents
  pure (map (buildGitSubmoduleRepository homeDirectory) gitSubmodulePathEntries)
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
    [ trimString rawPathEntry
    | gitSubmodulesLine <- lines gitSubmodulesContents,
      let trimmedLine = trimString gitSubmodulesLine,
      "path" `isPrefixOf` trimmedLine,
      "=" `isInfixOf` trimmedLine,
      let rawPathEntry = drop 1 (dropWhile (/= '=') trimmedLine),
      not (null (trimString rawPathEntry))
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
          not (any (`pathMatchesRegex` path) allowedPathRegexes)
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
pathMatchesRegex :: String -> FilePath -> Bool
pathMatchesRegex regexPattern path = path =~ regexPattern
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
listPackageNames :: IO [FilePath]
listPackageNames = listSubdirectoryNames "packages"
listSubdirectoryNames :: FilePath -> IO [FilePath]
listSubdirectoryNames parentDirectory = do
  parentDirectoryExists <- doesDirectoryExist parentDirectory
  if not parentDirectoryExists
    then pure []
    else do
      childNames <- listDirectory parentDirectory
      childIsDirectoryFlags <- forM childNames $ \childName -> doesDirectoryExist (parentDirectory </> childName)
      pure $ sort [childName | (childName, isDirectory) <- zip childNames childIsDirectoryFlags, isDirectory]
checkPackage :: [String] -> FilePath -> IO PackageCheck
checkPackage allRepositoryStructureIssues packageName = do
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
                      defaultNixTemplateComparisonIssues <- comparePackageDefaultNixWithTemplate packageName packageDefaultNixPath ("packages" </> matchedDefaultNixTemplateName </> "default.nix") allowedNixDifferenceKeysForPackage (Just defaultNixTemplateSource)
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
  pythonDebugTestIssues <- checkPythonDebugTests packageName packageKind
  haskellDebugTestIssues <- checkHaskellDebugTests packageName packageKind
  rustDebugTestIssues <- checkRustDebugTests packageName packageKind
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
      pythonDebugTestOutcome = if packageKind `elem` [PythonPackage, PythonLatexPackage] then checkOutcomeFromIssues pythonDebugTestIssues else CheckSkipped
      haskellDebugTestOutcome = if packageKind == HaskellPackage then checkOutcomeFromIssues haskellDebugTestIssues else CheckSkipped
      rustDebugTestOutcome = if packageKind == RustPackage then checkOutcomeFromIssues rustDebugTestIssues else CheckSkipped
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
                    rustDebugTestOutcome
                    ( makePackageTestCase
                        "supports DEBUG test execution"
                        rustDebugTestOutcome
                        rustDebugTestIssues
                        : [PackageTestCase rustUnitTestName CheckSkipped [] | rustUnitTestName <- rustUnitTestNames]
                    )
                ]
              else [],
            if packageKind == HaskellPackage
              then
                [ makePackageTest (packageName ++ ".cabal") cabalFileOutcome "matches Cabal conventions" cabalFileIssues,
                  PackageTest
                    "Main.hs"
                    haskellDebugTestOutcome
                    ( makePackageTestCase
                        "supports DEBUG test execution"
                        haskellDebugTestOutcome
                        haskellDebugTestIssues
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
                pythonDebugTestOutcome
                ( makePackageTestCase
                    "supports DEBUG test execution"
                    pythonDebugTestOutcome
                    pythonDebugTestIssues
                    : [PackageTestCase pythonUnitTestName CheckSkipped [] | pythonUnitTestName <- pythonUnitTestNames]
                )
            | packageKind `elem` [PythonPackage, PythonLatexPackage]
            ]
          ]
      packageTests = basePackageTests ++ languageSpecificPackageTests
      _packageTestSelectorReferences =
        [ ( packageTestName packageTest,
            packageTestOutcome packageTest,
            [ (packageTestCaseName packageTestCase, packageTestCaseOutcome packageTestCase, packageTestCaseIssues packageTestCase)
            | packageTestCase <- packageTestCases packageTest
            ]
          )
        | packageTest <- packageTests
        ]
      packageIssues =
        packageStructureIssues
          ++ defaultNixTemplateIssues
          ++ defaultNixConventionIssues
          ++ cargoTomlIssues
          ++ cabalFileIssues
          ++ pythonDebugTestIssues
          ++ haskellDebugTestIssues
          ++ rustDebugTestIssues
  pure
    PackageCheck
      { packageCheckName = packageName,
        packageCheckKind = packageKind,
        packageCheckTests = packageTests,
        packageCheckIssues = packageIssues
      }
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
             in "buildPythonPackage" `isInfixOf` packageDefaultNixSourceString
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
          hasMetaMainProgram = "meta.mainProgram = pname;" `isInfixOf` defaultNixSource
          hasExternalFetchUrlSource = "src = pkgs.fetchurl" `isInfixOf` defaultNixSource
          hasLocalSource = "src = ./.;" `isInfixOf` defaultNixSource
          hasPlaceholderVersion = "version = \"0.0.0\";" `isInfixOf` defaultNixSource
          hasVersionAssignment = "version = \"" `isInfixOf` defaultNixSource
          expectsMetaMainProgram =
            packageKind `elem` [RustPackage, PythonLatexPackage, PythonPackage, CPackage, LatexPackage, BinaryReleasePackage]
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
checkPythonDebugTests :: FilePath -> PackageKind -> IO [String]
checkPythonDebugTests packageName packageKind =
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
              (exitCode, validatorStdout, validatorStderr) <- readProcessWithExitCode pythonCommand ["-c", pythonDebugTestValidatorPythonSource, mainPythonPath] ""
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
        Just mainPythonSourceText -> do
          let extractedPythonUnitTestNames =
                [ functionName
                | sourceLine <- lines (T.unpack mainPythonSourceText),
                  Just functionName <- [extractPythonUnitTestName sourceLine]
                ]
          pure (sort (Set.toList (Set.fromList extractedPythonUnitTestNames)))
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
checkHaskellDebugTests :: FilePath -> PackageKind -> IO [String]
checkHaskellDebugTests packageName packageKind =
  if packageKind /= HaskellPackage
    then pure []
    else do
      let mainHaskellPath = "packages" </> packageName </> "Main.hs"
      maybeMainHaskellSourceText <- readTextFileIfExists mainHaskellPath
      case maybeMainHaskellSourceText of
        Nothing -> pure []
        Just mainHaskellSourceText -> do
          let haskellSource = T.unpack mainHaskellSourceText
              hasDebugEnvironmentGate = "lookupEnv \"DEBUG\"" `isInfixOf` haskellSource
              hasHUnitTestRunner = "runTestTT" `isInfixOf` haskellSource || "runDebugTests" `isInfixOf` haskellSource
              hasDebugBranchInMain =
                "Just \"1\" ->" `isInfixOf` haskellSource
                  || "(Just \"1\", " `isInfixOf` haskellSource
          pure $
            catMaybes
              [ if hasDebugEnvironmentGate
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: missing DEBUG environment check (lookupEnv \"DEBUG\")"),
                if hasDebugBranchInMain
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: main() must branch on DEBUG=1"),
                if hasHUnitTestRunner
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: DEBUG=1 branch must run HUnit tests (runTestTT)")
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
        Just mainHaskellSourceText -> do
          let haskellSourceLines = lines (T.unpack mainHaskellSourceText)
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
          pure (sort (Set.toList (Set.fromList discoveredHaskellUnitTestNames)))
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
      | "makeFormattingTest" `isInfixOf` line = go True rest
      | awaitingMakeFormattingTestLabel =
          let trimmed = dropWhile (== ' ') line
           in if null trimmed
                then go True rest
                else case firstQuotedToken line of
                  Just label -> label : go False rest
                  Nothing -> go False rest
      | otherwise = go False rest
firstQuotedToken :: String -> Maybe String
firstQuotedToken inputText =
  let firstTokenAfter quoteCharacter =
        case dropWhile (/= quoteCharacter) inputText of
          _ : rest ->
            let token = takeWhile (/= quoteCharacter) rest
             in if null token then Nothing else Just token
          _ -> Nothing
   in firstTokenAfter '"' <|> firstTokenAfter '\''
checkRustDebugTests :: FilePath -> PackageKind -> IO [String]
checkRustDebugTests packageName packageKind =
  if packageKind /= RustPackage
    then pure []
    else do
      let mainRustPath = "packages" </> packageName </> "src/main.rs"
          packageDefaultNixPath = "packages" </> packageName </> "default.nix"
      mainRustFileExists <- doesFileExist mainRustPath
      packageDefaultNixFileExists <- doesFileExist packageDefaultNixPath
      mainRustSource <-
        if mainRustFileExists
          then T.unpack <$> TIO.readFile mainRustPath
          else pure ""
      packageDefaultNixSource <-
        if packageDefaultNixFileExists
          then T.unpack <$> TIO.readFile packageDefaultNixPath
          else pure ""
      let hasRustTestModule = "#[cfg(test)]" `isInfixOf` mainRustSource && "mod tests" `isInfixOf` mainRustSource
          hasRustTestCases = "#[test]" `isInfixOf` mainRustSource
          hasDebugGateInDefaultNix = "DEBUG" `isInfixOf` packageDefaultNixSource && "cargo test" `isInfixOf` packageDefaultNixSource
      pure $
        catMaybes
          [ if hasRustTestModule
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[cfg(test)] mod tests"),
            if hasRustTestCases
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[test] test cases"),
            if hasDebugGateInDefaultNix
              then Nothing
              else Just ("packages/" ++ packageName ++ "/default.nix: DEBUG mode must run cargo test")
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
        Just mainRustSourceText -> pure (extractRustUnitTestNames (lines (T.unpack mainRustSourceText)))
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
        "missing_main_function" -> messagePrefix ++ "missing main() function"
        "missing_debug_gate" -> messagePrefix ++ "main() must include a DEBUG gate"
        "noncanonical_debug_gate" -> messagePrefix ++ "main() DEBUG gate must check DEBUG == \"1\""
        "debug_branch_no_unittest" -> messagePrefix ++ "DEBUG branch in main() must run unittest"
        "run_tests_missing_unittest" -> messagePrefix ++ "run_tests() is called from DEBUG branch but does not run unittest"
        "parse_error" -> messagePrefix ++ "python source could not be parsed"
        _ -> messagePrefix ++ "python validator failed with error code: " ++ errorCode
pythonDebugTestValidatorPythonSource :: String
pythonDebugTestValidatorPythonSource =
  unlines
    [ "import ast",
      "import sys",
      "",
      "def _is_os_getenv_debug(node):",
      "    if not isinstance(node, ast.Call):",
      "        return False",
      "    function = node.func",
      "    if not isinstance(function, ast.Attribute):",
      "        return False",
      "    if isinstance(function.value, ast.Name) and function.value.id == 'os' and function.attr == 'getenv':",
      "        if not node.args:",
      "            return False",
      "        first = node.args[0]",
      "        return isinstance(first, ast.Constant) and first.value == 'DEBUG'",
      "    if isinstance(function.value, ast.Attribute) and function.attr == 'get':",
      "        environment_access = function.value",
      "        if isinstance(environment_access.value, ast.Name) and environment_access.value.id == 'os' and environment_access.attr == 'environ':",
      "            if not node.args:",
      "                return False",
      "            first = node.args[0]",
      "            return isinstance(first, ast.Constant) and first.value == 'DEBUG'",
      "    return False",
      "",
      "def _contains_debug_gate(expression):",
      "    return any(_is_os_getenv_debug(node) for node in ast.walk(expression))",
      "",
      "def _contains_canonical_debug_gate(expression):",
      "    for node in ast.walk(expression):",
      "        if not isinstance(node, ast.Compare):",
      "            continue",
      "        if len(node.ops) != 1 or len(node.comparators) != 1:",
      "            continue",
      "        if not isinstance(node.ops[0], ast.Eq):",
      "            continue",
      "        if not _is_os_getenv_debug(node.left):",
      "            continue",
      "        comparator = node.comparators[0]",
      "        if isinstance(comparator, ast.Constant) and comparator.value == '1':",
      "            return True",
      "    return False",
      "",
      "def _is_unittest_main_call(node):",
      "    if not isinstance(node, ast.Call):",
      "        return False",
      "    function = node.func",
      "    return isinstance(function, ast.Attribute) and isinstance(function.value, ast.Name) and function.value.id == 'unittest' and function.attr == 'main'",
      "",
      "def _contains_unittest_runner(statements):",
      "    for statement in statements:",
      "        for node in ast.walk(statement):",
      "            if not isinstance(node, ast.Call):",
      "                continue",
      "            function = node.func",
      "            if isinstance(function, ast.Attribute) and isinstance(function.value, ast.Name) and function.value.id == 'unittest':",
      "                if function.attr in {'main', 'TextTestRunner', 'defaultTestLoader'}:",
      "                    return True",
      "    return False",
      "",
      "def _is_run_tests_call(node):",
      "    return isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == 'run_tests'",
      "",
      "def _branch_runs_unittest(branch_statements, functions):",
      "    if _contains_unittest_runner(branch_statements):",
      "        return True, False",
      "    run_tests_called = False",
      "    for statement in branch_statements:",
      "        for node in ast.walk(statement):",
      "            if _is_run_tests_call(node):",
      "                run_tests_called = True",
      "    if run_tests_called and 'run_tests' in functions:",
      "        if _contains_unittest_runner(functions['run_tests'].body):",
      "            return True, False",
      "        return False, True",
      "    return False, False",
      "",
      "def main():",
      "    path = sys.argv[1]",
      "    try:",
      "        source = open(path, encoding='utf-8').read()",
      "        module = ast.parse(source, filename=path)",
      "    except Exception:",
      "        print('ERR parse_error')",
      "        sys.exit(2)",
      "",
      "    functions = {}",
      "    for node in module.body:",
      "        if isinstance(node, ast.FunctionDef):",
      "            functions[node.name] = node",
      "",
      "    errors = []",
      "    if 'main' not in functions:",
      "        errors.append('missing_main_function')",
      "    else:",
      "        main_function = functions['main']",
      "        debug_if_nodes = [node for node in ast.walk(main_function) if isinstance(node, ast.If) and _contains_debug_gate(node.test)]",
      "        if not debug_if_nodes:",
      "            errors.append('missing_debug_gate')",
      "        else:",
      "            if not any(_contains_canonical_debug_gate(node.test) for node in debug_if_nodes):",
      "                errors.append('noncanonical_debug_gate')",
      "            debug_branch_ok = False",
      "            run_tests_invalid = False",
      "            for if_node in debug_if_nodes:",
      "                branch_ok, run_tests_missing = _branch_runs_unittest(if_node.body, functions)",
      "                if branch_ok:",
      "                    debug_branch_ok = True",
      "                    break",
      "                if run_tests_missing:",
      "                    run_tests_invalid = True",
      "            if not debug_branch_ok:",
      "                if run_tests_invalid:",
      "                    errors.append('run_tests_missing_unittest')",
      "                errors.append('debug_branch_no_unittest')",
      "",
      "    if errors:",
      "        for err in errors:",
      "            print('ERR ' + err)",
      "        sys.exit(1)",
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
                      ++ "/Cargo.toml: only dependency sections may differ from the internal Rust Cargo baseline"
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
                      ++ ".cabal: only build-depends may differ from the internal Haskell cabal baseline"
                  )
          ]
normalizeCabalForBaselineComparison :: FilePath -> T.Text -> T.Text
normalizeCabalForBaselineComparison packageName cabalContents =
  let step (insideBuildDependsSection, normalizedLinesSoFar) sourceLine =
        let trimmedLine = T.strip sourceLine
            normalizedLine = normalizeCabalLineForBaselineComparison packageName trimmedLine
         in if insideBuildDependsSection
              then
                if T.null trimmedLine
                  then (True, normalizedLinesSoFar)
                  else case T.breakOn ":" trimmedLine of
                    (_, "") -> (True, normalizedLinesSoFar)
                    _ -> (False, normalizedLinesSoFar ++ [normalizedLine])
              else
                if "build-depends:" `T.isPrefixOf` trimmedLine
                  then (True, normalizedLinesSoFar)
                  else
                    if T.null trimmedLine
                      then (False, normalizedLinesSoFar)
                      else (False, normalizedLinesSoFar ++ [normalizedLine])
      (_, normalizedLines) = foldl' step (False, []) (T.lines cabalContents)
   in T.unlines normalizedLines
normalizeCabalLineForBaselineComparison :: FilePath -> T.Text -> T.Text
normalizeCabalLineForBaselineComparison packageName trimmedLine
  | "name:" `T.isPrefixOf` trimmedLine = "name:          " <> T.pack packageName
  | "executable " `T.isPrefixOf` trimmedLine = "executable " <> T.pack packageName
  | otherwise = trimmedLine
lookupCabalField :: T.Text -> T.Text -> Maybe T.Text
lookupCabalField cabalField cabalContents =
  let fieldPrefix = cabalField <> ":"
      maybeMatchingFieldLine = listToMaybe [T.strip cabalLine | cabalLine <- T.lines cabalContents, fieldPrefix `T.isPrefixOf` T.strip cabalLine]
   in do
        matchingLine <- maybeMatchingFieldLine
        fieldValue <- T.stripPrefix fieldPrefix matchingLine
        pure (T.strip fieldValue)
comparePackageDefaultNixWithTemplate :: FilePath -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
comparePackageDefaultNixWithTemplate packageName packageDefaultNixPath templateDefaultNixPath allowedNixDifferenceKeys maybeTemplateDefaultNixSourceOverride = do
  packageDefaultNixParseResult <- parseNixExprFromFile packageDefaultNixPath
  templateDefaultNixParseResult <-
    case maybeTemplateDefaultNixSourceOverride of
      Just templateDefaultNixSource -> parseNixExprFromText templateDefaultNixSource
      Nothing -> parseNixExprFromFile templateDefaultNixPath
  case (packageDefaultNixParseResult, templateDefaultNixParseResult) of
    (Left parseError, _) ->
      pure ["packages/" ++ packageName ++ "/default.nix: parse error: " ++ show parseError]
    (_, Left parseError) ->
      pure [templateDefaultNixPath ++ ": parse error: " ++ show parseError]
    (Right packageDefaultNixExpr, Right templateDefaultNixExpr) ->
      let normalizedPackageDefaultNixExpr = normalizeNixExpr allowedNixDifferenceKeys packageDefaultNixExpr
          normalizedTemplateDefaultNixExpr = normalizeNixExpr allowedNixDifferenceKeys templateDefaultNixExpr
       in pure $
            formatDefaultNixTemplateDifferences
              packageName
              templateDefaultNixPath
              normalizedPackageDefaultNixExpr
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
normalizeNixExpr :: Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeNixExpr allowedNixDifferenceKeys (Fix (Compose (AnnUnit nixExprSpan expressionFunctor))) =
  let rebuiltExpressionFunctor = case expressionFunctor of
        NSet isRecursive bindings -> NSet isRecursive (normalizeNixBindings allowedNixDifferenceKeys bindings)
        NLet bindings body -> NLet (normalizeNixBindings allowedNixDifferenceKeys bindings) (normalizeNixExpr allowedNixDifferenceKeys body)
        NAbs (ParamSet paramsEllipsis paramsAt params) body -> NAbs (ParamSet paramsEllipsis paramsAt (sortNixParams params)) (normalizeNixExpr allowedNixDifferenceKeys body)
        NAbs (Param paramName) body -> NAbs (Param paramName) (normalizeNixExpr allowedNixDifferenceKeys body)
        otherNixExpr -> fmap (normalizeNixExpr allowedNixDifferenceKeys) otherNixExpr
   in Fix (Compose (AnnUnit nixExprSpan rebuiltExpressionFunctor))
sortNixParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortNixParams = sortBy (\(VarName leftName, _) (VarName rightName, _) -> compare leftName rightName)
normalizeNixBindings :: Set.Set T.Text -> [Binding NExprLoc] -> [Binding NExprLoc]
normalizeNixBindings allowedNixDifferenceKeys bindings =
  [normalizeNixBinding allowedNixDifferenceKeys binding | binding <- bindings, not (isAllowedNixDifferenceBinding allowedNixDifferenceKeys binding)]
normalizeNixBinding :: Set.Set T.Text -> Binding NExprLoc -> Binding NExprLoc
normalizeNixBinding allowedNixDifferenceKeys = \case
  NamedVar keyPath bindingValue sourcePosition -> NamedVar keyPath (normalizeNixExpr allowedNixDifferenceKeys bindingValue) sourcePosition
  Inherit maybeBoundNixExpr inheritedNames sourcePosition -> Inherit (normalizeNixExpr allowedNixDifferenceKeys <$> maybeBoundNixExpr) inheritedNames sourcePosition
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
formatDefaultNixTemplateDifferences packageName templateDefaultNixPath packageDefaultNixExpr templateDefaultNixExpr =
  let renderedPackageDefaultNix = renderNixExpr packageDefaultNixExpr
      renderedTemplateDefaultNix = renderNixExpr templateDefaultNixExpr
   in if renderedPackageDefaultNix == renderedTemplateDefaultNix
        then []
        else
          let packageLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings packageDefaultNixExpr)
              templateLetBindingMap = fromMaybe Map.empty (extractOutermostLetBindings templateDefaultNixExpr)
              packagePrimaryBindingMap = fromMaybe Map.empty (extractPrimaryNixBindings packageDefaultNixExpr)
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
           in [ "packages/"
                  ++ packageName
                  ++ "/default.nix: differs from template "
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
extractPrimaryNixBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractPrimaryNixBindings nixExpression = do
  bindingGroups <- collectNixSetBindingGroups nixExpression
  pure $ Map.fromList (maximumByLength bindingGroups)
extractOutermostLetBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractOutermostLetBindings (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NAbs _ body -> extractOutermostLetBindings body
    NLet bindings _ -> Just (Map.fromList (extractNamedNixBindings bindings))
    _ -> Nothing
maximumByLength :: [[a]] -> [a]
maximumByLength = maximumBy (comparing length)
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
  [ (T.intercalate "." (mapMaybe nixKeyNameText (NE.toList keyPath)), renderNixExpr bindingValue)
  | NamedVar keyPath bindingValue _ <- bindings
  ]
runDebugTests :: IO ()
runDebugTests = do
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
  trimString (trimString inputText) == trimString inputText
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
        assertEqual
          "Compacts whitespace with compactTextToSingleLine."
          "a b c"
          (compactTextToSingleLine " a \n  b\t c "),
      TestCase $ do
        assertEqual
          "Formats binding difference details with formatNixBindingDifferenceLine."
          "  - missing key: src"
          (formatNixBindingDifferenceLine "missing key" "src"),
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
          "maximumByLength returns the longest list."
          ([1, 2, 3] :: [Int])
          (maximumByLength [[1 :: Int], [1, 2, 3], [1, 2]]),
      TestCase $ do
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
          "breakOnSubstring returns prefix and suffix when found."
          (Just ("hello ", " world"))
          (breakOnSubstring "~:" "hello ~: world"),
      TestCase $ do
        assertEqual
          "breakOnSubstring returns Nothing when missing."
          Nothing
          (breakOnSubstring "~:" "hello world"),
      TestCase $ do
        assertEqual
          "Extracts label from HUnit tilde syntax."
          (Just "alpha test")
          (extractHUnitTildeTestLabel "  \"alpha test\" ~: assertEqual \"x\" 1 1"),
      TestCase $ do
        assertEqual
          "Extracts the last quoted token."
          (Just "first")
          (lastQuotedToken "prefix \"first\" middle 'second' suffix"),
      TestCase $ do
        assertEqual
          "Extracts first quoted token."
          (Just "label one")
          (firstQuotedToken "assertEqual \"label one\" expected actual"),
      TestCase $ do
        assertEqual
          "Finds assertEqual labels from source lines."
          ["label-a"]
          ( extractAssertEqualTestLabels
              [ "  assertEqual",
                "    \"label-a\"",
                "  assertEqual \"label-b\" 1 1"
              ]
          ),
      TestCase $ do
        assertEqual
          "Finds makeFormattingTest labels from source lines."
          ["format one"]
          ( extractMakeFormattingTestLabels
              [ "makeFormattingTest",
                "  \"format one\"",
                "makeFormattingTest \"format two\""
              ]
          ),
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
          "pathMatchesRegex accepts matching paths."
          True
          (pathMatchesRegex "^packages/.+/Main\\.hs$" "packages/canonicalization/Main.hs"),
      TestCase $ do
        assertEqual
          "pathMatchesRegex rejects non-matching paths."
          False
          (pathMatchesRegex "^packages/.+/Main\\.hs$" "packages/canonicalization/default.nix"),
      TestCase $ do
        assertEqual
          "Detects Cargo dependency section headers."
          True
          (isCargoDependencySectionHeader "[target.x86_64-unknown-linux-gnu.dependencies]"),
      TestCase $ do
        assertEqual
          "Detects non-dependency section headers."
          False
          (isCargoDependencySectionHeader "[package]"),
      TestCase $ do
        assertEqual
          "Detects TOML section headers."
          True
          (isTomlSectionHeader "[package]"),
      TestCase $ do
        assertEqual
          "Rejects malformed TOML section headers."
          False
          (isTomlSectionHeader "package"),
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
          "Normalizes only cabal name lines when applicable."
          "name:          demo"
          (T.unpack (normalizeCabalLineForBaselineComparison "demo" "name: old")),
      TestCase $ do
        assertEqual
          "Leaves unrelated cabal lines unchanged."
          "version: 0.1.0"
          (T.unpack (normalizeCabalLineForBaselineComparison "demo" "version: 0.1.0")),
      TestCase $ do
        assertEqual
          "allowedPathRegexesForPackageKind includes expected Haskell paths."
          True
          ( let regexes = allowedPathRegexesForPackageKind "packages/demo" "demo" HaskellPackage
             in any (`pathMatchesRegex` "packages/demo/Main.hs") regexes
                  && any (`pathMatchesRegex` "packages/demo/demo.cabal") regexes
          ),
      TestCase $ do
        assertEqual
          "allowedPathRegexesForPackageKind includes Terraform lockfile pattern."
          True
          ( let regexes = allowedPathRegexesForPackageKind "packages/demo" "demo" TerraformPackage
             in any (`pathMatchesRegex` "packages/demo/.terraform.lock.hcl") regexes
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
          (gitSubmoduleRepositoryIsCompatible (buildGitSubmoduleRepository "/home/user" "github.com/pbizopoulos/canonicalization/subdir"))
    ]
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
    ++ "  installCheckPhase = ''\n"
    ++ "    runHook preInstallCheck\n"
    ++ "    HOME=\"$(mktemp -d)\"\n"
    ++ "    DEBUG=1 \"$out/bin/${pname}\"\n"
    ++ "    runHook postInstallCheck\n"
    ++ "  '';\n"
    ++ "  installPhase = ''\n"
    ++ "    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py\n"
    ++ "    install -Dm755 ./main.py $out/bin/${pname}\n"
    ++ "    if [ -d ./prm ]; then\n"
    ++ "      cp -r ./prm/ $out/${python.sitePackages}/\n"
    ++ "      cp -r ./prm/ $out/bin/\n"
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
    ++ "pkgs.writeShellScriptBin \"x\" ''\n"
    ++ "  echo hi\n"
    ++ "''\n"
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
      "description = \"A CLI tool to remove empty lines from files.\"",
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
      "synopsis:      Hello World Haskell",
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
      "pkgs.haskellPackages.mkDerivation rec {",
      "  executableHaskellDepends = [",
      "    pkgs.haskellPackages.HUnit",
      "    pkgs.haskellPackages.aeson",
      "    pkgs.haskellPackages.base",
      "    pkgs.haskellPackages.bytestring",
      "  ];",
      "  executableToolDepends = [",
      "    pkgs.makeWrapper",
      "  ];",
      "  mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  postInstall = ''",
      "    wrapProgram $out/bin/${pname} --run \"rm -f tmp/${pname}.tix\" --set-default HPCTIXFILE tmp/${pname}.tix",
      "    DEBUG=1 \"$out/bin/${pname}\"",
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
      "  wrapperScript = pkgs.writeShellScript \"${pname}-wrapper\" ''",
      "    set -euo pipefail",
      "    export PATH='${",
      "      pkgs.lib.makeBinPath [",
      "        pkgs.cargo",
      "        pkgs.rustc",
      "        pkgs.stdenv.cc",
      "      ]",
      "    }':\"$PATH\"",
      "    resolve_source_root() {",
      "      local candidate",
      "      local current_dir=\"$PWD\"",
      "      if [ -n \"''${CANONICALIZATION_ROOT:-}\" ]; then",
      "        candidate=\"$CANONICALIZATION_ROOT/packages/${pname}\"",
      "        if [ -f \"$candidate/Cargo.toml\" ]; then",
      "          printf '%s\\n' \"$candidate\"",
      "          return 0",
      "        fi",
      "      fi",
      "      while [ \"$current_dir\" != \"/\" ]; do",
      "        candidate=\"$current_dir/packages/${pname}\"",
      "        if [ -f \"$candidate/Cargo.toml\" ]; then",
      "          printf '%s\\n' \"$candidate\"",
      "          return 0",
      "        fi",
      "        current_dir=\"$(dirname \"$current_dir\")\"",
      "      done",
      "      if [ -f \"$PWD/Cargo.toml\" ]; then",
      "        printf '%s\\n' \"$PWD\"",
      "        return 0",
      "      fi",
      "      return 1",
      "    }",
      "    if [ \"''${DEBUG:-0}\" = \"1\" ]; then",
      "      if source_root=\"$(resolve_source_root)\"; then",
      "        cd \"$source_root\"",
      "        cargo test --locked",
      "      fi",
      "    fi",
      "    exec \"@wrappedBin@\" \"$@\"",
      "  '';",
      "in",
      "pkgs.rustPlatform.buildRustPackage {",
      "  inherit pname;",
      "  cargoHash = \"sha256-5FZKAFwP3QKw6KDiJsshJXkpU9jbUCeQStsTAkIfOjA=\";",
      "  doInstallCheck = pkgs.stdenv.isLinux;",
      "  env = {",
      "    RUSTDOCFLAGS = \"-D warnings\";",
      "    RUSTFLAGS = \"-D warnings\";",
      "  };",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    test -x \"$out/bin/${pname}\"",
      "    \"$out/bin/${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  meta.mainProgram = pname;",
      "  postInstall = ''",
      "    mv \"$out/bin/${pname}\" \"$out/bin/.${pname}-wrapped\"",
      "    install -m755 ${wrapperScript} \"$out/bin/${pname}\"",
      "    substituteInPlace \"$out/bin/${pname}\" \\",
      "      --replace-fail \"@wrappedBin@\" \"$out/bin/.${pname}-wrapped\"",
      "  '';",
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
      "let",
      "  pname = baseNameOf ./.;",
      "in",
      "pkgs.writeShellScriptBin pname ''",
      "  if [ \"$DEBUG\" != \"1\" ]; then",
      "    exec ${pkgs.http-server}/bin/http-server ${./.} \"$@\"",
      "  fi",
      "''",
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
      "    -Wattribute-alias \\",
      "    -Wattributes \\",
      "    -Wbad-function-cast \\",
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
      "    -Wformat-signedness \\",
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
      "    -Wformat=2 \\",
      "    -fanalyzer \\",
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
      "  meta.mainProgram = pname;",
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
      "  meta.mainProgram = pname;",
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
      "let",
      "  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {",
      "    secrets.secrets.file = ../../secrets/secrets.age;",
      "  };",
      "  packageName = baseNameOf ./.;",
      "  repoSrc = ../..;",
      "in",
      "pkgs.writeShellApplication {",
      "  name = packageName;",
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
      "    source ${pkgs.lib.getExe installationScript}",
      "    # shellcheck disable=SC2086,SC2163,SC2154",
      "    export $secrets",
      "    workdir=$(mktemp -d)",
      "    cp -r ${repoSrc}/. \"$workdir/\"",
      "    chmod -R u+w \"$workdir\"",
      "    rm -rf \"$workdir/packages/${packageName}/.terraform\" \"$workdir/packages/${packageName}/.terraform.lock.hcl\"",
      "    tofu -chdir=\"$workdir/packages/${packageName}\" init -reconfigure",
      "    tofu -chdir=\"$workdir/packages/${packageName}\" apply",
      "  '';",
      "}",
      ""
    ]
pythonTemplateBaselineNixSource :: T.Text
pythonTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {",
      "    secrets.secrets.file = ../../secrets/secrets.age;",
      "  };",
      "  python = pkgs.python312;",
      "in",
      "python.pkgs.buildPythonPackage rec {",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    HOME=\"$(mktemp -d)\"",
      "    DEBUG=1 \"$out/bin/${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    install -Dm644 ./main.py $out/${python.sitePackages}/${pname}.py",
      "    install -Dm755 ./main.py $out/bin/${pname}",
      "    if [ -d ./prm ]; then",
      "      cp -r ./prm/ $out/${python.sitePackages}/",
      "      cp -r ./prm/ $out/bin/",
      "    fi",
      "  '';",
      "  meta.mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  pyproject = false;",
      "  shellHook = ''",
      "    source ${pkgs.lib.getExe installationScript}",
      "    export $secrets",
      "  '';",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}"
    ]
pythonPypiTemplateBaselineNixSource :: T.Text
pythonPypiTemplateBaselineNixSource =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  python = pkgs.python312;",
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
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    HOME=\"$(mktemp -d)\"",
      "    DEBUG=1 \"$out/bin/${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    datadir=\"$out/share/${pname}\"",
      "    install -Dm644 ./main.py ./ms.tex ./ms.bib -t \"$datadir\"",
      "    mkdir -p \"$out/bin\"",
      "    cat > \"$out/bin/${pname}\" <<EOF",
      "    #!${pkgs.bash}/bin/bash",
      "    set -euo pipefail",
      "    mkdir -p tmp",
      "    ${pythonEnv}/bin/python3 \"$datadir/main.py\"",
      "    cp \"$datadir\"/ms.{tex,bib} tmp/",
      "    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex",
      "    EOF",
      "    chmod +x \"$out/bin/${pname}\"",
      "  '';",
      "  meta.mainProgram = pname;",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = pythonDeps;",
      "  pyproject = false;",
      "  src = ./.;",
      "  strictDeps = true;",
      "  version = \"0.0.0\";",
      "}"
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
      "  meta.mainProgram = pname;",
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
