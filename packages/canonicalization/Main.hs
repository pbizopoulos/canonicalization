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
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
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
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertEqual, runTestTT)
import Test.QuickCheck qualified as QC
import Text.Regex.TDFA ((=~))
import Prelude
defaultAllowedDifferenceKeys :: Set.Set T.Text
defaultAllowedDifferenceKeys =
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
type TemplateSpec :: Type
data TemplateSpec = TemplateSpec
  { templateName :: FilePath,
    templateMatches :: FilePath -> String -> IO Bool,
    templateAllowedDifferenceKeys :: Set.Set T.Text,
    templateBaseline :: Maybe T.Text
  }
templateSpecs :: [TemplateSpec]
templateSpecs =
  [ TemplateSpec
      { templateName = "haskell_package_baseline",
        templateMatches = \_ nixSource -> pure ("haskellPackages.mkDerivation" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedDifferenceKeys,
        templateBaseline = Just haskellTemplateBaseline
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        templateMatches = \_ nixSource -> pure ("rustPlatform.buildRustPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "passthru" defaultAllowedDifferenceKeys,
        templateBaseline = Just rustTemplateBaseline
      },
    TemplateSpec
      { templateName = "html_template",
        templateMatches = \_ nixSource -> pure ("writeShellScriptBin" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.insert "text" defaultAllowedDifferenceKeys,
        templateBaseline = Just htmlTemplateBaseline
      },
    TemplateSpec
      { templateName = "python_latex_template",
        templateMatches = pythonLaTeXDetector,
        templateAllowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "version"],
        templateBaseline = Just pythonLaTeXTemplateBaseline
      },
    TemplateSpec
      { templateName = "python_pypi_template",
        templateMatches = pythonPyPIDetector,
        templateAllowedDifferenceKeys = Set.fromList ["nativeBuildInputs", "propagatedBuildInputs", "src", "version"],
        templateBaseline = Just pythonPyPIBaseline
      },
    TemplateSpec
      { templateName = "binary_release_template",
        templateMatches = binaryReleaseDetector,
        templateAllowedDifferenceKeys = Set.fromList ["installCheckPhase", "src", "version"],
        templateBaseline = Just binaryReleaseBaseline
      },
    TemplateSpec
      { templateName = "python_template",
        templateMatches = \_ nixSource -> pure ("buildPythonPackage" `isInfixOf` nixSource),
        templateAllowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "shellHook", "version"],
        templateBaseline = Just pythonTemplateBaseline
      },
    TemplateSpec
      { templateName = "deploy_host_template",
        templateMatches = \_ nixSource ->
          pure
            ( "writeShellApplication" `isInfixOf` nixSource
                && ("opentofu" `isInfixOf` nixSource || "agenix-shell" `isInfixOf` nixSource)
            ),
        templateAllowedDifferenceKeys = defaultAllowedDifferenceKeys,
        templateBaseline = Just deployHostTemplateBaseline
      },
    TemplateSpec
      { templateName = "latex_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "latexmk -pdf ms.tex" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = defaultAllowedDifferenceKeys,
        templateBaseline = Just latexTemplateBaseline
      },
    TemplateSpec
      { templateName = "c_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "cc -o ${pname} main.c -std=c89" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = Set.union defaultAllowedDifferenceKeys (Set.fromList ["buildPhase", "checkPhase"]),
        templateBaseline = Just cTemplateBaseline
      },
    TemplateSpec
      { templateName = "uncomment_template",
        templateMatches = \_ nixSource ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` nixSource
                && "autoPatchelfHook" `isInfixOf` nixSource
                && "Goldziher" `isInfixOf` nixSource
            ),
        templateAllowedDifferenceKeys = Set.union defaultAllowedDifferenceKeys (Set.fromList ["pname", "src"]),
        templateBaseline = Just uncommentTemplateBaseline
      }
  ]
pythonLaTeXDetector :: FilePath -> String -> IO Bool
pythonLaTeXDetector packageName nixSource
  | "buildPythonPackage" `isInfixOf` nixSource = do
      let packageDirectory = "packages" </> packageName
      hasManuscriptTexFile <- doesFileExist (packageDirectory </> "ms.tex")
      hasRefsBib <- doesFileExist (packageDirectory </> "refs.bib")
      hasFiguresDir <- doesDirectoryExist (packageDirectory </> "figures")
      pure (hasManuscriptTexFile || hasRefsBib || hasFiguresDir)
  | otherwise = pure False
pythonPyPIDetector :: FilePath -> String -> IO Bool
pythonPyPIDetector _ nixSource =
  pure
    ( "buildPythonPackage" `isInfixOf` nixSource
        && not ("src = ./.;" `isInfixOf` nixSource)
        && ("fetchPypi" `isInfixOf` nixSource || "fetchurl" `isInfixOf` nixSource)
    )
binaryReleaseDetector :: FilePath -> String -> IO Bool
binaryReleaseDetector _ nixSource =
  pure
    ( "stdenv.mkDerivation" `isInfixOf` nixSource
        && "src = pkgs.fetchurl" `isInfixOf` nixSource
        && "sourceRoot = \".\";" `isInfixOf` nixSource
        && "install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname}" `isInfixOf` nixSource
    )
templateSpecByName :: FilePath -> Maybe TemplateSpec
templateSpecByName templateNameToFind = find ((== templateNameToFind) . templateName) templateSpecs
type CheckOutcome :: Type
data CheckOutcome = CheckPassed | CheckFailed | CheckSkipped | CheckIncompatible deriving stock (Eq, Show)
outcomeFromIssues :: [a] -> CheckOutcome
outcomeFromIssues = \case [] -> CheckPassed; _ -> CheckFailed
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
  propertyTestMode <- lookupEnv "PROPERTY_TESTS"
  commandLineArgs <- getArgs
  case debugMode of
    Just "1" ->
      case propertyTestMode of
        Just "1" -> runPropertyTests
        _ -> runDebugTests
    _ -> runCli commandLineArgs
runCli :: [String] -> IO ()
runCli commandLineArgs =
  case commandLineArgs of
    ["check-repository"] -> runInGitRepository "." runCheckMode
    ["check-repository", repositoryDirectory] -> runInGitRepository repositoryDirectory runCheckMode
    ["check-gitmodules"] -> runCheckGitModulesMode
    _ -> do
      putStrLn "Usage: canonicalization check-repository [git-directory]"
      putStrLn "       canonicalization check-gitmodules"
      exitFailure
runInGitRepository :: FilePath -> IO a -> IO a
runInGitRepository repositoryDirectory action = do
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
runCheckMode :: IO ()
runCheckMode = do
  structureIssues <- checkRepositoryStructure
  unless (null structureIssues) $ do
    reportComplianceFailures "directory-structure" structureIssues
    exitFailure
  packageNames <- listPackageNames
  packageChecks <- forM packageNames (checkPackage [])
  let fileIssues = concatMap packageCheckIssues packageChecks
  unless (null fileIssues) $ do
    reportComplianceFailures "file-compliance" fileIssues
    exitFailure
reportComplianceFailures :: String -> [String] -> IO ()
reportComplianceFailures compliancePhase complianceIssues = do
  putStrLn ("check-repository failed at phase: " ++ compliancePhase)
  forM_ complianceIssues $ \issue ->
    putStrLn ("- [" ++ compliancePhase ++ "] " ++ issue)
  case compliancePhase of
    "directory-structure" ->
      putStrLn "hint: fix directory and required-file layout under packages/, hosts/, checks/, and repository root."
    "file-compliance" ->
      putStrLn "hint: align package files with the expected internal templates and language-specific policy checks."
    _ -> pure ()
runCheckGitModulesMode :: IO ()
runCheckGitModulesMode = do
  repositories <- loadHomeGitModuleRepositories
  let invalidPathEntries = [gitModuleRepositoryPathEntry repository | repository <- repositories, not (gitModuleRepositoryCompatible repository)]
  if null invalidPathEntries
    then putStrLn "all .gitmodules path entries comply with go-style naming (<host>/<owner>/<repo>)"
    else do
      forM_ invalidPathEntries $ \invalidPathEntry ->
        putStrLn (invalidPathEntry ++ ": must be exactly <host>/<owner>/<repo>")
      exitFailure
type GitModuleRepository :: Type
data GitModuleRepository = GitModuleRepository
  { gitModuleRepositoryHost :: String,
    gitModuleRepositoryOwner :: String,
    gitModuleRepositoryName :: String,
    gitModuleRepositoryPathEntry :: FilePath,
    gitModuleRepositoryPath :: FilePath,
    gitModuleRepositoryCompatible :: Bool
  }
loadHomeGitModuleRepositories :: IO [GitModuleRepository]
loadHomeGitModuleRepositories = do
  homeDirectory <- getHomeDirectory
  let gitmodulesFilePath = homeDirectory </> ".gitmodules"
  fileExists <- doesFileExist gitmodulesFilePath
  unless fileExists $ do
    putStrLn ("missing file: " ++ gitmodulesFilePath)
    exitFailure
  gitmodulesContents <- T.unpack <$> TIO.readFile gitmodulesFilePath
  let gitModulePathEntries = parseGitModulePaths gitmodulesContents
  pure (map (buildGitModuleRepository homeDirectory) gitModulePathEntries)
buildGitModuleRepository :: FilePath -> FilePath -> GitModuleRepository
buildGitModuleRepository homeDirectory gitModulePathEntry =
  let repositoryPath = homeDirectory </> gitModulePathEntry
      pathSegments = splitDirectories gitModulePathEntry
   in case pathSegments of
        [hostSegment, ownerSegment, repositorySegment] ->
          GitModuleRepository
            { gitModuleRepositoryHost = hostSegment,
              gitModuleRepositoryOwner = ownerSegment,
              gitModuleRepositoryName = repositorySegment,
              gitModuleRepositoryPathEntry = gitModulePathEntry,
              gitModuleRepositoryPath = repositoryPath,
              gitModuleRepositoryCompatible = True
            }
        _ ->
          GitModuleRepository
            { gitModuleRepositoryHost = "",
              gitModuleRepositoryOwner = "",
              gitModuleRepositoryName = takeFileName gitModulePathEntry,
              gitModuleRepositoryPathEntry = gitModulePathEntry,
              gitModuleRepositoryPath = repositoryPath,
              gitModuleRepositoryCompatible = False
            }
parseGitModulePaths :: String -> [FilePath]
parseGitModulePaths gitmodulesContents =
  nub
    [ trimString pathValue
    | gitmodulesLine <- lines gitmodulesContents,
      let trimmedLine = trimString gitmodulesLine,
      "path" `isPrefixOf` trimmedLine,
      "=" `isInfixOf` trimmedLine,
      let pathValue = drop 1 (dropWhile (/= '=') trimmedLine),
      not (null (trimString pathValue))
    ]
checkRepositoryStructure :: IO [String]
checkRepositoryStructure = do
  repositoryPaths <- collectRepositoryPaths "."
  let relativePaths = sort [path | path <- repositoryPaths, path /= "."]
      leafPaths = Set.fromList (filter (isLeafPath relativePaths) relativePaths)
      packageRoots = Set.fromList (mapMaybe packageRoot relativePaths)
      hostRoots = Set.fromList (mapMaybe hostRoot relativePaths)
      packageInfos = map (buildPackageInfo leafPaths) (Set.toList packageRoots)
      globalAllowedPatterns :: [String]
      globalAllowedPatterns =
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
      packageAllowedPatterns =
        concat
          [ allowedPatternsForPackageKind (packageRootPath packageInfo) (packageRootDirectoryName packageInfo) (detectedPackageKind packageInfo)
          | packageInfo <- packageInfos
          ]
      allowedPatterns = globalAllowedPatterns ++ packageAllowedPatterns
      missingPackageDefaults =
        [ packageRootPathValue ++ ": missing required file default.nix"
        | packageRootPathValue <- Set.toList packageRoots,
          (packageRootPathValue </> "default.nix") `notElem` relativePaths
        ]
      missingHostConfigurations =
        [ hostDirectory ++ ": missing required file configuration.nix"
        | hostDirectory <- Set.toList hostRoots,
          (hostDirectory </> "configuration.nix") `notElem` relativePaths
        ]
      missingCabalForMain =
        [ packageRootPathValue ++ ": missing required file " ++ packageDirectoryName ++ ".cabal for Main.hs package"
        | packageRootPathValue <- Set.toList packageRoots,
          Set.member (packageRootPathValue </> "Main.hs") (Set.fromList relativePaths),
          let packageDirectoryName = takeBaseName packageRootPathValue,
          (packageRootPathValue </> packageDirectoryName <.> "cabal") `notElem` relativePaths
        ]
      misnamedCabalFiles =
        [ path ++ ": cabal file must be named " ++ packageDirectoryName ++ ".cabal"
        | path <- relativePaths,
          ".cabal" `isSuffixOf` path,
          let packageRootPathValue = takeDirectory path,
          "packages/" `isPrefixOf` packageRootPathValue,
          let packageDirectoryName = takeBaseName packageRootPathValue,
          takeFileName path /= packageDirectoryName <.> "cabal"
        ]
      packageKindIssues =
        concatMap packageKindIssuesForPackage packageInfos
      disallowedPaths =
        [ path ++ ": is not allowed"
        | path <- Set.toList leafPaths,
          not (any (`pathMatches` path) allowedPatterns)
        ]
  pure (missingPackageDefaults ++ missingHostConfigurations ++ missingCabalForMain ++ misnamedCabalFiles ++ packageKindIssues ++ disallowedPaths)
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
  | UnknownPackage
  deriving stock (Eq, Ord, Show)
type PackageInfo :: Type
data PackageInfo = PackageInfo
  { packageRootPath :: FilePath,
    packageRootDirectoryName :: FilePath,
    packageLeafFiles :: [FilePath],
    detectedPackageKind :: PackageKind,
    matchedPackageMarkers :: [String]
  }
buildPackageInfo :: Set.Set FilePath -> FilePath -> PackageInfo
buildPackageInfo leafPaths packageRootPathValue =
  let packageDirectoryName = takeBaseName packageRootPathValue
      leafFiles =
        catMaybes
          [ stripPrefix (packageRootPathValue ++ "/") path
          | path <- Set.toList leafPaths,
            (packageRootPathValue ++ "/") `isPrefixOf` path
          ]
      markers = detectPackageMarkers leafFiles
   in PackageInfo
        { packageRootPath = packageRootPathValue,
          packageRootDirectoryName = packageDirectoryName,
          packageLeafFiles = leafFiles,
          detectedPackageKind = detectPackageKindFromMarkers markers,
          matchedPackageMarkers = map fst markers
        }
detectPackageMarkers :: [FilePath] -> [(String, PackageKind)]
detectPackageMarkers leafFiles =
  let hasLeafFile leafFile = leafFile `elem` leafFiles
      hasLeafFileWithPrefix pathPrefix = any (isPrefixOf pathPrefix) leafFiles
   in catMaybes
        [ if hasLeafFile "Main.hs" then Just ("Main.hs", HaskellPackage) else Nothing,
          if hasLeafFile "Cargo.toml" then Just ("Cargo.toml", RustPackage) else Nothing,
          if hasLeafFile "index.html" then Just ("index.html", HTMLPackage) else Nothing,
          if hasLeafFile "main.py" && hasLeafFile "ms.tex" then Just ("main.py+ms.tex", PythonLaTeXPackage) else Nothing,
          if hasLeafFile "main.py" && not (hasLeafFile "ms.tex") then Just ("main.py", PythonPackage) else Nothing,
          if hasLeafFile "main.c" then Just ("main.c", CPackage) else Nothing,
          if hasLeafFile "main.tf" then Just ("main.tf", TerraformPackage) else Nothing,
          if hasLeafFile "ms.tex" && not (hasLeafFile "main.py") then Just ("ms.tex", LaTeXPackage) else Nothing,
          if hasLeafFileWithPrefix "Cargo.toml" then Nothing else if not (hasLeafFile "main.c") && not (hasLeafFile "Main.hs") && not (hasLeafFile "main.py") && not (hasLeafFile "index.html") && not (hasLeafFile "main.tf") && not (hasLeafFile "ms.tex") then Just ("binary-layout", BinaryReleasePackage) else Nothing
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
packageKindIssuesForPackage :: PackageInfo -> [String]
packageKindIssuesForPackage packageInfo =
  [ packageRootPath packageInfo
      ++ ": has ambiguous project markers: "
      ++ intercalate ", " (matchedPackageMarkers packageInfo)
  | length (matchedPackageMarkers packageInfo) > 1
  ]
allowedPatternsForPackageKind :: FilePath -> FilePath -> PackageKind -> [String]
allowedPatternsForPackageKind packageRootPathValue packageDirectoryName packageKind =
  let base = ["^" ++ packageRootPathValue ++ "/default\\.nix$", "^" ++ packageRootPathValue ++ "/\\.gitignore$"]
      add patterns = base ++ patterns
   in case packageKind of
        HaskellPackage -> add ["^" ++ packageRootPathValue ++ "/Main\\.hs$", "^" ++ packageRootPathValue ++ "/" ++ packageDirectoryName ++ "\\.cabal$"]
        RustPackage -> add ["^" ++ packageRootPathValue ++ "/Cargo\\.toml$", "^" ++ packageRootPathValue ++ "/Cargo\\.lock$", "^" ++ packageRootPathValue ++ "/src/main\\.rs$"]
        HTMLPackage -> add ["^" ++ packageRootPathValue ++ "/index\\.html$", "^" ++ packageRootPathValue ++ "/script\\.js$", "^" ++ packageRootPathValue ++ "/style\\.css$"]
        PythonLaTeXPackage -> add ["^" ++ packageRootPathValue ++ "/main\\.py$", "^" ++ packageRootPathValue ++ "/ms\\.tex$", "^" ++ packageRootPathValue ++ "/ms\\.bib$", "^" ++ packageRootPathValue ++ "/refs\\.bib$", "^" ++ packageRootPathValue ++ "/figures(/.*)?$"]
        PythonPackage -> add ["^" ++ packageRootPathValue ++ "/main\\.py$"]
        PythonPyPIPackage -> base
        CPackage -> add ["^" ++ packageRootPathValue ++ "/main\\.c$"]
        TerraformPackage -> add ["^" ++ packageRootPathValue ++ "/main\\.tf$", "^" ++ packageRootPathValue ++ "/\\.terraform(/.*)?$", "^" ++ packageRootPathValue ++ "/\\.terraform\\.lock\\.hcl$"]
        LaTeXPackage -> add ["^" ++ packageRootPathValue ++ "/ms\\.tex$", "^" ++ packageRootPathValue ++ "/ms\\.bib$"]
        BinaryReleasePackage -> base
        UnknownPackage -> base
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
pathMatches :: String -> FilePath -> Bool
pathMatches regexPattern path = path =~ regexPattern
packageRoot :: FilePath -> Maybe FilePath
packageRoot repositoryPath =
  case splitDirectories repositoryPath of
    "packages" : packageName : _ -> Just ("packages" </> packageName)
    _ -> Nothing
hostRoot :: FilePath -> Maybe FilePath
hostRoot repositoryPath =
  case splitDirectories repositoryPath of
    "hosts" : hostDirectoryName : _ -> Just ("hosts" </> hostDirectoryName)
    _ -> Nothing
listPackageNames :: IO [FilePath]
listPackageNames = listSubdirectories "packages"
listSubdirectories :: FilePath -> IO [FilePath]
listSubdirectories parentDirectory = do
  parentDirectoryExists <- doesDirectoryExist parentDirectory
  if not parentDirectoryExists
    then pure []
    else do
      childNames <- listDirectory parentDirectory
      childIsDirectoryFlags <- forM childNames $ \childName -> doesDirectoryExist (parentDirectory </> childName)
      pure $ sort [childName | (childName, isDirectory) <- zip childNames childIsDirectoryFlags, isDirectory]
checkPackage :: [String] -> FilePath -> IO PackageCheck
checkPackage repositoryStructureIssues packageName = do
  let packageDefaultPath = "packages" </> packageName </> "default.nix"
      scopedStructureIssues =
        [ issue
        | issue <- repositoryStructureIssues,
          ("packages/" ++ packageName) `isPrefixOf` issue
        ]
  packageKind <- detectPackageKindForPackage packageName
  packageDefaultExists <- doesFileExist packageDefaultPath
  (templateIssues, _) <-
    if not packageDefaultExists
      then pure ([], Nothing)
      else do
        packageDefaultNixSource <- TIO.readFile packageDefaultPath
        inferredTemplate <- inferTemplateName packageName (T.unpack packageDefaultNixSource)
        case inferredTemplate of
          Nothing ->
            pure
              ( [ "packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"
                ],
                Nothing
              )
          Just inferredTemplateName -> do
            case templateSpecByName inferredTemplateName of
              Nothing ->
                pure
                  ( [ "packages/" ++ packageName ++ "/default.nix: unsupported template " ++ inferredTemplateName
                    ],
                    Just inferredTemplateName
                  )
              Just templateSpec ->
                case templateBaseline templateSpec of
                  Just templateNixSource ->
                    do
                      let allowedKeysForPackage =
                            if packageName == "c_template" && inferredTemplateName == "c_template"
                              then defaultAllowedDifferenceKeys
                              else templateAllowedDifferenceKeys templateSpec
                      templateComparisonIssues <- compareWithTemplate packageName packageDefaultPath ("packages" </> inferredTemplateName </> "default.nix") allowedKeysForPackage (Just templateNixSource)
                      pure (templateComparisonIssues, Just inferredTemplateName)
                  Nothing ->
                    pure
                      ( [ "packages/"
                            ++ packageName
                            ++ "/default.nix: internal error: missing embedded template baseline for "
                            ++ inferredTemplateName
                        ],
                        Just inferredTemplateName
                      )
  cargoIssues <- checkCargoToml packageName
  cabalIssues <- checkCabalFile packageName
  pythonDebugIssues <- checkPythonDebugUnitTest packageName packageKind
  haskellDebugIssues <- checkHaskellDebugTests packageName packageKind
  rustDebugIssues <- checkRustDebugTests packageName packageKind
  pythonUnitTestNames <- discoverPythonUnitTestNames packageName packageKind
  haskellUnitTestNames <- discoverHaskellUnitTestNames packageName packageKind
  rustUnitTestNames <- discoverRustUnitTestNames packageName packageKind
  let cargoOutcome = outcomeFromIssues cargoIssues
      cabalOutcome = outcomeFromIssues cabalIssues
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
        [issue | issue <- scopedStructureIssues, "/default.nix" `isInfixOf` issue]
          ++ templateIssues
      defaultNixOutcome =
        if packageDefaultExists && null defaultNixIssues
          then CheckPassed
          else CheckFailed
      pythonOutcome = if packageKind `elem` [PythonPackage, PythonLaTeXPackage] then outcomeFromIssues pythonDebugIssues else CheckSkipped
      haskellOutcome = if packageKind == HaskellPackage then outcomeFromIssues haskellDebugIssues else CheckSkipped
      rustOutcome = if packageKind == RustPackage then outcomeFromIssues rustDebugIssues else CheckSkipped
      basePackageTests =
        [ PackageTest
            "directory structure"
            (outcomeFromIssues scopedStructureIssues)
            [],
          makePackageTest "default.nix" defaultNixOutcome "matches template and policy" defaultNixIssues
        ]
      languageSpecificPackageTests =
        concat
          [ if packageKind == RustPackage
              then
                [ makePackageTest "Cargo.toml" cargoOutcome "matches Cargo.toml conventions" cargoIssues,
                  PackageTest
                    "src/main.rs"
                    rustOutcome
                    ( makePackageTestCase
                        "supports DEBUG test execution"
                        rustOutcome
                        rustDebugIssues
                        : [PackageTestCase rustUnitTestName CheckSkipped [] | rustUnitTestName <- rustUnitTestNames]
                    )
                ]
              else [],
            if packageKind == HaskellPackage
              then
                [ makePackageTest (packageName ++ ".cabal") cabalOutcome "matches Cabal conventions" cabalIssues,
                  PackageTest
                    "Main.hs"
                    haskellOutcome
                    ( makePackageTestCase
                        "supports DEBUG test execution"
                        haskellOutcome
                        haskellDebugIssues
                        : [ PackageTestCase
                              discoveredHaskellTestName
                              CheckSkipped
                              []
                          | discoveredHaskellTestName <- if null haskellUnitTestNames then ["No named HUnit test labels discovered"] else haskellUnitTestNames
                          ]
                    )
                ]
              else [],
            [ PackageTest
                "main.py"
                pythonOutcome
                ( makePackageTestCase
                    "supports DEBUG test execution"
                    pythonOutcome
                    pythonDebugIssues
                    : [PackageTestCase pythonUnitTestName CheckSkipped [] | pythonUnitTestName <- pythonUnitTestNames]
                )
            | packageKind `elem` [PythonPackage, PythonLaTeXPackage]
            ]
          ]
      packageTests = basePackageTests ++ languageSpecificPackageTests
      _forcePackageCheckSelectorUse =
        [ ( packageTestName packageTest,
            packageTestOutcome packageTest,
            [ (packageTestCaseNameValue, packageTestCaseOutcomeValue, packageTestCaseIssuesValue)
            | packageTestCase <- packageTestCases packageTest,
              let packageTestCaseNameValue = packageTestCaseName packageTestCase,
              let packageTestCaseOutcomeValue = packageTestCaseOutcome packageTestCase,
              let packageTestCaseIssuesValue = packageTestCaseIssues packageTestCase
            ]
          )
        | packageTest <- packageTests
        ]
      packageIssues =
        scopedStructureIssues
          ++ templateIssues
          ++ cargoIssues
          ++ cabalIssues
          ++ pythonDebugIssues
          ++ haskellDebugIssues
          ++ rustDebugIssues
  pure
    PackageCheck
      { packageCheckName = packageName,
        packageCheckKind = packageKind,
        packageCheckTests = packageTests,
        packageCheckIssues = packageIssues
      }
detectPackageKindForPackage :: FilePath -> IO PackageKind
detectPackageKindForPackage packageName = do
  let packageRootPathValue = "packages" </> packageName
      packageFileExists relativePath = doesFileExist (packageRootPathValue </> relativePath)
  hasMainHaskellFile <- packageFileExists "Main.hs"
  hasCargoTomlFile <- packageFileExists "Cargo.toml"
  hasIndexHtmlFile <- packageFileExists "index.html"
  hasMainPythonFile <- packageFileExists "main.py"
  hasManuscriptTexFile <- packageFileExists "ms.tex"
  hasMainCFile <- packageFileExists "main.c"
  hasMainTerraformFile <- packageFileExists "main.tf"
  defaultNixSource <- readTextFileIfExists (packageRootPathValue </> "default.nix")
  let isPythonPyPIPackage =
        case defaultNixSource of
          Nothing -> False
          Just defaultNixText ->
            let nixSource = T.unpack defaultNixText
             in "buildPythonPackage" `isInfixOf` nixSource
                  && not ("src = ./.;" `isInfixOf` nixSource)
                  && ("fetchPypi" `isInfixOf` nixSource || "fetchurl" `isInfixOf` nixSource)
  let packageKind
        | hasMainHaskellFile = HaskellPackage
        | hasCargoTomlFile = RustPackage
        | hasIndexHtmlFile = HTMLPackage
        | hasMainPythonFile && hasManuscriptTexFile = PythonLaTeXPackage
        | hasMainPythonFile = PythonPackage
        | isPythonPyPIPackage = PythonPyPIPackage
        | hasMainCFile = CPackage
        | hasMainTerraformFile = TerraformPackage
        | hasManuscriptTexFile = LaTeXPackage
        | otherwise = BinaryReleasePackage
  pure packageKind
readTextFileIfExists :: FilePath -> IO (Maybe T.Text)
readTextFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then Just <$> TIO.readFile filePath else pure Nothing
checkPythonDebugUnitTest :: FilePath -> PackageKind -> IO [String]
checkPythonDebugUnitTest packageName packageKind =
  if packageKind `notElem` [PythonPackage, PythonLaTeXPackage]
    then pure []
    else do
      let mainPythonPath = "packages" </> packageName </> "main.py"
      maybeMainPythonSource <- readTextFileIfExists mainPythonPath
      case maybeMainPythonSource of
        Nothing -> pure []
        Just _ -> do
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
              (exitCode, stdoutText, stderrText) <- readProcessWithExitCode pythonCommand ["-c", pythonDebugUnitTestValidator, mainPythonPath] ""
              let validatorOutputLines = lines stdoutText
                  errorCodes = [drop 4 validatorOutputLine | validatorOutputLine <- validatorOutputLines, "ERR " `isPrefixOf` validatorOutputLine]
                  mappedErrors = map (mapPythonValidatorError packageName) errorCodes
              case exitCode of
                ExitSuccess ->
                  if "OK" `elem` validatorOutputLines
                    then pure []
                    else
                      pure
                        [ "packages/" ++ packageName ++ "/main.py: python AST validator produced unexpected output"
                        ]
                ExitFailure 1 -> pure mappedErrors
                ExitFailure _ ->
                  pure
                    [ "packages/"
                        ++ packageName
                        ++ "/main.py: python AST validator execution failed: "
                        ++ oneLine (T.pack stderrText)
                    ]
discoverPythonUnitTestNames :: FilePath -> PackageKind -> IO [String]
discoverPythonUnitTestNames packageName packageKind =
  if packageKind `notElem` [PythonPackage, PythonLaTeXPackage]
    then pure []
    else do
      let mainPythonPath = "packages" </> packageName </> "main.py"
      maybeMainPythonSourceText <- readTextFileIfExists mainPythonPath
      case maybeMainPythonSourceText of
        Nothing -> pure []
        Just mainPythonSourceText -> do
          let extracted =
                [ functionName
                | sourceLine <- lines (T.unpack mainPythonSourceText),
                  Just functionName <- [extractPythonTestName sourceLine]
                ]
          pure (sort (Set.toList (Set.fromList extracted)))
extractPythonTestName :: String -> Maybe String
extractPythonTestName sourceLine =
  let trimmed = dropWhile (== ' ') sourceLine
      defPrefix :: String
      defPrefix = "def test_"
   in if defPrefix `isPrefixOf` trimmed
        then
          let namePortion = takeWhile (\ch -> ch /= '(' && ch /= ' ' && ch /= ':') (drop 4 trimmed)
           in if null namePortion then Nothing else Just namePortion
        else Nothing
checkHaskellDebugTests :: FilePath -> PackageKind -> IO [String]
checkHaskellDebugTests packageName packageKind =
  if packageKind /= HaskellPackage
    then pure []
    else do
      let mainHaskellPath = "packages" </> packageName </> "Main.hs"
      mainFileExists <- doesFileExist mainHaskellPath
      if not mainFileExists
        then pure []
        else do
          mainHaskellSourceText <- TIO.readFile mainHaskellPath
          let haskellSource = T.unpack mainHaskellSourceText
              hasDebugEnvGate = "lookupEnv \"DEBUG\"" `isInfixOf` haskellSource
              hasTestRunner = "runTestTT" `isInfixOf` haskellSource || "runDebugTests" `isInfixOf` haskellSource
              hasMainDebugBranch = "Just \"1\" ->" `isInfixOf` haskellSource
          pure $
            catMaybes
              [ if hasDebugEnvGate
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: missing DEBUG environment check (lookupEnv \"DEBUG\")"),
                if hasMainDebugBranch
                  then Nothing
                  else Just ("packages/" ++ packageName ++ "/Main.hs: main() must branch on DEBUG=1"),
                if hasTestRunner
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
              labelsFromFormattingHelper = extractMakeFormattingTestLabels haskellSourceLines
              labelsFromTilde =
                [ label
                | sourceLine <- haskellSourceLines,
                  Just label <- [extractHaskellTestLabel sourceLine]
                ]
              labelsFromAssertEqual = extractAssertEqualLabels haskellSourceLines
              fallbackCaseNames =
                [ "Unnamed HUnit test case #" ++ show i
                | i <- [1 .. length [() | sourceLine <- haskellSourceLines, "TestCase" `isInfixOf` sourceLine]]
                ]
              discovered =
                if null labelsFromFormattingHelper && null labelsFromTilde && null labelsFromAssertEqual
                  then fallbackCaseNames
                  else labelsFromFormattingHelper ++ labelsFromTilde ++ labelsFromAssertEqual
          pure (sort (Set.toList (Set.fromList discovered)))
extractHaskellTestLabel :: String -> Maybe String
extractHaskellTestLabel sourceLine =
  case breakOnSubstring "~:" sourceLine of
    Nothing -> Nothing
    Just (beforeTilde, _) -> lastQuotedToken beforeTilde
breakOnSubstring :: String -> String -> Maybe (String, String)
breakOnSubstring needle = go ""
  where
    go _prefix [] = Nothing
    go prefix rest
      | needle `isPrefixOf` rest = Just (prefix, drop (length needle) rest)
      | otherwise =
          case rest of
            ch : tailRest -> go (prefix ++ [ch]) tailRest
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
extractAssertEqualLabels :: [String] -> [String]
extractAssertEqualLabels = go False
  where
    go _ [] = []
    go awaitingLabel (line : rest)
      | "assertEqual" `isInfixOf` line = go True rest
      | awaitingLabel =
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
    go awaitingLabel (line : rest)
      | "makeFormattingTest" `isInfixOf` line = go True rest
      | awaitingLabel =
          let trimmed = dropWhile (== ' ') line
           in if null trimmed
                then go True rest
                else case firstQuotedToken line of
                  Just label -> label : go False rest
                  Nothing -> go False rest
      | otherwise = go False rest
firstQuotedToken :: String -> Maybe String
firstQuotedToken inputText =
  let afterDouble = dropWhile (/= '"') inputText
      afterSingle = dropWhile (/= '\'') inputText
   in case afterDouble of
        '"' : rest ->
          let token = takeWhile (/= '"') rest
           in if null token then Nothing else Just token
        _ ->
          case afterSingle of
            '\'' : rest ->
              let token = takeWhile (/= '\'') rest
               in if null token then Nothing else Just token
            _ -> Nothing
checkRustDebugTests :: FilePath -> PackageKind -> IO [String]
checkRustDebugTests packageName packageKind =
  if packageKind /= RustPackage
    then pure []
    else do
      let mainRustPath = "packages" </> packageName </> "src/main.rs"
          defaultNixPath = "packages" </> packageName </> "default.nix"
      mainFileExists <- doesFileExist mainRustPath
      defaultNixFileExists <- doesFileExist defaultNixPath
      mainSource <-
        if mainFileExists
          then T.unpack <$> TIO.readFile mainRustPath
          else pure ""
      defaultNixSource <-
        if defaultNixFileExists
          then T.unpack <$> TIO.readFile defaultNixPath
          else pure ""
      let hasRustTestModule = "#[cfg(test)]" `isInfixOf` mainSource && "mod tests" `isInfixOf` mainSource
          hasRustTestCases = "#[test]" `isInfixOf` mainSource
          hasDebugGateInNix = "DEBUG" `isInfixOf` defaultNixSource && "cargo test" `isInfixOf` defaultNixSource
      pure $
        catMaybes
          [ if hasRustTestModule
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[cfg(test)] mod tests"),
            if hasRustTestCases
              then Nothing
              else Just ("packages/" ++ packageName ++ "/src/main.rs: missing #[test] test cases"),
            if hasDebugGateInNix
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
        Just mainRustSourceText -> pure (extractRustTests (lines (T.unpack mainRustSourceText)))
extractRustTests :: [String] -> [String]
extractRustTests sourceLines = sort (Set.toList (Set.fromList (go False sourceLines)))
  where
    go _ [] = []
    go awaitingFunctionAfterTestAttribute (line : rest) =
      let trimmed = dropWhile (== ' ') line
       in if "#[test]" `isPrefixOf` trimmed
            then go True rest
            else
              if awaitingFunctionAfterTestAttribute && "fn " `isPrefixOf` trimmed
                then
                  let functionName = takeWhile (\ch -> ch /= '(' && ch /= ' ') (drop 3 trimmed)
                   in [functionName | not (null functionName)] ++ go False rest
                else go False rest
mapPythonValidatorError :: FilePath -> String -> String
mapPythonValidatorError packageName errorCode =
  let messagePrefix = "packages/" ++ packageName ++ "/main.py: "
   in case errorCode of
        "missing_main_function" -> messagePrefix ++ "missing main() function"
        "missing_debug_gate" -> messagePrefix ++ "main() must include a DEBUG gate"
        "debug_branch_no_unittest" -> messagePrefix ++ "DEBUG branch in main() must run unittest"
        "run_tests_missing_unittest" -> messagePrefix ++ "run_tests() is called from DEBUG branch but does not run unittest"
        "parse_error" -> messagePrefix ++ "python source could not be parsed"
        _ -> messagePrefix ++ "python validator failed with error code: " ++ errorCode
pythonDebugUnitTestValidator :: String
pythonDebugUnitTestValidator =
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
      "        base = function.value",
      "        if isinstance(base.value, ast.Name) and base.value.id == 'os' and base.attr == 'environ':",
      "            if not node.args:",
      "                return False",
      "            first = node.args[0]",
      "            return isinstance(first, ast.Constant) and first.value == 'DEBUG'",
      "    return False",
      "",
      "def _contains_debug_gate(expression):",
      "    return any(_is_os_getenv_debug(node) for node in ast.walk(expression))",
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
  cargoTomlExists <- doesFileExist cargoTomlPath
  if not cargoTomlExists
    then pure []
    else do
      cargoTomlContents <- TIO.readFile cargoTomlPath
      let packageSection = extractTomlSection "package" cargoTomlContents
          lintsRustSection = extractTomlSection "lints.rust" cargoTomlContents
          packageNameValue = lookupTomlString "name" packageSection
          unsafeCodeLint = lookupTomlString "unsafe_code" lintsRustSection
          normalizedCargoToml = normalizeCargoTomlForTightness packageName cargoTomlContents
          normalizedTemplateCargoToml = normalizeCargoTomlForTightness packageName rustCargoTightnessBaseline
      pure $
        catMaybes
          [ case packageNameValue of
              Nothing ->
                Just ("packages/" ++ packageName ++ "/Cargo.toml: missing [package].name")
              Just actualName ->
                if actualName == T.pack packageName
                  then Nothing
                  else
                    Just
                      ( "packages/"
                          ++ packageName
                          ++ "/Cargo.toml: [package].name must match directory name (expected \""
                          ++ packageName
                          ++ "\", got \""
                          ++ T.unpack actualName
                          ++ "\")"
                      ),
            if unsafeCodeLint == Just "forbid"
              then Nothing
              else Just ("packages/" ++ packageName ++ "/Cargo.toml: [lints.rust].unsafe_code must be \"forbid\""),
            if normalizedCargoToml == normalizedTemplateCargoToml
              then Nothing
              else
                Just
                  ( "packages/"
                      ++ packageName
                      ++ "/Cargo.toml: only dependency sections may differ from the internal Rust Cargo baseline"
                  )
          ]
normalizeCargoTomlForTightness :: FilePath -> T.Text -> T.Text
normalizeCargoTomlForTightness packageName tomlContents =
  let step (currentTomlHeader, normalizedLinesSoFar) sourceLine =
        let trimmedLine = T.strip sourceLine
         in if isTomlSectionHeader trimmedLine
              then
                if isDependencyHeader trimmedLine
                  then (Just trimmedLine, normalizedLinesSoFar)
                  else (Just trimmedLine, normalizedLinesSoFar ++ [trimmedLine])
              else case currentTomlHeader of
                Just header | isDependencyHeader header -> (currentTomlHeader, normalizedLinesSoFar)
                _ | T.null trimmedLine -> (currentTomlHeader, normalizedLinesSoFar)
                Just "[package]" | isNameLine trimmedLine -> (currentTomlHeader, normalizedLinesSoFar ++ [normalizedNameLine])
                Just "[[bin]]" | isNameLine trimmedLine -> (currentTomlHeader, normalizedLinesSoFar ++ [normalizedNameLine])
                _ -> (currentTomlHeader, normalizedLinesSoFar ++ [trimmedLine])
      (_, normalizedLines) = foldl' step (Nothing, []) (T.lines tomlContents)
      normalizedNameLine = "name = \"" <> T.pack packageName <> "\""
   in T.unlines normalizedLines
isDependencyHeader :: T.Text -> Bool
isDependencyHeader trimmedLine =
  trimmedLine == "[dependencies]"
    || trimmedLine == "[dev-dependencies]"
    || trimmedLine == "[build-dependencies]"
    || isTargetDependenciesHeader trimmedLine
isTargetDependenciesHeader :: T.Text -> Bool
isTargetDependenciesHeader trimmedLine =
  T.isPrefixOf "[target." trimmedLine
    && T.isSuffixOf ".dependencies]" trimmedLine
isNameLine :: T.Text -> Bool
isNameLine trimmedLine = "name = \"" `T.isPrefixOf` trimmedLine
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
      matchingFieldLine = listToMaybe [T.strip sectionLine | sectionLine <- T.lines sectionContents, keyPrefix `T.isPrefixOf` T.strip sectionLine]
   in do
        matchingLine <- matchingFieldLine
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
      let normalizedCabal = normalizeCabalForTightness packageName cabalContents
          normalizedTemplateCabal = normalizeCabalForTightness packageName haskellCabalTightnessBaseline
          cabalPackageName = lookupCabalField "name" cabalContents
      pure $
        catMaybes
          [ if cabalPackageName == Just (T.pack packageName)
              then Nothing
              else Just ("packages/" ++ packageName ++ "/" ++ packageName ++ ".cabal: name must match directory name"),
            if normalizedCabal == normalizedTemplateCabal
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
normalizeCabalForTightness :: FilePath -> T.Text -> T.Text
normalizeCabalForTightness packageName cabalContents =
  let step (insideBuildDependsSection, normalizedLinesSoFar) sourceLine =
        let trimmedLine = T.strip sourceLine
            normalizedLine = normalizeCabalLine packageName trimmedLine
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
normalizeCabalLine :: FilePath -> T.Text -> T.Text
normalizeCabalLine packageName trimmedLine
  | "name:" `T.isPrefixOf` trimmedLine = "name:          " <> T.pack packageName
  | "executable " `T.isPrefixOf` trimmedLine = "executable " <> T.pack packageName
  | otherwise = trimmedLine
lookupCabalField :: T.Text -> T.Text -> Maybe T.Text
lookupCabalField cabalField cabalContents =
  let fieldPrefix = cabalField <> ":"
      matchingFieldLine = listToMaybe [T.strip cabalLine | cabalLine <- T.lines cabalContents, fieldPrefix `T.isPrefixOf` T.strip cabalLine]
   in do
        matchingLine <- matchingFieldLine
        fieldValue <- T.stripPrefix fieldPrefix matchingLine
        pure (T.strip fieldValue)
compareWithTemplate :: FilePath -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
compareWithTemplate packageName packageDefaultPath templateDefaultPath allowedDifferenceKeys templateOverrideNixSource = do
  parsedPackageNixExpr <- parseNixExprFromFile packageDefaultPath
  parsedTemplateNixExpr <-
    case templateOverrideNixSource of
      Just overrideNixSource -> parseNixExprFromText overrideNixSource
      Nothing -> parseNixExprFromFile templateDefaultPath
  case (parsedPackageNixExpr, parsedTemplateNixExpr) of
    (Left parseError, _) ->
      pure ["packages/" ++ packageName ++ "/default.nix: parse error: " ++ show parseError]
    (_, Left parseError) ->
      pure [templateDefaultPath ++ ": parse error: " ++ show parseError]
    (Right packageNixExpr, Right templateNixExpr) ->
      let normalizedPackageNixExpr = normalizeNixExpr allowedDifferenceKeys packageNixExpr
          normalizedTemplateNixExpr = normalizeNixExpr allowedDifferenceKeys templateNixExpr
       in pure $
            formatDifferences
              packageName
              templateDefaultPath
              normalizedPackageNixExpr
              normalizedTemplateNixExpr
parseNixExprFromText :: T.Text -> IO (Either String NExprLoc)
parseNixExprFromText nixSource = do
  (temporaryPath, temporaryHandle) <- openTempFile "/tmp" "check-repository-template-override.nix"
  TIO.hPutStr temporaryHandle nixSource
  hClose temporaryHandle
  parseNixExprFromFile temporaryPath
    `finally` removeFileIfExists temporaryPath
parseNixExprFromFile :: FilePath -> IO (Either String NExprLoc)
parseNixExprFromFile nixFilePath =
  fmap (either (Left . show) Right) (parseNixFileLoc (Path nixFilePath))
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  when fileExists (removeFile filePath)
inferTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferTemplateName packageName nixSource = do
  matches <- forM templateSpecs $ \templateSpec -> do
    matched <- templateMatches templateSpec packageName nixSource
    pure (if matched then Just (templateName templateSpec) else Nothing)
  pure (listToMaybe (catMaybes matches))
normalizeNixExpr :: Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeNixExpr allowedDifferenceKeys (Fix (Compose (AnnUnit nixExprSpan expressionFunctor))) =
  let rebuiltNixExpr = case expressionFunctor of
        NSet isRecursive bindings -> NSet isRecursive (normalizeBindings allowedDifferenceKeys bindings)
        NLet bindings body -> NLet (normalizeBindings allowedDifferenceKeys bindings) (normalizeNixExpr allowedDifferenceKeys body)
        NAbs (ParamSet paramsEllipsis paramsAt params) body -> NAbs (ParamSet paramsEllipsis paramsAt (sortParams params)) (normalizeNixExpr allowedDifferenceKeys body)
        NAbs (Param paramName) body -> NAbs (Param paramName) (normalizeNixExpr allowedDifferenceKeys body)
        otherNixExpr -> fmap (normalizeNixExpr allowedDifferenceKeys) otherNixExpr
   in Fix (Compose (AnnUnit nixExprSpan rebuiltNixExpr))
sortParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortParams = sortBy (\(VarName leftName, _) (VarName rightName, _) -> compare leftName rightName)
normalizeBindings :: Set.Set T.Text -> [Binding NExprLoc] -> [Binding NExprLoc]
normalizeBindings allowedDifferenceKeys bindings =
  [normalizeBinding allowedDifferenceKeys binding | binding <- bindings, not (isAllowedDifferenceBinding allowedDifferenceKeys binding)]
normalizeBinding :: Set.Set T.Text -> Binding NExprLoc -> Binding NExprLoc
normalizeBinding allowedDifferenceKeys = \case
  NamedVar keyPath bindingValue sourcePosition -> NamedVar keyPath (normalizeNixExpr allowedDifferenceKeys bindingValue) sourcePosition
  Inherit maybeBoundExpr names sourcePosition -> Inherit (normalizeNixExpr allowedDifferenceKeys <$> maybeBoundExpr) names sourcePosition
isAllowedDifferenceBinding :: Set.Set T.Text -> Binding NExprLoc -> Bool
isAllowedDifferenceBinding allowedDifferenceKeys = \case
  NamedVar (bindingKey :| _) _ _ ->
    case keyNameText bindingKey of
      Just keyText -> Set.member keyText allowedDifferenceKeys
      Nothing -> False
  _ -> False
keyNameText :: NKeyName NExprLoc -> Maybe T.Text
keyNameText = \case
  StaticKey (VarName keyText) -> Just keyText
  DynamicKey (Plain (DoubleQuoted [Plain keyText])) -> Just keyText
  _ -> Nothing
renderNixExpr :: NExprLoc -> T.Text
renderNixExpr =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyNix
    . stripAnnotation
formatDifferences :: FilePath -> FilePath -> NExprLoc -> NExprLoc -> [String]
formatDifferences packageName templateDefaultPath packageNixExpr templateNixExpr =
  let renderedPackage = renderNixExpr packageNixExpr
      renderedTemplate = renderNixExpr templateNixExpr
   in if renderedPackage == renderedTemplate
        then []
        else
          let packageBindingsMaybe = extractPrimaryBindings packageNixExpr
              templateBindingsMaybe = extractPrimaryBindings templateNixExpr
           in case (packageBindingsMaybe, templateBindingsMaybe) of
                (Just packageBindingMap, Just templateBindingMap) ->
                  let missingBindingKeys = Map.keys (Map.difference templateBindingMap packageBindingMap)
                      unexpectedBindingKeys = Map.keys (Map.difference packageBindingMap templateBindingMap)
                      sharedBindingKeys = Map.keys (Map.intersection packageBindingMap templateBindingMap)
                      changedBindingKeys =
                        [ bindingKey
                        | bindingKey <- sharedBindingKeys,
                          Map.lookup bindingKey packageBindingMap /= Map.lookup bindingKey templateBindingMap
                        ]
                      detailLines =
                        map (issueLine "missing key") missingBindingKeys
                          ++ map (issueLine "unexpected key") unexpectedBindingKeys
                          ++ map
                            ( \bindingKey ->
                                let expectedValue = oneLine (fromMaybe "" (Map.lookup bindingKey templateBindingMap))
                                    actualValue = oneLine (fromMaybe "" (Map.lookup bindingKey packageBindingMap))
                                 in "  - changed key: "
                                      ++ T.unpack bindingKey
                                      ++ "\n    expected: "
                                      ++ expectedValue
                                      ++ "\n    actual:   "
                                      ++ actualValue
                            )
                            changedBindingKeys
                   in if null detailLines
                        then
                          [ "packages/"
                              ++ packageName
                              ++ "/default.nix: differs from template "
                              ++ templateDefaultPath
                              ++ " (excluding dependency keys)"
                          ]
                        else
                          [ "packages/"
                              ++ packageName
                              ++ "/default.nix: differs from template "
                              ++ templateDefaultPath
                              ++ " (excluding dependency keys)\n"
                              ++ intercalate "\n" detailLines
                          ]
                _ ->
                  [ "packages/"
                      ++ packageName
                      ++ "/default.nix: differs from template "
                      ++ templateDefaultPath
                      ++ " (excluding dependency keys)"
                  ]
issueLine :: String -> T.Text -> String
issueLine issue bindingKey = "  - " ++ issue ++ ": " ++ T.unpack bindingKey
oneLine :: T.Text -> String
oneLine textValue =
  let compactText = T.unwords (T.words textValue)
   in T.unpack compactText
extractPrimaryBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractPrimaryBindings nixExpression = do
  bindingGroups <- collectSetBindings nixExpression
  pure $ Map.fromList (maximumByLength bindingGroups)
maximumByLength :: [[a]] -> [a]
maximumByLength = maximumBy (comparing length)
collectSetBindings :: NExprLoc -> Maybe [[(T.Text, T.Text)]]
collectSetBindings (Fix (Compose (AnnUnit _ expressionFunctor))) =
  case expressionFunctor of
    NSet _ bindings ->
      let currentBindings = extractBindings bindings
          nestedBindings = concatMap collectFromBinding bindings
       in Just (currentBindings : nestedBindings)
    NLet bindings body ->
      let nestedFromBindings = concatMap collectFromBinding bindings
          nestedFromBody = fromMaybe [] (collectSetBindings body)
       in Just (nestedFromBindings ++ nestedFromBody)
    NAbs _ body -> collectSetBindings body
    otherNixExpr ->
      Just (concatMap (fromMaybe [] . collectSetBindings) otherNixExpr)
collectFromBinding :: Binding NExprLoc -> [[(T.Text, T.Text)]]
collectFromBinding (NamedVar _ bindingValue _) = fromMaybe [] (collectSetBindings bindingValue)
collectFromBinding (Inherit maybeBoundExpr _ _) = maybe [] (fromMaybe [] . collectSetBindings) maybeBoundExpr
extractBindings :: [Binding NExprLoc] -> [(T.Text, T.Text)]
extractBindings bindings =
  [ (T.intercalate "." (mapMaybe keyNameText (NE.toList keyPath)), renderNixExpr bindingValue)
  | NamedVar keyPath bindingValue _ <- bindings
  ]
runDebugTests :: IO ()
runDebugTests = do
  hunitCounts <- runTestTT debugTests
  propertySuccess <- quickCheckDebugProperties
  if errors hunitCounts == 0 && failures hunitCounts == 0 && propertySuccess
    then putStrLn "test ... ok"
    else exitFailure
runPropertyTests :: IO ()
runPropertyTests = do
  propertySuccess <- quickCheckDebugProperties
  if propertySuccess
    then putStrLn "property tests ... ok"
    else exitFailure
quickCheckDebugProperties :: IO Bool
quickCheckDebugProperties = do
  trimResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_trimStringIdempotent)
  gitModuleParseResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_parseGitModulePathsPreservesFirstOccurrences)
  gitModuleRepositoryAcceptanceResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_buildGitModuleRepositoryAcceptsGoStylePaths)
  gitModuleRepositoryRejectionResult <- QC.quickCheckResult (QC.withMaxSuccess 100 prop_buildGitModuleRepositoryRejectsMalformedPaths)
  pure (all isQuickCheckSuccess [trimResult, gitModuleParseResult, gitModuleRepositoryAcceptanceResult, gitModuleRepositoryRejectionResult])
isQuickCheckSuccess :: QC.Result -> Bool
isQuickCheckSuccess QC.Success {} = True
isQuickCheckSuccess _ = False
prop_trimStringIdempotent :: String -> Bool
prop_trimStringIdempotent inputText =
  trimString (trimString inputText) == trimString inputText
prop_parseGitModulePathsPreservesFirstOccurrences :: QC.Property
prop_parseGitModulePathsPreservesFirstOccurrences =
  QC.forAll gitModulePathEntriesGen $ \gitModulePathEntries ->
    let rendered =
          concatMap
            ( \gitModulePathEntry ->
                "[submodule \"example\"]\n"
                  ++ "  path =  "
                  ++ gitModulePathEntry
                  ++ "  \n"
                  ++ "  url = https://example.test/repo.git\n"
            )
            gitModulePathEntries
            ++ "ignore = this-line\n"
     in parseGitModulePaths rendered == nub gitModulePathEntries
prop_buildGitModuleRepositoryAcceptsGoStylePaths :: QC.Property
prop_buildGitModuleRepositoryAcceptsGoStylePaths =
  QC.forAll goStylePathGen $ \gitModulePathEntry ->
    let repository = buildGitModuleRepository "/home/test" gitModulePathEntry
        pathSegments = splitDirectories gitModulePathEntry
     in case pathSegments of
          [hostSegment, ownerSegment, repositorySegment] ->
            gitModuleRepositoryCompatible repository
              && gitModuleRepositoryPathEntry repository == gitModulePathEntry
              && gitModuleRepositoryPath repository == "/home/test" </> gitModulePathEntry
              && gitModuleRepositoryHost repository == hostSegment
              && gitModuleRepositoryOwner repository == ownerSegment
              && gitModuleRepositoryName repository == repositorySegment
          _ -> False
prop_buildGitModuleRepositoryRejectsMalformedPaths :: QC.Property
prop_buildGitModuleRepositoryRejectsMalformedPaths =
  QC.forAll malformedPathGen $ \gitModulePathEntry ->
    let repository = buildGitModuleRepository "/home/test" gitModulePathEntry
     in not (gitModuleRepositoryCompatible repository) && gitModuleRepositoryPathEntry repository == gitModulePathEntry
gitModulePathEntriesGen :: QC.Gen [FilePath]
gitModulePathEntriesGen = QC.listOf goStylePathGen
goStylePathGen :: QC.Gen FilePath
goStylePathGen = do
  hostSegment <- hostSegmentGen
  ownerSegment <- pathSegmentGen "-"
  repositorySegment <- pathSegmentGen "-_"
  pure (intercalate "/" [hostSegment, ownerSegment, repositorySegment])
hostSegmentGen :: QC.Gen String
hostSegmentGen = do
  firstCharacter <- QC.elements (['a' .. 'z'] ++ ['0' .. '9'])
  restCharacters <- QC.listOf (QC.elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "."))
  pure (firstCharacter : restCharacters)
malformedPathGen :: QC.Gen FilePath
malformedPathGen = do
  segmentCount <- QC.elements [0, 1, 2, 4, 5]
  segments <- QC.vectorOf segmentCount (pathSegmentGen "-_.")
  pure (intercalate "/" segments)
pathSegmentGen :: [Char] -> QC.Gen String
pathSegmentGen extraCharacters =
  QC.listOf1 (QC.elements (['a' .. 'z'] ++ ['0' .. '9'] ++ extraCharacters))
debugTests :: Test
debugTests =
  TestList
    [ TestCase $ do
        inferred <- inferTemplateName "test" uncommentFixture
        assertEqual
          "Infers the uncomment template."
          (Just "uncomment_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" rustFixture
        assertEqual
          "Infers the Rust package template."
          (Just "rust_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" haskellFixture
        assertEqual
          "Infers the Haskell package template."
          (Just "haskell_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonFixture
        assertEqual
          "Infers the Python template."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonPyPIFixture
        assertEqual
          "Infers the PyPI Python template."
          (Just "python_pypi_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" binaryReleaseFixture
        assertEqual
          "Infers the binary release template."
          (Just "binary_release_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonLaTeXFixture
        assertEqual
          "Infers the Python template for the python-latex fixture."
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "deploy_host_template" deployHostFixture
        assertEqual
          "Infers the deploy host template."
          (Just "deploy_host_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" cFixture
        assertEqual
          "Infers the C template."
          (Just "c_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" latexFixture
        assertEqual
          "Infers the LaTeX template."
          (Just "latex_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" htmlFixture
        assertEqual
          "Infers the HTML template."
          (Just "html_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" unknownFixture
        assertEqual
          "Returns no template for unknown input."
          Nothing
          inferred,
      TestCase $ do
        assertEqual
          "Compacts whitespace with oneLine."
          "a b c"
          (oneLine " a \n  b\t c "),
      TestCase $ do
        assertEqual
          "Formats issue details with issueLine."
          "  - missing key: src"
          (issueLine "missing key" "src"),
      TestCase $ do
        assertEqual
          "Extracts the package section with extractTomlSection."
          "name = \"example-package\"\nversion = \"0.1.0\"\nedition = \"2021\"\ndescription = \"Example package fixture for TOML parsing.\"\nlicense = \"MIT\"\nrepository = \"https://github.com/pbizopoulos/canonicalization\"\nreadme = \"../../README\"\nkeywords = [\"check\", \"lint\", \"fixture\"]\ncategories = [\"development-tools\"]\n\n"
          (extractTomlSection "package" exampleCargoFixture),
      TestCase $ do
        assertEqual
          "Parses the package name with lookupTomlString."
          (Just "remove-empty-lines")
          (lookupTomlString "name" (extractTomlSection "package" removeEmptyLinesCargoFixture)),
      TestCase $ do
        assertEqual
          "Parses lints.rust.unsafe_code with lookupTomlString."
          (Just "forbid")
          (lookupTomlString "unsafe_code" (extractTomlSection "lints.rust" removeEmptyLinesCargoFixture)),
      TestCase $ do
        assertEqual
          "Marks canonical go-style .gitmodules path as compatible."
          True
          (gitModuleRepositoryCompatible (buildGitModuleRepository "/home/user" "github.com/pbizopoulos/canonicalization")),
      TestCase $ do
        assertEqual
          "Rejects non go-style .gitmodules path with extra segments."
          False
          (gitModuleRepositoryCompatible (buildGitModuleRepository "/home/user" "github.com/pbizopoulos/canonicalization/subdir"))
    ]
rustFixture :: String
rustFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.rustPlatform.buildRustPackage {\n"
    ++ "  cargoHash = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\";\n"
    ++ "}\n"
uncommentFixture :: String
uncommentFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  nativeBuildInputs = [ pkgs.autoPatchelfHook ];\n"
    ++ "  src = pkgs.fetchurl { url = \"https://github.com/Goldziher/${pname}/releases/download/v${version}/${pname}-x86_64-unknown-linux-gnu.tar.gz\"; sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
haskellFixture :: String
haskellFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.haskellPackages.mkDerivation rec {\n"
    ++ "  pname = baseNameOf ./.;\n"
    ++ "}\n"
pythonFixture :: String
pythonFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.python3Packages.buildPythonPackage rec {\n"
    ++ "  src = ./.;\n"
    ++ "}\n"
pythonLaTeXFixture :: String
pythonLaTeXFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.python3Packages.buildPythonPackage rec {\n"
    ++ "  installPhase = '' latexmk -cd -pdf tmp/ms.tex '';\n"
    ++ "}\n"
pythonPyPIFixture :: String
pythonPyPIFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "let pyPkgs = pkgs.python3Packages; in\n"
    ++ "pyPkgs.buildPythonPackage rec {\n"
    ++ "  src = pyPkgs.fetchPypi { pname = \"x\"; version = \"1.0.0\"; hash = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
deployHostFixture :: String
deployHostFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeShellApplication {\n"
    ++ "  runtimeInputs = [ pkgs.opentofu ];\n"
    ++ "  text = \"echo agenix-shell\";\n"
    ++ "}\n"
cFixture :: String
cFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  buildPhase = '' cc -o ${pname} main.c -std=c89 '';\n"
    ++ "}\n"
latexFixture :: String
latexFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  buildPhase = '' latexmk -pdf ms.tex '';\n"
    ++ "}\n"
htmlFixture :: String
htmlFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeShellScriptBin \"x\" ''\n"
    ++ "  echo hi\n"
    ++ "''\n"
unknownFixture :: String
unknownFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.writeText \"x\" \"y\"\n"
binaryReleaseFixture :: String
binaryReleaseFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.stdenv.mkDerivation rec {\n"
    ++ "  sourceRoot = \".\";\n"
    ++ "  installPhase = ''\n"
    ++ "    install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname} $out/bin/${pname}\n"
    ++ "  '';\n"
    ++ "  src = pkgs.fetchurl { url = \"https://example.invalid/tool.tar.gz\"; sha256 = \"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"; };\n"
    ++ "}\n"
exampleCargoFixture :: T.Text
exampleCargoFixture =
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
removeEmptyLinesCargoFixture :: T.Text
removeEmptyLinesCargoFixture =
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
rustCargoTightnessBaseline :: T.Text
rustCargoTightnessBaseline =
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
haskellCabalTightnessBaseline :: T.Text
haskellCabalTightnessBaseline =
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
haskellTemplateBaseline :: T.Text
haskellTemplateBaseline =
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
rustTemplateBaseline :: T.Text
rustTemplateBaseline =
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
htmlTemplateBaseline :: T.Text
htmlTemplateBaseline =
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
cTemplateBaseline :: T.Text
cTemplateBaseline =
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
latexTemplateBaseline :: T.Text
latexTemplateBaseline =
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
deployHostTemplateBaseline :: T.Text
deployHostTemplateBaseline =
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
pythonTemplateBaseline :: T.Text
pythonTemplateBaseline =
  T.unlines
    [ "{",
      "  inputs,",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {",
      "    secrets.secrets.file = ../../secrets/secrets.age;",
      "  };",
      "  pyPkgs = pkgs.python312Packages;",
      "  python = pkgs.python312;",
      "in",
      "pyPkgs.buildPythonPackage rec {",
      "  installCheckPhase = ''",
      "    runHook preInstallCheck",
      "    HOME=\"$(mktemp -d)\"",
      "    DEBUG=1 \"$out/bin/${pname}\"",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py",
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
pythonPyPIBaseline :: T.Text
pythonPyPIBaseline =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  pyPkgs = pkgs.python312Packages;",
      "in",
      "pyPkgs.buildPythonPackage rec {",
      "  format = \"wheel\";",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = [];",
      "  pythonImportsCheck = [",
      "    pname",
      "  ];",
      "  src = pyPkgs.fetchPypi rec {",
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
binaryReleaseBaseline :: T.Text
binaryReleaseBaseline =
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
pythonLaTeXTemplateBaseline :: T.Text
pythonLaTeXTemplateBaseline =
  T.unlines
    [ "{",
      "  pkgs ? import <nixpkgs> { },",
      "}:",
      "let",
      "  pythonDeps = [",
      "    pkgs.python3Packages.matplotlib",
      "    pkgs.python3Packages.pandas",
      "  ];",
      "  pythonEnv = pkgs.python3.withPackages (_: pythonDeps);",
      "in",
      "pkgs.python3Packages.buildPythonPackage rec {",
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
uncommentTemplateBaseline :: T.Text
uncommentTemplateBaseline =
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
