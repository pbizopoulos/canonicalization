{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main, runPackageTests, runPackageTestsWithTimings) where
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Data.Char (isAlphaNum)
import Data.Kind (Type)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, getHomeDirectory, getTemporaryDirectory, removeFile, removePathForcibly, withCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath ((</>))
import System.FilePath.Windows qualified as Windows
import System.IO (hClose, hPutStr, hPutStrLn, openTempFile, stderr, stdout)
import System.Posix.Process (executeFile)
import System.Process (rawSystem, readProcessWithExitCode)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestLabel, TestList), assertBool, assertEqual, assertFailure, runTestTT)
import Prelude
type RepositoryLocation :: Type
data RepositoryLocation = RepositoryLocation
  { repositoryLocationHostname :: String,
    repositoryLocationPathComponents :: NonEmpty FilePath,
    repositoryLocationUrl :: String
  }
  deriving stock (Eq, Show)
main :: IO ()
main = getArgs >>= runCli
runCli :: [String] -> IO ()
runCli commandLineArgs =
  case commandLineArgs of
    [] -> printMainHelpAndExit (ExitFailure 1)
    ["help"] -> printMainHelpAndExit ExitSuccess
    _ | isHelpRequest commandLineArgs -> printCommandUsageAndExit ExitSuccess commandLineArgs
    ["init"] -> initializeHomeGitRepository
    "init" : _ -> printCommandUsageAndExit usageExitCode commandLineArgs
    ["add", repositoryUrl] ->
      case parseRepositoryUrl repositoryUrl of
        Just repositoryLocation -> addGitRepositoryFromLocation repositoryLocation
        Nothing -> printCommandUsageAndExit usageExitCode commandLineArgs
    "add" : _ -> printCommandUsageAndExit usageExitCode commandLineArgs
    ["check"] -> checkHomeGitmoduleCompliance >>= either failCanonicalizationCheck pure
    "check" : _ -> printCommandUsageAndExit usageExitCode commandLineArgs
    _ -> printCommandUsageAndExit usageExitCode commandLineArgs
printMainHelpAndExit :: ExitCode -> IO a
printMainHelpAndExit exitCode = do
  hPutStr (if exitCode == ExitSuccess then stdout else stderr) mainHelpText
  exitWith exitCode
printCommandUsageAndExit :: ExitCode -> [String] -> IO a
printCommandUsageAndExit exitCode commandLineArgs = do
  hPutStr
    (if exitCode == ExitSuccess then stdout else stderr)
    (usageTextForCommand (listToMaybe commandLineArgs))
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
      "   add        Add a repository below $HOME",
      "   check      Check $HOME/.gitmodules path entries",
      "   init       Initialize $HOME as a Git repository",
      "",
      "See 'git canonicalization <command> -h' for help on a specific command.",
      ""
    ]
usageTextForCommand :: Maybe String -> String
usageTextForCommand = \case
  Just "add" ->
    unlines
      [ "usage: git canonicalization add <https-or-ssh-repository-url>",
        "",
        "Add the repository below $HOME as <hostname>/<repository-path>.",
        ""
      ]
  Just "check" ->
    unlines
      [ "usage: git canonicalization check",
        "",
        "Check that every $HOME/.gitmodules path is <host>/<repository-path>.",
        ""
      ]
  Just "init" ->
    unlines
      [ "usage: git canonicalization init",
        "",
        "Initialize $HOME as a Git repository and ensure its .gitignore starts with *.",
        "Additional whitelist rules must start with !.",
        ""
      ]
  _ -> unlines ["usage: git canonicalization [-h | --help] <command> [<args>]", ""]
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
repositoryLocationFromUrlPath repositoryUrl hostnameAndPath = do
  let (hostname, slashAndPath) = break (== '/') hostnameAndPath
  repositoryPath <- stripPrefix "/" slashAndPath
  repositoryLocationFromUrlParts repositoryUrl hostname repositoryPath
repositoryLocationFromUrlParts :: String -> String -> String -> Maybe RepositoryLocation
repositoryLocationFromUrlParts repositoryUrl hostname repositoryPath =
  case reverse (T.splitOn "/" (T.dropWhileEnd (== '/') (T.pack repositoryPath))) of
    repositoryNameWithSuffix : reversedParentPathComponents -> do
      let repositoryName = fromMaybe repositoryNameWithSuffix (T.stripSuffix ".git" repositoryNameWithSuffix)
          pathComponents = reverse reversedParentPathComponents ++ [repositoryName]
      nonEmptyPathComponents <- NE.nonEmpty pathComponents
      if T.null (T.pack hostname) || any T.null pathComponents
        then Nothing
        else
          Just
            RepositoryLocation
              { repositoryLocationHostname = hostname,
                repositoryLocationPathComponents = T.unpack <$> nonEmptyPathComponents,
                repositoryLocationUrl = repositoryUrl
              }
    [] -> Nothing
addGitRepositoryFromLocation :: RepositoryLocation -> IO ()
addGitRepositoryFromLocation repositoryLocation = do
  homeDirectory <- getHomeDirectory
  case gitSubmoduleAddArguments homeDirectory repositoryLocation of
    Left validationError -> dieWithFatal validationError
    Right gitArguments -> executeFile "git" True gitArguments Nothing
gitSubmoduleAddArguments :: FilePath -> RepositoryLocation -> Either String [String]
gitSubmoduleAddArguments homeDirectory repositoryLocation =
  case catMaybes (validateNewName "hostname" hostname : map (validateNewName "repository path component") pathComponents) of
    validationError : _ -> Left validationError
    [] ->
      let repositoryPathEntry = foldl (</>) hostname pathComponents
          repositoryUrl = repositoryLocationUrl repositoryLocation
       in Right ["-C", homeDirectory, "submodule", "add", "--force", repositoryUrl, repositoryPathEntry]
  where
    hostname = repositoryLocationHostname repositoryLocation
    pathComponents = NE.toList (repositoryLocationPathComponents repositoryLocation)
validateNewName :: String -> FilePath -> Maybe String
validateNewName nameKind name
  | null name = Just (nameKind ++ " name must not be empty")
  | name `elem` [".", ".."] = Just (nameKind ++ " name must not be '.' or '..'")
  | any Windows.isPathSeparator name = Just (nameKind ++ " name must not contain path separators")
  | not (all isAllowedNameCharacter name) = Just (nameKind ++ " name must contain only letters, digits, '.', '-', or '_'")
  | otherwise = Nothing
  where
    isAllowedNameCharacter character = isAlphaNum character || character `elem` ("._-" :: String)
initializeHomeGitRepository :: IO ()
initializeHomeGitRepository = do
  homeDirectory <- getHomeDirectory
  let gitignorePath = homeDirectory </> ".gitignore"
  gitignoreExists <- doesFileExist gitignorePath
  when gitignoreExists $ do
    gitignoreContents <- TIO.readFile gitignorePath
    unless (homeGitignoreIsCompatible gitignoreContents) $
      dieWithFatal (gitignorePath ++ ": existing file must start with * and subsequent lines must start with !")
  runGitAndWait ["init", homeDirectory]
  unless gitignoreExists (TIO.writeFile gitignorePath "*\n")
homeGitignoreIsCompatible :: T.Text -> Bool
homeGitignoreIsCompatible gitignoreContents =
  case T.lines gitignoreContents of
    "*" : whitelistRules -> all (T.isPrefixOf "!") whitelistRules
    _ -> False
runGitAndWait :: [String] -> IO ()
runGitAndWait gitArguments = do
  gitExit <- rawSystem "git" gitArguments
  when (gitExit /= ExitSuccess) (exitWith gitExit)
checkHomeGitmoduleCompliance :: IO (Either String ())
checkHomeGitmoduleCompliance = do
  homeDirectory <- getHomeDirectory
  let gitmodulesPath = homeDirectory </> ".gitmodules"
  gitmodulesExists <- doesFileExist gitmodulesPath
  if not gitmodulesExists
    then pure (Left ("missing file: " ++ gitmodulesPath))
    else do
      (gitConfigExit, gitConfigStdout, gitConfigStderr) <-
        readProcessWithExitCode "git" ["config", "get", "--file", gitmodulesPath, "--null", "--all", "--regexp", "(^|\\.)path$"] ""
      pathEntries <-
        case gitConfigExit of
          ExitSuccess -> pure (parseNullSeparatedValues gitConfigStdout)
          ExitFailure 1
            | null gitConfigStdout && null gitConfigStderr -> pure []
          _ -> do
            putStr gitConfigStdout
            hPutStr stderr gitConfigStderr
            exitWith gitConfigExit
      pure $
        case filter (not . isCompatibleHomeGitmodulePath) pathEntries of
          [] -> Right ()
          invalidPathEntries ->
            Left
              ( intercalate
                  "\n"
                  [ T.unpack pathEntry ++ ": must be <host>/<repository-path> with valid path components"
                  | pathEntry <- invalidPathEntries
                  ]
              )
parseNullSeparatedValues :: String -> [T.Text]
parseNullSeparatedValues = nub . filter (not . T.null) . T.splitOn "\0" . T.pack
isCompatibleHomeGitmodulePath :: T.Text -> Bool
isCompatibleHomeGitmodulePath pathEntry =
  case T.splitOn "/" pathEntry of
    hostname : repositoryPathComponents ->
      not (T.null hostname)
        && not (null repositoryPathComponents)
        && all (isNothing . validateNewName "path component" . T.unpack) (hostname : repositoryPathComponents)
    [] -> False
failCanonicalizationCheck :: String -> IO a
failCanonicalizationCheck diagnostic = do
  forM_ (lines diagnostic) (hPutStrLn stderr . ("error: " ++))
  exitFailure
dieWithFatal :: String -> IO a
dieWithFatal diagnostic = do
  hPutStrLn stderr ("fatal: " ++ diagnostic)
  exitWith (ExitFailure 128)
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
    [ TestLabel "Parses nested repository URLs and builds canonical Git arguments." (TestCase repositoryUrlParsingTest),
      TestLabel "Documents the focused Git wrapper commands." (TestCase commandLineHelpEndToEndTest),
      TestLabel "Rejects missing, unknown, and over-specified commands." (TestCase invalidCommandEndToEndTest),
      TestLabel "Initializes and safely reinitializes a compatible home repository." (TestCase initCompatibleHomeEndToEndTest),
      TestLabel "Rejects initialization when the home ignore policy conflicts." (TestCase initConflictingHomeEndToEndTest),
      TestLabel "Checks home gitmodules independently of the current repository." (TestCase checkHomeFromNestedRepositoryEndToEndTest),
      TestLabel "Rejects missing and malformed home gitmodules." (TestCase checkInvalidHomeGitmodulesEndToEndTest)
    ]
repositoryUrlParsingTest :: IO ()
repositoryUrlParsingTest = do
  let kernelRepositoryUrl :: String
      kernelRepositoryUrl = "https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/"
      kernelRepositoryLocation =
        RepositoryLocation
          { repositoryLocationHostname = "git.kernel.org",
            repositoryLocationPathComponents = "pub" :| ["scm", "editors", "uemacs", "uemacs"],
            repositoryLocationUrl = kernelRepositoryUrl
          }
  assertEqual
    "Nested namespaces and suffixes are normalized for the local path."
    (Just kernelRepositoryLocation)
    (parseRepositoryUrl kernelRepositoryUrl)
  assertEqual
    "Git receives the original URL and complete local hierarchy."
    (Right ["-C", "/home/example", "submodule", "add", "--force", kernelRepositoryUrl, "git.kernel.org/pub/scm/editors/uemacs/uemacs"])
    (gitSubmoduleAddArguments "/home/example" kernelRepositoryLocation)
  assertEqual "An empty repository path is rejected." Nothing (parseRepositoryUrl "https://example.test/")
  assertBool "Nested home paths are accepted." (isCompatibleHomeGitmodulePath "example.test/owner/demo")
  assertBool "Traversal components are rejected." (not (isCompatibleHomeGitmodulePath "example.test/owner/../demo"))
commandLineHelpEndToEndTest :: IO ()
commandLineHelpEndToEndTest =
  withTemporaryDirectory "git-canonicalization-help" $ \temporaryDirectory -> do
    (helpExit, helpStdout, helpStderr) <- runEndToEndCommandIn temporaryDirectory temporaryDirectory ["-h"]
    assertEqual "Top-level help succeeds." ExitSuccess helpExit
    assertBool "Top-level help uses Git subcommand syntax." ("usage: git canonicalization" `isPrefixOf` helpStdout)
    assertEqual "Top-level help leaves stderr empty." "" helpStderr
    forM_ ["add", "check", "init"] $ \commandName -> do
      (commandExit, commandStdout, commandStderr) <- runEndToEndCommandIn temporaryDirectory temporaryDirectory [commandName, "--help"]
      assertEqual "Command help succeeds." ExitSuccess commandExit
      assertBool "Command help names the selected command." (("git canonicalization " ++ commandName) `isInfixOf` commandStdout)
      assertEqual "Command help leaves stderr empty." "" commandStderr
invalidCommandEndToEndTest :: IO ()
invalidCommandEndToEndTest =
  withTemporaryDirectory "git-canonicalization-invalid" $ \temporaryDirectory -> do
    forM_ [[], ["unknown"], ["check", "."], ["init", "extra"], ["add"]] $ \arguments -> do
      (commandExit, commandStdout, commandStderr) <- runEndToEndCommandIn temporaryDirectory temporaryDirectory arguments
      assertBool "Invalid commands fail." (commandExit /= ExitSuccess)
      assertEqual "Invalid commands leave stdout empty." "" commandStdout
      assertBool "Invalid commands print usage." ("usage: git canonicalization" `isPrefixOf` commandStderr)
initCompatibleHomeEndToEndTest :: IO ()
initCompatibleHomeEndToEndTest =
  withTemporaryDirectory "git-canonicalization-init" $ \temporaryHome -> do
    (initExit, _initStdout, _initStderr) <- runEndToEndCommandIn temporaryHome temporaryHome ["init"]
    assertEqual "Home initialization succeeds." ExitSuccess initExit
    doesDirectoryExist (temporaryHome </> ".git") >>= assertBool "Initialization creates a Git repository."
    TIO.readFile (temporaryHome </> ".gitignore") >>= assertEqual "Initialization creates the canonical ignore rule." "*\n"
    TIO.writeFile (temporaryHome </> ".gitignore") "*\n!.gitmodules\n!github.com/\n"
    (reinitExit, _reinitStdout, _reinitStderr) <- runEndToEndCommandIn temporaryHome temporaryHome ["init"]
    assertEqual "Compatible reinitialization succeeds." ExitSuccess reinitExit
initConflictingHomeEndToEndTest :: IO ()
initConflictingHomeEndToEndTest =
  withTemporaryDirectory "git-canonicalization-init-conflict" $ \temporaryHome -> do
    TIO.writeFile (temporaryHome </> ".gitignore") "*.tmp\n"
    (initExit, initStdout, initStderr) <- runEndToEndCommandIn temporaryHome temporaryHome ["init"]
    assertEqual "Conflicting initialization uses Git's fatal status." (ExitFailure 128) initExit
    assertEqual "Conflicting initialization leaves stdout empty." "" initStdout
    assertBool "The conflict is diagnosed." ("existing file must start with *" `isInfixOf` initStderr)
    doesPathExist (temporaryHome </> ".git") >>= assertBool "Rejected initialization has no side effects." . not
checkHomeFromNestedRepositoryEndToEndTest :: IO ()
checkHomeFromNestedRepositoryEndToEndTest =
  withTemporaryDirectory "git-canonicalization-check" $ \temporaryHome -> do
    TIO.writeFile (temporaryHome </> ".gitmodules") "path = example.test/owner/demo\n"
    let nestedRepository = temporaryHome </> "workspace"
    createDirectoryIfMissing True nestedRepository
    runGitFixtureCommand ["init", "--quiet", nestedRepository]
    (checkExit, checkStdout, checkStderr) <- runEndToEndCommandIn temporaryHome nestedRepository ["check"]
    assertEqual "The home check succeeds from a different repository." ExitSuccess checkExit
    assertEqual "A successful check leaves stdout empty." "" checkStdout
    assertEqual "A successful check leaves stderr empty." "" checkStderr
checkInvalidHomeGitmodulesEndToEndTest :: IO ()
checkInvalidHomeGitmodulesEndToEndTest =
  withTemporaryDirectory "git-canonicalization-check-invalid" $ \temporaryHome -> do
    (missingExit, _missingStdout, missingStderr) <- runEndToEndCommandIn temporaryHome temporaryHome ["check"]
    assertEqual "A missing gitmodules file fails." (ExitFailure 1) missingExit
    assertBool "The missing file is diagnosed." ("missing file" `isInfixOf` missingStderr)
    TIO.writeFile (temporaryHome </> ".gitmodules") "path = repository\n"
    (malformedExit, _malformedStdout, malformedStderr) <- runEndToEndCommandIn temporaryHome temporaryHome ["check"]
    assertEqual "A malformed path fails." (ExitFailure 1) malformedExit
    assertBool "The path policy is diagnosed." ("must be <host>/<repository-path>" `isInfixOf` malformedStderr)
runEndToEndCommandIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runEndToEndCommandIn temporaryHome workingDirectory arguments =
  withEnvironmentVariable "HOME" temporaryHome $
    withCurrentDirectory workingDirectory (readProcessWithExitCode "git" ("canonicalization" : arguments) "")
runGitFixtureCommand :: [String] -> IO ()
runGitFixtureCommand arguments = do
  (gitExit, _gitStdout, gitStderr) <- readProcessWithExitCode "git" arguments ""
  when (gitExit /= ExitSuccess) (assertFailure ("Git fixture command failed: " ++ gitStderr))
withTemporaryDirectory :: String -> (FilePath -> IO a) -> IO a
withTemporaryDirectory temporaryName action = do
  temporaryDirectory <- getTemporaryDirectory
  (temporaryPath, temporaryHandle) <- openTempFile temporaryDirectory temporaryName
  hClose temporaryHandle
  removeFile temporaryPath
  createDirectoryIfMissing True temporaryPath
  action temporaryPath `finally` removePathForcibly temporaryPath
withEnvironmentVariable :: String -> String -> IO a -> IO a
withEnvironmentVariable variableName variableValue action = do
  originalValue <- lookupEnv variableName
  setEnv variableName variableValue
  action `finally` restoreEnvironmentVariable originalValue
  where
    restoreEnvironmentVariable = \case
      Nothing -> unsetEnv variableName
      Just value -> setEnv variableName value
