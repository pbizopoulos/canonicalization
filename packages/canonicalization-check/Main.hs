{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main) where
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (forM, unless, when)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, sort, sortBy, stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VCP
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
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, removeFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.FilePath ((<.>), (</>))
import System.FilePath.Posix (splitDirectories, takeBaseName, takeDirectory, takeFileName)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertEqual, runTestTT)
import Text.Regex.TDFA ((=~))
import Prelude
defaultAllowedKeys :: Set.Set T.Text
defaultAllowedKeys =
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
      "runtimeInputs"
    ]
type TemplateSpec :: Type
data TemplateSpec = TemplateSpec
  { templateName :: FilePath,
    matchesTemplate :: FilePath -> String -> IO Bool,
    allowedDifferenceKeys :: Set.Set T.Text,
    embeddedBaseline :: Maybe T.Text
  }
templateSpecs :: [TemplateSpec]
templateSpecs =
  [ TemplateSpec
      { templateName = "haskell_package_baseline",
        matchesTemplate = \_ content -> pure ("haskellPackages.mkDerivation" `isInfixOf` content),
        allowedDifferenceKeys = Set.insert "passthru" defaultAllowedKeys,
        embeddedBaseline = Just haskellTemplateBaseline
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        matchesTemplate = \_ content -> pure ("rustPlatform.buildRustPackage" `isInfixOf` content),
        allowedDifferenceKeys = Set.insert "passthru" defaultAllowedKeys,
        embeddedBaseline = Just rustTemplateBaseline
      },
    TemplateSpec
      { templateName = "html_template",
        matchesTemplate = \_ content -> pure ("writeShellScriptBin" `isInfixOf` content),
        allowedDifferenceKeys = Set.insert "text" defaultAllowedKeys,
        embeddedBaseline = Just htmlTemplateBaseline
      },
    TemplateSpec
      { templateName = "python_latex_template",
        matchesTemplate = pythonLatexDetector,
        allowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "version"],
        embeddedBaseline = Just pythonLatexTemplateBaseline
      },
    TemplateSpec
      { templateName = "python_remote_template",
        matchesTemplate = pythonRemoteDetector,
        allowedDifferenceKeys = Set.fromList ["nativeBuildInputs", "propagatedBuildInputs", "src", "version"],
        embeddedBaseline = Just pythonRemoteBaseline
      },
    TemplateSpec
      { templateName = "binary_release_template",
        matchesTemplate = binaryReleaseDetector,
        allowedDifferenceKeys = Set.fromList ["src", "version"],
        embeddedBaseline = Just binaryReleaseBaseline
      },
    TemplateSpec
      { templateName = "python_template",
        matchesTemplate = \_ content -> pure ("buildPythonPackage" `isInfixOf` content),
        allowedDifferenceKeys = Set.fromList ["propagatedBuildInputs", "shellHook", "version"],
        embeddedBaseline = Just pythonTemplateBaseline
      },
    TemplateSpec
      { templateName = "deploy_host_template",
        matchesTemplate = \_ content ->
          pure
            ( "writeShellApplication" `isInfixOf` content
                && ("opentofu" `isInfixOf` content || "agenix-shell" `isInfixOf` content)
            ),
        allowedDifferenceKeys = defaultAllowedKeys,
        embeddedBaseline = Just deployHostTemplateBaseline
      },
    TemplateSpec
      { templateName = "latex_template",
        matchesTemplate = \_ content ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` content
                && "latexmk -pdf ms.tex" `isInfixOf` content
            ),
        allowedDifferenceKeys = defaultAllowedKeys,
        embeddedBaseline = Just latexTemplateBaseline
      },
    TemplateSpec
      { templateName = "c_template",
        matchesTemplate = \_ content ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` content
                && "cc -o ${pname} main.c -std=c89" `isInfixOf` content
            ),
        allowedDifferenceKeys = Set.union defaultAllowedKeys (Set.fromList ["buildPhase", "checkPhase"]),
        embeddedBaseline = Just cTemplateBaseline
      },
    TemplateSpec
      { templateName = "uncomment_template",
        matchesTemplate = \_ content ->
          pure
            ( "stdenv.mkDerivation" `isInfixOf` content
                && "autoPatchelfHook" `isInfixOf` content
                && "Goldziher" `isInfixOf` content
            ),
        allowedDifferenceKeys = Set.union defaultAllowedKeys (Set.fromList ["pname", "src"]),
        embeddedBaseline = Just uncommentTemplateBaseline
      }
  ]
pythonLatexDetector :: FilePath -> String -> IO Bool
pythonLatexDetector packageName content
  | "buildPythonPackage" `isInfixOf` content = do
      let packageDir = "packages" </> packageName
      hasMsTex <- doesFileExist (packageDir </> "ms.tex")
      hasRefsBib <- doesFileExist (packageDir </> "refs.bib")
      hasFiguresDir <- doesDirectoryExist (packageDir </> "figures")
      pure (hasMsTex || hasRefsBib || hasFiguresDir)
  | otherwise = pure False
pythonRemoteDetector :: FilePath -> String -> IO Bool
pythonRemoteDetector _ content =
  pure
    ( "buildPythonPackage" `isInfixOf` content
        && not ("src = ./.;" `isInfixOf` content)
        && ("fetchPypi" `isInfixOf` content || "fetchurl" `isInfixOf` content)
    )
binaryReleaseDetector :: FilePath -> String -> IO Bool
binaryReleaseDetector _ content =
  pure
    ( "stdenv.mkDerivation" `isInfixOf` content
        && "src = pkgs.fetchurl" `isInfixOf` content
        && "sourceRoot = \".\";" `isInfixOf` content
        && "install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname}" `isInfixOf` content
    )
templateSpecByName :: FilePath -> Maybe TemplateSpec
templateSpecByName name = find ((== name) . templateName) templateSpecs
type CheckResults :: Type
data CheckResults = CheckResults
  { structureIssues :: [String],
    packageResults :: [PackageCheck],
    hostResults :: [HostCheck]
  }
type CheckStatus :: Type
data CheckStatus = Passed | Failed | Skipped deriving stock (Eq, Show)
statusFromIssues :: [a] -> CheckStatus
statusFromIssues = \case [] -> Passed; _ -> Failed
type TestResult :: Type
data TestResult = TestResult
  { testName :: String,
    testStatus :: CheckStatus,
    testCases :: [TestCaseResult]
  }
type TestCaseResult :: Type
data TestCaseResult = TestCaseResult
  { caseName :: String,
    caseStatus :: CheckStatus,
    caseDetails :: [String]
  }
type PackageCheck :: Type
data PackageCheck = PackageCheck
  { packageNodeName :: String,
    packageKind :: ProjectKind,
    packageTests :: [TestResult],
    packageErrors :: [String]
  }
type HostCheck :: Type
data HostCheck = HostCheck
  { hostName :: String,
    hostTests :: [TestResult],
    hostErrors :: [String]
  }
main :: IO ()
main = do
  debug <- lookupEnv "DEBUG"
  args <- getArgs
  case debug of
    Just "1" -> runDebugTests
    _ ->
      case args of
        ["tui"] -> runTuiMode
        _ -> runCheckMode
runCheckAnalysis :: IO CheckResults
runCheckAnalysis = do
  foundStructureIssues <- checkRepositoryStructure
  foundPackageNames <- listPackageNames
  foundPackageResults <- forM foundPackageNames (checkPackage foundStructureIssues)
  foundHostNames <- listHostNames
  let foundHostResults = map (buildHostCheck foundStructureIssues) foundHostNames
  pure
    CheckResults
      { structureIssues = foundStructureIssues,
        packageResults = foundPackageResults,
        hostResults = foundHostResults
      }
runCheckMode :: IO ()
runCheckMode = do
  results <- runCheckAnalysis
  failOnIssues True results
runTuiMode :: IO ()
runTuiMode = do
  results <- runCheckAnalysis
  latestResults <- runTui results
  failOnIssues False latestResults
allIssuesFromResults :: CheckResults -> [String]
allIssuesFromResults results = structureIssues results ++ concatMap packageErrors (packageResults results)
failOnIssues :: Bool -> CheckResults -> IO ()
failOnIssues printIssues results = do
  let allIssues = allIssuesFromResults results
  unless (null allIssues) $ do
    when printIssues (mapM_ putStrLn allIssues)
    exitFailure
type TreeNode :: Type
data TreeNode = TreeNode
  { nodeId :: String,
    nodeLabel :: String,
    nodeStatus :: CheckStatus,
    nodeChildren :: [TreeNode]
  }
type VisibleNode :: Type
data VisibleNode = VisibleNode
  { visibleNode :: TreeNode,
    visibleDepth :: Int,
    visibleParentId :: Maybe String
  }
type UiState :: Type
data UiState = UiState
  { uiTree :: TreeNode,
    uiExpanded :: Set.Set String,
    uiViewportTop :: Int,
    uiSelected :: Int,
    uiPendingG :: Bool,
    uiPendingZ :: Bool,
    uiNotice :: Maybe String,
    uiLatestResults :: CheckResults
  }
runTui :: CheckResults -> IO CheckResults
runTui results = do
  let tree = buildCheckTree results
      initialState =
        UiState
          { uiTree = tree,
            uiExpanded = Set.fromList [nodeId tree, "packages", "hosts"],
            uiViewportTop = 0,
            uiSelected = 0,
            uiPendingG = False,
            uiPendingZ = False,
            uiNotice = Nothing,
            uiLatestResults = results
          }
  vty <- VCP.mkVty V.defaultConfig
  finalState <- runTuiLoop vty initialState
  V.shutdown vty
  pure (uiLatestResults finalState)
buildCheckTree :: CheckResults -> TreeNode
buildCheckTree results =
  let packageNodes = map packageNode (packageResults results)
      hostNodes = map hostNode (hostResults results)
      allIssues = structureIssues results ++ concatMap packageErrors (packageResults results)
      repoStatus = if null allIssues then Passed else Failed
   in TreeNode
        { nodeId = "repository",
          nodeLabel = "repository",
          nodeStatus = repoStatus,
          nodeChildren =
            [ TreeNode
                { nodeId = "packages",
                  nodeLabel = "packages",
                  nodeStatus = aggregateStatus (map nodeStatus packageNodes),
                  nodeChildren = packageNodes
                },
              TreeNode
                { nodeId = "hosts",
                  nodeLabel = "hosts",
                  nodeStatus = aggregateStatus (map nodeStatus hostNodes),
                  nodeChildren = hostNodes
                }
            ]
        }
packageNode :: PackageCheck -> TreeNode
packageNode pkg =
  let packageId = "packages/" ++ packageNodeName pkg
      testNodes = map (buildTestTreeNode packageId) (packageTests pkg)
      fileIssueNodes = buildPackageFileIssueNodes pkg (Set.fromList (map testName (packageTests pkg)))
   in TreeNode
        { nodeId = packageId,
          nodeLabel = packageNodeName pkg ++ " (" ++ projectKindLabel (packageKind pkg) ++ ")",
          nodeStatus = packageNodeStatus pkg,
          nodeChildren = fileIssueNodes ++ testNodes
        }
hostNode :: HostCheck -> TreeNode
hostNode host =
  TreeNode
    { nodeId = "hosts/" ++ hostName host,
      nodeLabel = hostName host,
      nodeStatus = hostNodeStatus host,
      nodeChildren = map (buildTestTreeNode ("hosts/" ++ hostName host)) (hostTests host)
    }
runTuiLoop :: V.Vty -> UiState -> IO UiState
runTuiLoop vty state = do
  viewportHeight <- getViewportHeight vty state
  drawTuiFrame vty state
  event <- V.nextEvent vty
  case event of
    V.EvKey (V.KChar 'q') [] -> pure state
    V.EvKey V.KEsc [] -> pure state
    V.EvKey (V.KChar 'j') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveSelection viewportHeight 1 state)))
    V.EvKey V.KDown [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveSelection viewportHeight 1 state)))
    V.EvKey (V.KChar 'k') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveSelection viewportHeight (-1) state)))
    V.EvKey V.KUp [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveSelection viewportHeight (-1) state)))
    V.EvKey (V.KChar 'H') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveToViewportTop state)))
    V.EvKey (V.KChar 'M') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveToViewportMiddle viewportHeight state)))
    V.EvKey (V.KChar 'L') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (moveToViewportBottom viewportHeight state)))
    V.EvKey (V.KChar 'z') [] -> runTuiLoop vty (clearPendingG state {uiPendingZ = True})
    V.EvKey (V.KChar 'o') [] ->
      if uiPendingZ state
        then runTuiLoop vty (clearPendingZ (clearPendingG (expandAtSelection state)))
        else runTuiLoop vty (clearPendingZ (clearPendingG state))
    V.EvKey (V.KChar 'c') [] ->
      if uiPendingZ state
        then runTuiLoop vty (clearPendingZ (clearPendingG (collapseAtSelection state)))
        else runTuiLoop vty (clearPendingZ (clearPendingG state))
    V.EvKey (V.KChar 'g') [] ->
      if uiPendingG state
        then runTuiLoop vty (clearPendingZ (clearPendingG (goTop state)))
        else runTuiLoop vty (clearPendingZ (state {uiPendingG = True}))
    V.EvKey (V.KChar 'G') [] -> runTuiLoop vty (clearPendingZ (clearPendingG (goBottom viewportHeight state)))
    V.EvKey (V.KChar 'r') [] -> do
      let waitingState = state {uiNotice = Just "Running canonicalization checks..."}
      drawTuiFrame vty waitingState
      refreshedResults <- runCheckAnalysis
      runTuiLoop vty (resetUiForResults refreshedResults waitingState)
    _ -> runTuiLoop vty (clearPendingZ (clearPendingG state))
drawTuiFrame :: V.Vty -> UiState -> IO ()
drawTuiFrame vty state = do
  viewportHeight <- getViewportHeight vty state
  let allLineImages = zipWith (renderVisibleNodeImage state) [0 ..] (visibleNodes state)
      lineImages = take viewportHeight (drop (uiViewportTop state) allLineImages)
      noticePrefix =
        case uiNotice state of
          Nothing -> []
          Just noticeLine -> [V.string V.defAttr noticeLine]
      contentImages =
        if null lineImages
          then [V.string V.defAttr ""]
          else lineImages
      image = V.vertCat (noticePrefix ++ contentImages)
  V.update vty (V.picForImage image)
visibleNodes :: UiState -> [VisibleNode]
visibleNodes state = flattenVisible Nothing 0 (uiTree state)
  where
    flattenVisible parent depth node =
      let current = VisibleNode node depth parent
          expanded = Set.member (nodeId node) (uiExpanded state)
          descendants =
            if expanded
              then concatMap (flattenVisible (Just (nodeId node)) (depth + 1)) (nodeChildren node)
              else []
       in current : descendants
renderVisibleNodeImage :: UiState -> Int -> VisibleNode -> V.Image
renderVisibleNodeImage state index vnode =
  let node = visibleNode vnode
      depth = visibleDepth vnode
      isSelected = index == uiSelected state
      prefix = replicate (depth * 2) ' '
      foldMarker :: String
      foldMarker
        | null (nodeChildren node) = " "
        | Set.member (nodeId node) (uiExpanded state) = "▾"
        | otherwise = "▸"
      statusLabel :: String
      statusLabel = case nodeStatus node of
        Passed -> "OK"
        Failed -> "FAIL"
        Skipped -> "SKIP"
      selectedPrefix :: String
      selectedPrefix = if isSelected then "> " else "  "
      beforeStatus = selectedPrefix ++ prefix ++ foldMarker ++ " "
      statusToken = "[" ++ statusLabel ++ "]"
      afterStatus = " " ++ nodeLabel node
      statusAttr = case nodeStatus node of
        Passed -> V.withForeColor V.defAttr V.green
        Failed -> V.withForeColor V.defAttr V.red
        Skipped -> V.withForeColor V.defAttr V.yellow
   in V.horizCat [V.string V.defAttr beforeStatus, V.string statusAttr statusToken, V.string V.defAttr afterStatus]
moveSelection :: Int -> Int -> UiState -> UiState
moveSelection viewportHeight delta state =
  let total = length (visibleNodes state)
      current = uiSelected state
      next = max 0 (min (total - 1) (current + delta))
   in ensureSelectionVisible viewportHeight (state {uiSelected = next})
goTop :: UiState -> UiState
goTop state = state {uiSelected = 0, uiViewportTop = 0}
goBottom :: Int -> UiState -> UiState
goBottom viewportHeight state =
  let total = length (visibleNodes state)
   in ensureSelectionVisible viewportHeight (state {uiSelected = max 0 (total - 1)})
moveToViewportTop :: UiState -> UiState
moveToViewportTop state =
  let total = length (visibleNodes state)
      target = max 0 (min (total - 1) (uiViewportTop state))
   in state {uiSelected = target}
moveToViewportMiddle :: Int -> UiState -> UiState
moveToViewportMiddle viewportHeight state =
  let total = length (visibleNodes state)
      target = max 0 (min (total - 1) (uiViewportTop state + (viewportHeight `div` 2)))
   in state {uiSelected = target}
moveToViewportBottom :: Int -> UiState -> UiState
moveToViewportBottom viewportHeight state =
  let total = length (visibleNodes state)
      target = max 0 (min (total - 1) (uiViewportTop state + viewportHeight - 1))
   in state {uiSelected = target}
ensureSelectionVisible :: Int -> UiState -> UiState
ensureSelectionVisible viewportHeight state =
  let total = length (visibleNodes state)
      maxTop = max 0 (total - viewportHeight)
      sel = uiSelected state
      top0 = uiViewportTop state
      top1
        | sel < top0 = sel
        | sel >= top0 + viewportHeight = sel - viewportHeight + 1
        | otherwise = top0
      boundedTop = max 0 (min maxTop top1)
   in state {uiViewportTop = boundedTop}
clearPendingG :: UiState -> UiState
clearPendingG state = state {uiPendingG = False, uiNotice = Nothing}
clearPendingZ :: UiState -> UiState
clearPendingZ state = state {uiPendingZ = False, uiNotice = Nothing}
resetUiForResults :: CheckResults -> UiState -> UiState
resetUiForResults refreshedResults state =
  let tree = buildCheckTree refreshedResults
   in state
        { uiTree = tree,
          uiExpanded = Set.fromList [nodeId tree, "packages", "hosts"],
          uiViewportTop = 0,
          uiSelected = 0,
          uiPendingG = False,
          uiPendingZ = False,
          uiNotice = Nothing,
          uiLatestResults = refreshedResults
        }
getViewportHeight :: V.Vty -> UiState -> IO Int
getViewportHeight vty state = do
  (_, screenHeight) <- V.displayBounds (V.outputIface vty)
  let noticeLines :: Int
      noticeLines =
        case uiNotice state of
          Nothing -> 0
          Just _ -> 1
      available = screenHeight - noticeLines
  pure (max 1 available)
buildTestTreeNode :: String -> TestResult -> TreeNode
buildTestTreeNode parentNodeId testResult =
  let caseNodes =
        [ TreeNode
            { nodeId = parentNodeId ++ "/" ++ testName testResult ++ "/case/" ++ caseName caseResult,
              nodeLabel = caseName caseResult,
              nodeStatus = caseStatus caseResult,
              nodeChildren =
                [ TreeNode
                    { nodeId = parentNodeId ++ "/" ++ testName testResult ++ "/case/" ++ caseName caseResult ++ "/detail/" ++ show i,
                      nodeLabel = detail,
                      nodeStatus = caseStatus caseResult,
                      nodeChildren = []
                    }
                | (i, detail) <- zip [0 :: Int ..] (caseDetails caseResult)
                ]
            }
        | caseResult <- testCases testResult
        ]
   in TreeNode
        { nodeId = parentNodeId ++ "/" ++ testName testResult,
          nodeLabel = testName testResult,
          nodeStatus = testStatus testResult,
          nodeChildren = caseNodes
        }
expandAtSelection :: UiState -> UiState
expandAtSelection state =
  case selectedVisibleNode state of
    Nothing -> state
    Just vnode ->
      let node = visibleNode vnode
       in if null (nodeChildren node)
            then state
            else state {uiExpanded = Set.insert (nodeId node) (uiExpanded state)}
collapseAtSelection :: UiState -> UiState
collapseAtSelection state =
  case selectedVisibleNode state of
    Nothing -> state
    Just vnode ->
      let node = visibleNode vnode
          expanded = Set.member (nodeId node) (uiExpanded state)
       in if expanded && not (null (nodeChildren node))
            then state {uiExpanded = Set.delete (nodeId node) (uiExpanded state)}
            else case visibleParentId vnode of
              Nothing -> state
              Just parentId ->
                let allVisible = visibleNodes state
                    parentIndex = findVisibleIndex parentId allVisible
                    nextExpanded = Set.delete parentId (uiExpanded state)
                 in state {uiExpanded = nextExpanded, uiSelected = parentIndex}
selectedVisibleNode :: UiState -> Maybe VisibleNode
selectedVisibleNode state =
  let nodes = visibleNodes state
      idx = uiSelected state
   in if idx >= 0 && idx < length nodes then Just (nodes !! idx) else Nothing
findVisibleIndex :: String -> [VisibleNode] -> Int
findVisibleIndex targetId nodes =
  case [i | (i, vnode) <- zip [0 ..] nodes, nodeId (visibleNode vnode) == targetId] of
    i : _ -> i
    [] -> 0
packageNodeStatus :: PackageCheck -> CheckStatus
packageNodeStatus pkg
  | not (null (packageErrors pkg)) = Failed
  | otherwise = aggregateStatus (map testStatus (packageTests pkg))
hostNodeStatus :: HostCheck -> CheckStatus
hostNodeStatus host = aggregateStatus (map testStatus (hostTests host))
aggregateStatus :: [CheckStatus] -> CheckStatus
aggregateStatus statuses
  | Failed `elem` statuses = Failed
  | Passed `elem` statuses = Passed
  | otherwise = Skipped
projectKindLabel :: ProjectKind -> String
projectKindLabel kind =
  case kind of
    HaskellKind -> "haskell"
    RustKind -> "rust"
    HtmlKind -> "html"
    PythonLatexKind -> "python-latex"
    PythonKind -> "python"
    CKind -> "c"
    TerraformKind -> "terraform"
    LatexKind -> "latex"
    BinaryReleaseKind -> "binary"
    UnknownKind -> "unknown"
buildPackageFileIssueNodes :: PackageCheck -> Set.Set String -> [TreeNode]
buildPackageFileIssueNodes pkg representedFiles =
  let grouped = foldl collect Map.empty (filter (not . isDirectoryStructureIssue) (packageErrors pkg))
      collect acc issue =
        let (fileLabel, message) = splitIssueByFile (packageNodeName pkg) issue
         in Map.insertWith (++) fileLabel [message] acc
   in [ TreeNode
          { nodeId = "packages/" ++ packageNodeName pkg ++ "/file/" ++ fileLabel,
            nodeLabel = fileLabel,
            nodeStatus = Failed,
            nodeChildren =
              [ TreeNode
                  { nodeId = "packages/" ++ packageNodeName pkg ++ "/file/" ++ fileLabel ++ "/issue/" ++ show i,
                    nodeLabel = msg,
                    nodeStatus = Failed,
                    nodeChildren = []
                  }
              | (i, msg) <- zip [0 :: Int ..] (reverse messages)
              ]
          }
      | (fileLabel, messages) <- Map.toList grouped,
        not (Set.member fileLabel representedFiles)
      ]
splitIssueByFile :: FilePath -> String -> (String, String)
splitIssueByFile packageName issue =
  let prefix = "packages/" ++ packageName ++ "/"
      fallback :: (String, String)
      fallback = ("package", issue)
   in if prefix `isPrefixOf` issue
        then
          let rest = drop (length prefix) issue
           in case breakOnSubstring ":" rest of
                Nothing -> (if null rest then "package" else rest, issue)
                Just (filePart, afterColon) ->
                  let fileLabel = if null filePart then "package" else filePart
                      detail = dropWhile (== ' ') afterColon
                   in (fileLabel, if null detail then issue else detail)
        else fallback
isDirectoryStructureIssue :: String -> Bool
isDirectoryStructureIssue issue = ": is not allowed" `isInfixOf` issue
checkRepositoryStructure :: IO [String]
checkRepositoryStructure = do
  allPaths <- collectRepoPaths "."
  let relPaths = sort [path | path <- allPaths, path /= "."]
      leafPaths = Set.fromList (filter (isLeafPath relPaths) relPaths)
      packageRoots = Set.fromList (mapMaybe packageRoot relPaths)
      hostRoots = Set.fromList (mapMaybe hostRoot relPaths)
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
          [ allowedPatternsForKind (packageRootPath pkgInfo) (packageDirName pkgInfo) (detectedKind pkgInfo)
          | pkgInfo <- packageInfos
          ]
      allowedPatterns = globalAllowedPatterns ++ packageAllowedPatterns
      missingPackageDefaults =
        [ pkgRoot ++ ": is missing required default.nix"
        | pkgRoot <- Set.toList packageRoots,
          (pkgRoot </> "default.nix") `notElem` relPaths
        ]
      missingHostConfigs =
        [ hostDir ++ ": is missing required configuration.nix"
        | hostDir <- Set.toList hostRoots,
          (hostDir </> "configuration.nix") `notElem` relPaths
        ]
      missingCabalForMain =
        [ pkgRoot ++ ": is missing required " ++ pkgName ++ ".cabal for Main.hs package"
        | pkgRoot <- Set.toList packageRoots,
          Set.member (pkgRoot </> "Main.hs") (Set.fromList relPaths),
          let pkgName = takeBaseName pkgRoot,
          (pkgRoot </> pkgName <.> "cabal") `notElem` relPaths
        ]
      badlyNamedCabals =
        [ path ++ ": cabal file must be named " ++ pkgName ++ ".cabal"
        | path <- relPaths,
          ".cabal" `isSuffixOf` path,
          let pkgRoot = takeDirectory path,
          "packages/" `isPrefixOf` pkgRoot,
          let pkgName = takeBaseName pkgRoot,
          takeFileName path /= pkgName <.> "cabal"
        ]
      packageKindIssues =
        concatMap packageKindProblems packageInfos
      disallowedPaths =
        [ path ++ ": is not allowed"
        | path <- Set.toList leafPaths,
          not (any (`pathMatches` path) allowedPatterns)
        ]
  pure (missingPackageDefaults ++ missingHostConfigs ++ missingCabalForMain ++ badlyNamedCabals ++ packageKindIssues ++ disallowedPaths)
type ProjectKind :: Type
data ProjectKind
  = HaskellKind
  | RustKind
  | HtmlKind
  | PythonLatexKind
  | PythonKind
  | CKind
  | TerraformKind
  | LatexKind
  | BinaryReleaseKind
  | UnknownKind
  deriving stock (Eq, Ord, Show)
type PackageInfo :: Type
data PackageInfo = PackageInfo
  { packageRootPath :: FilePath,
    packageDirName :: FilePath,
    packageLeafFiles :: [FilePath],
    detectedKind :: ProjectKind,
    matchedMarkers :: [String]
  }
buildPackageInfo :: Set.Set FilePath -> FilePath -> PackageInfo
buildPackageInfo leafPaths pkgRoot =
  let pkgName = takeBaseName pkgRoot
      leafFiles =
        catMaybes
          [ stripPrefix (pkgRoot ++ "/") path
          | path <- Set.toList leafPaths,
            (pkgRoot ++ "/") `isPrefixOf` path
          ]
      markers = detectMarkers leafFiles
   in PackageInfo
        { packageRootPath = pkgRoot,
          packageDirName = pkgName,
          packageLeafFiles = leafFiles,
          detectedKind = detectKind markers,
          matchedMarkers = map fst markers
        }
detectMarkers :: [FilePath] -> [(String, ProjectKind)]
detectMarkers leafFiles =
  let has path = path `elem` leafFiles
      hasAny prefix = any (isPrefixOf prefix) leafFiles
   in catMaybes
        [ if has "Main.hs" then Just ("Main.hs", HaskellKind) else Nothing,
          if has "Cargo.toml" then Just ("Cargo.toml", RustKind) else Nothing,
          if has "index.html" then Just ("index.html", HtmlKind) else Nothing,
          if has "main.py" && has "ms.tex" then Just ("main.py+ms.tex", PythonLatexKind) else Nothing,
          if has "main.py" && not (has "ms.tex") then Just ("main.py", PythonKind) else Nothing,
          if has "main.c" then Just ("main.c", CKind) else Nothing,
          if has "main.tf" then Just ("main.tf", TerraformKind) else Nothing,
          if has "ms.tex" && not (has "main.py") then Just ("ms.tex", LatexKind) else Nothing,
          if hasAny "Cargo.toml" then Nothing else if not (has "main.c") && not (has "Main.hs") && not (has "main.py") && not (has "index.html") && not (has "main.tf") && not (has "ms.tex") then Just ("binary-layout", BinaryReleaseKind) else Nothing
        ]
detectKind :: [(String, ProjectKind)] -> ProjectKind
detectKind markers =
  case [kind | (_, kind) <- markers, kind /= BinaryReleaseKind] of
    [kind] -> kind
    [] ->
      if any ((== BinaryReleaseKind) . snd) markers
        then BinaryReleaseKind
        else UnknownKind
    _ -> UnknownKind
packageKindProblems :: PackageInfo -> [String]
packageKindProblems pkgInfo =
  [ packageRootPath pkgInfo
      ++ ": has ambiguous project markers: "
      ++ intercalate ", " (matchedMarkers pkgInfo)
  | length (matchedMarkers pkgInfo) > 1
  ]
allowedPatternsForKind :: FilePath -> FilePath -> ProjectKind -> [String]
allowedPatternsForKind pkgRoot pkgName kind =
  let base = ["^" ++ pkgRoot ++ "/default\\.nix$", "^" ++ pkgRoot ++ "/\\.gitignore$"]
      add patterns = base ++ patterns
   in case kind of
        HaskellKind -> add ["^" ++ pkgRoot ++ "/Main\\.hs$", "^" ++ pkgRoot ++ "/" ++ pkgName ++ "\\.cabal$"]
        RustKind -> add ["^" ++ pkgRoot ++ "/Cargo\\.toml$", "^" ++ pkgRoot ++ "/Cargo\\.lock$", "^" ++ pkgRoot ++ "/src/main\\.rs$"]
        HtmlKind -> add ["^" ++ pkgRoot ++ "/index\\.html$", "^" ++ pkgRoot ++ "/script\\.js$", "^" ++ pkgRoot ++ "/style\\.css$"]
        PythonLatexKind -> add ["^" ++ pkgRoot ++ "/main\\.py$", "^" ++ pkgRoot ++ "/ms\\.tex$", "^" ++ pkgRoot ++ "/ms\\.bib$", "^" ++ pkgRoot ++ "/refs\\.bib$", "^" ++ pkgRoot ++ "/figures(/.*)?$"]
        PythonKind -> add ["^" ++ pkgRoot ++ "/main\\.py$"]
        CKind -> add ["^" ++ pkgRoot ++ "/main\\.c$"]
        TerraformKind -> add ["^" ++ pkgRoot ++ "/main\\.tf$", "^" ++ pkgRoot ++ "/\\.terraform(/.*)?$", "^" ++ pkgRoot ++ "/\\.terraform\\.lock\\.hcl$"]
        LatexKind -> add ["^" ++ pkgRoot ++ "/ms\\.tex$", "^" ++ pkgRoot ++ "/ms\\.bib$"]
        BinaryReleaseKind -> base
        UnknownKind -> base
collectRepoPaths :: FilePath -> IO [FilePath]
collectRepoPaths root = do
  children <- listDirectory root
  let childPaths = sort [root </> child | child <- children]
  keptChildren <- fmap catMaybes $
    forM childPaths $ \childPath -> do
      isDir <- doesDirectoryExist childPath
      let relativeChildPath = toRelativePath childPath
      case (isDir, shouldTraverseDirectory relativeChildPath) of
        (True, True) -> Just <$> collectRepoPaths childPath
        (True, False) -> pure Nothing
        (False, _) -> pure (Just [relativeChildPath])
  pure (toRelativePath root : concat keptChildren)
toRelativePath :: FilePath -> FilePath
toRelativePath "." = "."
toRelativePath path =
  case splitDirectories path of
    "." : rest -> foldl1 (</>) rest
    segments -> foldl1 (</>) segments
shouldTraverseDirectory :: FilePath -> Bool
shouldTraverseDirectory path =
  not
    ( any
        (`elem` ["tmp", "prm", "target", "result", ".agents", ".codex"])
        (splitDirectories path)
    )
isLeafPath :: [FilePath] -> FilePath -> Bool
isLeafPath allPaths path =
  let children = [candidate | candidate <- allPaths, takeDirectory candidate == path]
   in null children
pathMatches :: String -> FilePath -> Bool
pathMatches regex path = path =~ regex
packageRoot :: FilePath -> Maybe FilePath
packageRoot path =
  case splitDirectories path of
    "packages" : packageName : _ -> Just ("packages" </> packageName)
    _ -> Nothing
hostRoot :: FilePath -> Maybe FilePath
hostRoot path =
  case splitDirectories path of
    "hosts" : hostDirName : _ -> Just ("hosts" </> hostDirName)
    _ -> Nothing
listPackageNames :: IO [FilePath]
listPackageNames = listSubdirectories "packages"
listHostNames :: IO [FilePath]
listHostNames = listSubdirectories "hosts"
listSubdirectories :: FilePath -> IO [FilePath]
listSubdirectories root = do
  entries <- listDirectory root
  flags <- forM entries $ \name -> doesDirectoryExist (root </> name)
  pure $ sort [name | (name, isDir) <- zip entries flags, isDir]
buildHostCheck :: [String] -> FilePath -> HostCheck
buildHostCheck allStructureIssues currentHostName =
  let scopedIssues = [issue | issue <- allStructureIssues, ("hosts/" ++ currentHostName) `isPrefixOf` issue]
      configStatus = statusFromIssues scopedIssues
      configTest =
        TestResult
          { testName = "configuration.nix",
            testStatus = configStatus,
            testCases = [TestCaseResult "contains configuration.nix" configStatus (if configStatus == Failed then scopedIssues else [])]
          }
   in HostCheck
        { hostName = currentHostName,
          hostTests = [configTest],
          hostErrors = scopedIssues
        }
checkPackage :: [String] -> FilePath -> IO PackageCheck
checkPackage allStructureIssues currentPackageName = do
  let packageDefault = "packages" </> currentPackageName </> "default.nix"
      scopedStructureIssues =
        [ issue
        | issue <- allStructureIssues,
          ("packages/" ++ currentPackageName) `isPrefixOf` issue
        ]
  projectKind <- detectProjectKindForPackage currentPackageName
  exists <- doesFileExist packageDefault
  (templateIssues, _) <-
    if not exists
      then pure ([], Nothing)
      else do
        packageContents <- TIO.readFile packageDefault
        inferredTemplate <- inferTemplateName currentPackageName (T.unpack packageContents)
        case inferredTemplate of
          Nothing ->
            pure
              ( [ "packages/" ++ currentPackageName ++ "/default.nix: could not infer corresponding template"
                ],
                Nothing
              )
          Just inferredTemplateName -> do
            case templateSpecByName inferredTemplateName of
              Nothing ->
                pure
                  ( [ "packages/" ++ currentPackageName ++ "/default.nix: unsupported template " ++ inferredTemplateName
                    ],
                    Just inferredTemplateName
                  )
              Just spec ->
                case embeddedBaseline spec of
                  Just templateContents ->
                    do
                      let allowedKeysForPackage =
                            if currentPackageName == "c_template" && inferredTemplateName == "c_template"
                              then defaultAllowedKeys
                              else allowedDifferenceKeys spec
                      issues <- compareWithTemplate currentPackageName packageDefault ("packages" </> inferredTemplateName </> "default.nix") allowedKeysForPackage (Just templateContents)
                      pure (issues, Just inferredTemplateName)
                  Nothing ->
                    pure
                      ( [ "packages/"
                            ++ currentPackageName
                            ++ "/default.nix: internal error: missing embedded baseline for template "
                            ++ inferredTemplateName
                        ],
                        Just inferredTemplateName
                      )
  cargoIssues <- checkCargoToml currentPackageName
  cabalIssues <- checkCabalFile currentPackageName
  pythonDebugIssues <- checkPythonDebugUnittest currentPackageName projectKind
  haskellDebugIssues <- checkHaskellDebugTests currentPackageName projectKind
  rustDebugIssues <- checkRustDebugTests currentPackageName projectKind
  pythonUnitTestNames <- discoverPythonUnitTestNames currentPackageName projectKind
  haskellUnitTestNames <- discoverHaskellUnitTestNames currentPackageName projectKind
  rustUnitTestNames <- discoverRustUnitTestNames currentPackageName projectKind
  let cargoStatus = statusFromIssues cargoIssues
      cabalStatus = statusFromIssues cabalIssues
      defaultNixIssues =
        [issue | issue <- scopedStructureIssues, "/default.nix" `isInfixOf` issue]
          ++ templateIssues
      defaultNixStatus =
        if exists && null defaultNixIssues
          then Passed
          else Failed
      pythonStatus = if projectKind `elem` [PythonKind, PythonLatexKind] then statusFromIssues pythonDebugIssues else Skipped
      haskellStatus = if projectKind == HaskellKind then statusFromIssues haskellDebugIssues else Skipped
      rustStatus = if projectKind == RustKind then statusFromIssues rustDebugIssues else Skipped
      baseTests =
        [ TestResult
            "directory structure"
            (statusFromIssues scopedStructureIssues)
            [],
          TestResult
            "default.nix"
            defaultNixStatus
            []
        ]
      languageSpecificTests =
        concat
          [ if projectKind == RustKind
              then
                [ TestResult
                    "Cargo.toml"
                    cargoStatus
                    [ TestCaseResult
                        "matches Cargo.toml conventions"
                        cargoStatus
                        (if cargoStatus == Failed then cargoIssues else [])
                    ],
                  TestResult
                    "src/main.rs"
                    rustStatus
                    ( TestCaseResult
                        "supports DEBUG test execution"
                        rustStatus
                        (if rustStatus == Failed then rustDebugIssues else [])
                        : [TestCaseResult (formatDiscoveredUnitTestName name) Skipped [] | name <- rustUnitTestNames]
                    )
                ]
              else [],
            if projectKind == HaskellKind
              then
                [ TestResult
                    (currentPackageName ++ ".cabal")
                    cabalStatus
                    [ TestCaseResult
                        "matches Cabal conventions"
                        cabalStatus
                        (if cabalStatus == Failed then cabalIssues else [])
                    ],
                  TestResult
                    "Main.hs"
                    haskellStatus
                    ( TestCaseResult
                        "supports DEBUG test execution"
                        haskellStatus
                        (if haskellStatus == Failed then haskellDebugIssues else [])
                        : [ TestCaseResult
                              (formatDiscoveredUnitTestName testName')
                              Skipped
                              []
                          | testName' <- if null haskellUnitTestNames then ["no named HUnit test labels discovered"] else haskellUnitTestNames
                          ]
                    )
                ]
              else [],
            [ TestResult
                "main.py"
                pythonStatus
                ( TestCaseResult
                    "supports DEBUG test execution"
                    pythonStatus
                    (if pythonStatus == Failed then pythonDebugIssues else [])
                    : [TestCaseResult (formatDiscoveredUnitTestName name) Skipped [] | name <- pythonUnitTestNames]
                )
            | projectKind `elem` [PythonKind, PythonLatexKind]
            ]
          ]
      tests = baseTests ++ languageSpecificTests
      allIssues =
        scopedStructureIssues
          ++ templateIssues
          ++ cargoIssues
          ++ cabalIssues
          ++ pythonDebugIssues
          ++ haskellDebugIssues
          ++ rustDebugIssues
  pure
    PackageCheck
      { packageNodeName = currentPackageName,
        packageKind = projectKind,
        packageTests = tests,
        packageErrors = allIssues
      }
detectProjectKindForPackage :: FilePath -> IO ProjectKind
detectProjectKindForPackage packageName = do
  let pkgRoot = "packages" </> packageName
      has rel = doesFileExist (pkgRoot </> rel)
  hasMainHs <- has "Main.hs"
  hasCargoToml <- has "Cargo.toml"
  hasIndexHtml <- has "index.html"
  hasMainPy <- has "main.py"
  hasMsTex <- has "ms.tex"
  hasMainC <- has "main.c"
  hasMainTf <- has "main.tf"
  let kind
        | hasMainHs = HaskellKind
        | hasCargoToml = RustKind
        | hasIndexHtml = HtmlKind
        | hasMainPy && hasMsTex = PythonLatexKind
        | hasMainPy = PythonKind
        | hasMainC = CKind
        | hasMainTf = TerraformKind
        | hasMsTex = LatexKind
        | otherwise = BinaryReleaseKind
  pure kind
readTextFileIfExists :: FilePath -> IO (Maybe T.Text)
readTextFileIfExists path = do
  exists <- doesFileExist path
  if exists then Just <$> TIO.readFile path else pure Nothing
checkPythonDebugUnittest :: FilePath -> ProjectKind -> IO [String]
checkPythonDebugUnittest packageName projectKind =
  if projectKind `notElem` [PythonKind, PythonLatexKind]
    then pure []
    else do
      let mainPyPath = "packages" </> packageName </> "main.py"
      maybeMainPy <- readTextFileIfExists mainPyPath
      case maybeMainPy of
        Nothing -> pure []
        Just _ -> do
          python3Path <- findExecutable "python3"
          pythonPath <- findExecutable "python"
          case python3Path <|> pythonPath of
            Nothing ->
              pure
                [ "packages/"
                    ++ packageName
                    ++ "/main.py: python interpreter not found (tried python3, python)"
                ]
            Just pythonCommand -> do
              (exitCode, stdoutText, stderrText) <- readProcessWithExitCode pythonCommand ["-c", pythonDebugUnittestValidator, mainPyPath] ""
              let rawLines = lines stdoutText
                  errorCodes = [drop 4 line | line <- rawLines, "ERR " `isPrefixOf` line]
                  mappedErrors = map (mapPythonValidatorError packageName) errorCodes
              case exitCode of
                ExitSuccess ->
                  if "OK" `elem` rawLines
                    then pure []
                    else
                      pure
                        [ "packages/" ++ packageName ++ "/main.py: python validator produced unexpected output"
                        ]
                ExitFailure 1 -> pure mappedErrors
                ExitFailure _ ->
                  pure
                    [ "packages/"
                        ++ packageName
                        ++ "/main.py: python AST validator execution failed: "
                        ++ oneLine (T.pack stderrText)
                    ]
discoverPythonUnitTestNames :: FilePath -> ProjectKind -> IO [String]
discoverPythonUnitTestNames packageName projectKind =
  if projectKind `notElem` [PythonKind, PythonLatexKind]
    then pure []
    else do
      let mainPyPath = "packages" </> packageName </> "main.py"
      maybeContent <- readTextFileIfExists mainPyPath
      case maybeContent of
        Nothing -> pure []
        Just content -> do
          let extracted =
                [ fnName
                | rawLine <- lines (T.unpack content),
                  Just fnName <- [extractPythonTestName rawLine]
                ]
          pure (sort (Set.toList (Set.fromList extracted)))
formatDiscoveredUnitTestName :: String -> String
formatDiscoveredUnitTestName name = "unit test: " ++ name
extractPythonTestName :: String -> Maybe String
extractPythonTestName rawLine =
  let trimmed = dropWhile (== ' ') rawLine
      defPrefix :: String
      defPrefix = "def test_"
   in if defPrefix `isPrefixOf` trimmed
        then
          let namePortion = takeWhile (\ch -> ch /= '(' && ch /= ' ' && ch /= ':') (drop 4 trimmed)
           in if null namePortion then Nothing else Just namePortion
        else Nothing
checkHaskellDebugTests :: FilePath -> ProjectKind -> IO [String]
checkHaskellDebugTests packageName projectKind =
  if projectKind /= HaskellKind
    then pure []
    else do
      let mainHsPath = "packages" </> packageName </> "Main.hs"
      mainExists <- doesFileExist mainHsPath
      if not mainExists
        then pure []
        else do
          content <- TIO.readFile mainHsPath
          let source = T.unpack content
              hasDebugEnvGate = "lookupEnv \"DEBUG\"" `isInfixOf` source
              hasTestRunner = "runTestTT" `isInfixOf` source || "runDebugTests" `isInfixOf` source
              hasMainDebugBranch = "Just \"1\" ->" `isInfixOf` source
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
                  else Just ("packages/" ++ packageName ++ "/Main.hs: DEBUG branch must run HUnit tests (runTestTT)")
              ]
discoverHaskellUnitTestNames :: FilePath -> ProjectKind -> IO [String]
discoverHaskellUnitTestNames packageName projectKind =
  if projectKind /= HaskellKind
    then pure []
    else do
      let mainHsPath = "packages" </> packageName </> "Main.hs"
      maybeContent <- readTextFileIfExists mainHsPath
      case maybeContent of
        Nothing -> pure []
        Just content -> do
          let sourceLines = lines (T.unpack content)
              labelsFromFormattingHelper = extractMakeFormattingTestLabels sourceLines
              labelsFromTilde =
                [ label
                | rawLine <- sourceLines,
                  Just label <- [extractHaskellTestLabel rawLine]
                ]
              labelsFromAssertEqual = extractAssertEqualLabels sourceLines
              fallbackCaseNames =
                [ "unnamed HUnit test case #" ++ show i
                | i <- [1 .. length [() | line <- sourceLines, "TestCase" `isInfixOf` line]]
                ]
              discovered =
                if null labelsFromFormattingHelper && null labelsFromTilde && null labelsFromAssertEqual
                  then fallbackCaseNames
                  else labelsFromFormattingHelper ++ labelsFromTilde ++ labelsFromAssertEqual
          pure (sort (Set.toList (Set.fromList discovered)))
extractHaskellTestLabel :: String -> Maybe String
extractHaskellTestLabel rawLine =
  case breakOnSubstring "~:" rawLine of
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
lastQuotedToken source =
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
      tokens = go source Nothing "" []
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
          case firstQuotedToken line of
            Just label -> label : go False rest
            Nothing -> go True rest
      | otherwise = go False rest
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
firstQuotedToken source =
  let afterDouble = dropWhile (/= '"') source
      afterSingle = dropWhile (/= '\'') source
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
checkRustDebugTests :: FilePath -> ProjectKind -> IO [String]
checkRustDebugTests packageName projectKind =
  if projectKind /= RustKind
    then pure []
    else do
      let mainRsPath = "packages" </> packageName </> "src/main.rs"
          defaultNixPath = "packages" </> packageName </> "default.nix"
      mainExists <- doesFileExist mainRsPath
      defaultNixExists <- doesFileExist defaultNixPath
      mainSource <-
        if mainExists
          then T.unpack <$> TIO.readFile mainRsPath
          else pure ""
      defaultNixSource <-
        if defaultNixExists
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
discoverRustUnitTestNames :: FilePath -> ProjectKind -> IO [String]
discoverRustUnitTestNames packageName projectKind =
  if projectKind /= RustKind
    then pure []
    else do
      let mainRsPath = "packages" </> packageName </> "src/main.rs"
      maybeContent <- readTextFileIfExists mainRsPath
      case maybeContent of
        Nothing -> pure []
        Just content -> pure (extractRustTests (lines (T.unpack content)))
extractRustTests :: [String] -> [String]
extractRustTests rawLines = sort (Set.toList (Set.fromList (go False rawLines)))
  where
    go _ [] = []
    go awaitingFn (line : rest) =
      let trimmed = dropWhile (== ' ') line
       in if "#[test]" `isPrefixOf` trimmed
            then go True rest
            else
              if awaitingFn && "fn " `isPrefixOf` trimmed
                then
                  let fnName = takeWhile (\ch -> ch /= '(' && ch /= ' ') (drop 3 trimmed)
                   in [fnName | not (null fnName)] ++ go False rest
                else go False rest
mapPythonValidatorError :: FilePath -> String -> String
mapPythonValidatorError packageName errorCode =
  let prefix = "packages/" ++ packageName ++ "/main.py: "
   in case errorCode of
        "missing_main_function" -> prefix ++ "missing main() function"
        "missing_debug_gate" -> prefix ++ "main() must include a DEBUG gate"
        "debug_branch_no_unittest" -> prefix ++ "DEBUG=true branch in main() must run unittest"
        "run_tests_missing_unittest" -> prefix ++ "run_tests() is called from DEBUG branch but does not run unittest"
        "parse_error" -> prefix ++ "python source could not be parsed"
        _ -> prefix ++ "python validator failed with error code: " ++ errorCode
pythonDebugUnittestValidator :: String
pythonDebugUnittestValidator =
  unlines
    [ "import ast",
      "import sys",
      "",
      "def _is_os_getenv_debug(node):",
      "    if not isinstance(node, ast.Call):",
      "        return False",
      "    func = node.func",
      "    if not isinstance(func, ast.Attribute):",
      "        return False",
      "    if isinstance(func.value, ast.Name) and func.value.id == 'os' and func.attr == 'getenv':",
      "        if not node.args:",
      "            return False",
      "        first = node.args[0]",
      "        return isinstance(first, ast.Constant) and first.value == 'DEBUG'",
      "    if isinstance(func.value, ast.Attribute) and func.attr == 'get':",
      "        base = func.value",
      "        if isinstance(base.value, ast.Name) and base.value.id == 'os' and base.attr == 'environ':",
      "            if not node.args:",
      "                return False",
      "            first = node.args[0]",
      "            return isinstance(first, ast.Constant) and first.value == 'DEBUG'",
      "    return False",
      "",
      "def _contains_debug_gate(expr):",
      "    return any(_is_os_getenv_debug(n) for n in ast.walk(expr))",
      "",
      "def _is_unittest_main_call(node):",
      "    if not isinstance(node, ast.Call):",
      "        return False",
      "    func = node.func",
      "    return isinstance(func, ast.Attribute) and isinstance(func.value, ast.Name) and func.value.id == 'unittest' and func.attr == 'main'",
      "",
      "def _contains_unittest_runner(statements):",
      "    for statement in statements:",
      "        for node in ast.walk(statement):",
      "            if not isinstance(node, ast.Call):",
      "                continue",
      "            func = node.func",
      "            if isinstance(func, ast.Attribute) and isinstance(func.value, ast.Name) and func.value.id == 'unittest':",
      "                if func.attr in {'main', 'TextTestRunner', 'defaultTestLoader'}:",
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
      "        main_fn = functions['main']",
      "        debug_if_nodes = [n for n in ast.walk(main_fn) if isinstance(n, ast.If) and _contains_debug_gate(n.test)]",
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
  let cargoPath = "packages" </> packageName </> "Cargo.toml"
  cargoExists <- doesFileExist cargoPath
  if not cargoExists
    then pure []
    else do
      cargoContents <- TIO.readFile cargoPath
      let packageSection = extractTomlSection "package" cargoContents
          lintsRustSection = extractTomlSection "lints.rust" cargoContents
          packageNameValue = lookupTomlString "name" packageSection
          unsafeCodeLint = lookupTomlString "unsafe_code" lintsRustSection
          normalizedCargo = normalizeCargoTomlForTightness packageName cargoContents
          normalizedTemplateCargo = normalizeCargoTomlForTightness packageName rustCargoTightnessBaseline
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
              else Just ("packages/" ++ packageName ++ "/Cargo.toml: require [lints.rust].unsafe_code = \"forbid\""),
            if normalizedCargo == normalizedTemplateCargo
              then Nothing
              else
                Just
                  ( "packages/"
                      ++ packageName
                      ++ "/Cargo.toml: only dependency sections may differ from the internal Rust Cargo baseline"
                  )
          ]
normalizeCargoTomlForTightness :: FilePath -> T.Text -> T.Text
normalizeCargoTomlForTightness packageName contents =
  let step (currentHeader, acc) rawLine =
        let stripped = T.strip rawLine
         in if isTomlSectionHeader stripped
              then
                if isDependencyHeader stripped
                  then (Just stripped, acc)
                  else (Just stripped, acc ++ [stripped])
              else case currentHeader of
                Just header | isDependencyHeader header -> (currentHeader, acc)
                _ | T.null stripped -> (currentHeader, acc)
                Just "[package]" | isNameLine stripped -> (currentHeader, acc ++ [nameLine])
                Just "[[bin]]" | isNameLine stripped -> (currentHeader, acc ++ [nameLine])
                _ -> (currentHeader, acc ++ [stripped])
      (_, normalizedLines) = foldl' step (Nothing, []) (T.lines contents)
      nameLine = "name = \"" <> T.pack packageName <> "\""
   in T.unlines normalizedLines
isDependencyHeader :: T.Text -> Bool
isDependencyHeader stripped =
  stripped == "[dependencies]"
    || stripped == "[dev-dependencies]"
    || stripped == "[build-dependencies]"
    || isTargetDependenciesHeader stripped
isTargetDependenciesHeader :: T.Text -> Bool
isTargetDependenciesHeader stripped =
  T.isPrefixOf "[target." stripped
    && T.isSuffixOf ".dependencies]" stripped
isNameLine :: T.Text -> Bool
isNameLine stripped = "name = \"" `T.isPrefixOf` stripped
extractTomlSection :: T.Text -> T.Text -> T.Text
extractTomlSection sectionName contents =
  let sectionHeader = "[" <> sectionName <> "]"
      linesOfFile = T.lines contents
      sectionStart = dropWhile (\line -> T.strip line /= sectionHeader) linesOfFile
      sectionBody = drop 1 sectionStart
   in T.unlines (takeWhile (not . isTomlSectionHeader . T.strip) sectionBody)
isTomlSectionHeader :: T.Text -> Bool
isTomlSectionHeader line =
  T.length line >= 3 && T.head line == '[' && T.last line == ']'
lookupTomlString :: T.Text -> T.Text -> Maybe T.Text
lookupTomlString key sectionContents =
  let keyPrefix = key <> " = "
      maybeLine = listToMaybe [T.strip line | line <- T.lines sectionContents, keyPrefix `T.isPrefixOf` T.strip line]
   in do
        line <- maybeLine
        value <- T.stripPrefix keyPrefix line
        T.stripPrefix "\"" value >>= T.stripSuffix "\""
checkCabalFile :: FilePath -> IO [String]
checkCabalFile packageName = do
  let cabalPath = "packages" </> packageName </> packageName <.> "cabal"
  cabalExists <- doesFileExist cabalPath
  if not cabalExists
    then pure []
    else do
      cabalContents <- TIO.readFile cabalPath
      let normalizedCabal = normalizeCabalForTightness packageName cabalContents
          normalizedTemplate = normalizeCabalForTightness packageName haskellCabalTightnessBaseline
          cabalName = lookupCabalField "name" cabalContents
      pure $
        catMaybes
          [ if cabalName == Just (T.pack packageName)
              then Nothing
              else Just ("packages/" ++ packageName ++ "/" ++ packageName ++ ".cabal: name must match directory name"),
            if normalizedCabal == normalizedTemplate
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
normalizeCabalForTightness packageName contents =
  let step (inBuildDepends, acc) rawLine =
        let stripped = T.strip rawLine
            normalized = normalizeCabalLine packageName stripped
         in if inBuildDepends
              then
                if T.null stripped
                  then (True, acc)
                  else case T.breakOn ":" stripped of
                    (_, "") -> (True, acc)
                    _ -> (False, acc ++ [normalized])
              else
                if "build-depends:" `T.isPrefixOf` stripped
                  then (True, acc)
                  else
                    if T.null stripped
                      then (False, acc)
                      else (False, acc ++ [normalized])
      (_, normalizedLines) = foldl' step (False, []) (T.lines contents)
   in T.unlines normalizedLines
normalizeCabalLine :: FilePath -> T.Text -> T.Text
normalizeCabalLine packageName stripped
  | "name:" `T.isPrefixOf` stripped = "name:          " <> T.pack packageName
  | "executable " `T.isPrefixOf` stripped = "executable " <> T.pack packageName
  | otherwise = stripped
lookupCabalField :: T.Text -> T.Text -> Maybe T.Text
lookupCabalField field contents =
  let fieldPrefix = field <> ":"
      maybeLine = listToMaybe [T.strip line | line <- T.lines contents, fieldPrefix `T.isPrefixOf` T.strip line]
   in do
        line <- maybeLine
        value <- T.stripPrefix fieldPrefix line
        pure (T.strip value)
compareWithTemplate :: FilePath -> FilePath -> FilePath -> Set.Set T.Text -> Maybe T.Text -> IO [String]
compareWithTemplate packageName packageDefault templateDefault allowedKeys templateOverrideContents = do
  packageExpr <- parseNixExprFromFile packageDefault
  templateExpr <-
    case templateOverrideContents of
      Just contents -> parseNixExprFromText contents
      Nothing -> parseNixExprFromFile templateDefault
  case (packageExpr, templateExpr) of
    (Left parseError, _) ->
      pure ["packages/" ++ packageName ++ "/default.nix: parse error: " ++ show parseError]
    (_, Left parseError) ->
      pure [templateDefault ++ ": parse error: " ++ show parseError]
    (Right pkg, Right tmpl) ->
      let normalizedPackage = normalizeExpr allowedKeys pkg
          normalizedTemplate = normalizeExpr allowedKeys tmpl
       in pure $
            formatDifferences
              packageName
              templateDefault
              normalizedPackage
              normalizedTemplate
parseNixExprFromText :: T.Text -> IO (Either String NExprLoc)
parseNixExprFromText contents = do
  (tempPath, handle) <- openTempFile "/tmp" "check-repository-template-override.nix"
  TIO.hPutStr handle contents
  hClose handle
  parseNixExprFromFile tempPath
    `finally` removeFileIfExists tempPath
parseNixExprFromFile :: FilePath -> IO (Either String NExprLoc)
parseNixExprFromFile path =
  fmap (either (Left . show) Right) (parseNixFileLoc (Path path))
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)
inferTemplateName :: FilePath -> String -> IO (Maybe FilePath)
inferTemplateName packageName content = do
  matches <- forM templateSpecs $ \spec -> do
    matched <- matchesTemplate spec packageName content
    pure (if matched then Just (templateName spec) else Nothing)
  pure (listToMaybe (catMaybes matches))
normalizeExpr :: Set.Set T.Text -> NExprLoc -> NExprLoc
normalizeExpr allowedKeys (Fix (Compose (AnnUnit exprSpan exprF))) =
  let rebuiltExpr = case exprF of
        NSet rec bindings -> NSet rec (normalizeBindings allowedKeys bindings)
        NLet bindings body -> NLet (normalizeBindings allowedKeys bindings) (normalizeExpr allowedKeys body)
        NAbs (ParamSet p1 p2 ps) body -> NAbs (ParamSet p1 p2 (sortParams ps)) (normalizeExpr allowedKeys body)
        NAbs (Param p) body -> NAbs (Param p) (normalizeExpr allowedKeys body)
        otherExpr -> fmap (normalizeExpr allowedKeys) otherExpr
   in Fix (Compose (AnnUnit exprSpan rebuiltExpr))
sortParams :: [(VarName, Maybe NExprLoc)] -> [(VarName, Maybe NExprLoc)]
sortParams = sortBy (\(VarName a, _) (VarName b, _) -> compare a b)
normalizeBindings :: Set.Set T.Text -> [Binding NExprLoc] -> [Binding NExprLoc]
normalizeBindings allowedKeys bindings =
  [normalizeBinding allowedKeys binding | binding <- bindings, not (isAllowedDifferenceBinding allowedKeys binding)]
normalizeBinding :: Set.Set T.Text -> Binding NExprLoc -> Binding NExprLoc
normalizeBinding allowedKeys = \case
  NamedVar keyPath value pos -> NamedVar keyPath (normalizeExpr allowedKeys value) pos
  Inherit maybeExpr names pos -> Inherit (normalizeExpr allowedKeys <$> maybeExpr) names pos
isAllowedDifferenceBinding :: Set.Set T.Text -> Binding NExprLoc -> Bool
isAllowedDifferenceBinding allowedKeys = \case
  NamedVar (key :| _) _ _ ->
    case keyNameText key of
      Just keyText -> Set.member keyText allowedKeys
      Nothing -> False
  _ -> False
keyNameText :: NKeyName NExprLoc -> Maybe T.Text
keyNameText = \case
  StaticKey (VarName keyText) -> Just keyText
  DynamicKey (Plain (DoubleQuoted [Plain keyText])) -> Just keyText
  _ -> Nothing
renderExpr :: NExprLoc -> T.Text
renderExpr =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyNix
    . stripAnnotation
formatDifferences :: FilePath -> FilePath -> NExprLoc -> NExprLoc -> [String]
formatDifferences packageName templateDefault packageExpr templateExpr =
  let renderedPackage = renderExpr packageExpr
      renderedTemplate = renderExpr templateExpr
   in if renderedPackage == renderedTemplate
        then []
        else
          let packageBindings = extractPrimaryBindings packageExpr
              templateBindings = extractPrimaryBindings templateExpr
           in case (packageBindings, templateBindings) of
                (Just pkgMap, Just tmplMap) ->
                  let missingKeys = Map.keys (Map.difference tmplMap pkgMap)
                      unexpectedKeys = Map.keys (Map.difference pkgMap tmplMap)
                      sharedKeys = Map.keys (Map.intersection pkgMap tmplMap)
                      changedKeys =
                        [ key
                        | key <- sharedKeys,
                          Map.lookup key pkgMap /= Map.lookup key tmplMap
                        ]
                      detailLines =
                        map (issueLine "missing key") missingKeys
                          ++ map (issueLine "unexpected key") unexpectedKeys
                          ++ map
                            ( \key ->
                                let expectedValue = oneLine (fromMaybe "" (Map.lookup key tmplMap))
                                    actualValue = oneLine (fromMaybe "" (Map.lookup key pkgMap))
                                 in "  - changed key: "
                                      ++ T.unpack key
                                      ++ "\n    expected: "
                                      ++ expectedValue
                                      ++ "\n    actual:   "
                                      ++ actualValue
                            )
                            changedKeys
                   in if null detailLines
                        then
                          [ "packages/"
                              ++ packageName
                              ++ "/default.nix: differs from template "
                              ++ templateDefault
                              ++ " (excluding dependency keys)"
                          ]
                        else
                          [ "packages/"
                              ++ packageName
                              ++ "/default.nix: differs from template "
                              ++ templateDefault
                              ++ " (excluding dependency keys)\n"
                              ++ intercalate "\n" detailLines
                          ]
                _ ->
                  [ "packages/"
                      ++ packageName
                      ++ "/default.nix: differs from template "
                      ++ templateDefault
                      ++ " (excluding dependency keys)"
                  ]
issueLine :: String -> T.Text -> String
issueLine issue key = "  - " ++ issue ++ ": " ++ T.unpack key
oneLine :: T.Text -> String
oneLine value =
  let compact = T.unwords (T.words value)
   in T.unpack compact
extractPrimaryBindings :: NExprLoc -> Maybe (Map.Map T.Text T.Text)
extractPrimaryBindings expr = do
  bindingGroups <- collectSetBindings expr
  pure $ Map.fromList (maximumByLength bindingGroups)
maximumByLength :: [[a]] -> [a]
maximumByLength = maximumBy (comparing length)
collectSetBindings :: NExprLoc -> Maybe [[(T.Text, T.Text)]]
collectSetBindings (Fix (Compose (AnnUnit _ exprF))) =
  case exprF of
    NSet _ bindings ->
      let current = extractBindings bindings
          nested = concatMap collectFromBinding bindings
       in Just (current : nested)
    NLet bindings body ->
      let nestedFromBindings = concatMap collectFromBinding bindings
          nestedFromBody = fromMaybe [] (collectSetBindings body)
       in Just (nestedFromBindings ++ nestedFromBody)
    NAbs _ body -> collectSetBindings body
    otherExpr ->
      Just (concatMap (fromMaybe [] . collectSetBindings) otherExpr)
collectFromBinding :: Binding NExprLoc -> [[(T.Text, T.Text)]]
collectFromBinding (NamedVar _ value _) = fromMaybe [] (collectSetBindings value)
collectFromBinding (Inherit maybeExpr _ _) = maybe [] (fromMaybe [] . collectSetBindings) maybeExpr
extractBindings :: [Binding NExprLoc] -> [(T.Text, T.Text)]
extractBindings bindings =
  [ (T.intercalate "." (mapMaybe keyNameText (NE.toList keyPath)), renderExpr value)
  | NamedVar keyPath value _ <- bindings
  ]
runDebugTests :: IO ()
runDebugTests = do
  counts <- runTestTT debugTests
  if errors counts == 0 && failures counts == 0
    then putStrLn "test ... ok"
    else exitFailure
debugTests :: Test
debugTests =
  TestList
    [ TestCase $ do
        inferred <- inferTemplateName "test" uncommentFixture
        assertEqual
          "infers uncomment template"
          (Just "uncomment_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" rustFixture
        assertEqual
          "infers rust package template"
          (Just "rust_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" haskellFixture
        assertEqual
          "infers haskell package template"
          (Just "haskell_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonFixture
        assertEqual
          "infers python template"
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonRemoteFixture
        assertEqual
          "infers python remote template"
          (Just "python_remote_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" binaryReleaseFixture
        assertEqual
          "infers binary release template"
          (Just "binary_release_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonLatexFixture
        assertEqual
          "infers python template for python-latex fixture"
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "deploy_host_template" deployHostFixture
        assertEqual
          "infers deploy host template"
          (Just "deploy_host_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" cFixture
        assertEqual
          "infers c template"
          (Just "c_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" latexFixture
        assertEqual
          "infers latex template"
          (Just "latex_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" htmlFixture
        assertEqual
          "infers html template"
          (Just "html_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" unknownFixture
        assertEqual
          "returns Nothing for unknown template"
          Nothing
          inferred,
      TestCase $ do
        assertEqual
          "oneLine compacts whitespace"
          "a b c"
          (oneLine " a \n  b\t c "),
      TestCase $ do
        assertEqual
          "issueLine formats issue details"
          "  - missing key: src"
          (issueLine "missing key" "src"),
      TestCase $ do
        assertEqual
          "extractTomlSection extracts package section"
          "name = \"example-package\"\nversion = \"0.1.0\"\nedition = \"2021\"\ndescription = \"Example package fixture for TOML parsing.\"\nlicense = \"MIT\"\nrepository = \"https://github.com/pbizopoulos/canonicalization\"\nreadme = \"../../README\"\nkeywords = [\"check\", \"lint\", \"fixture\"]\ncategories = [\"development-tools\"]\n\n"
          (extractTomlSection "package" exampleCargoFixture),
      TestCase $ do
        assertEqual
          "lookupTomlString parses package name"
          (Just "remove-empty-lines")
          (lookupTomlString "name" (extractTomlSection "package" removeEmptyLinesCargoFixture)),
      TestCase $ do
        assertEqual
          "lookupTomlString parses lints.rust.unsafe_code"
          (Just "forbid")
          (lookupTomlString "unsafe_code" (extractTomlSection "lints.rust" removeEmptyLinesCargoFixture))
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
pythonLatexFixture :: String
pythonLatexFixture =
  "{ pkgs ? import <nixpkgs> { }, }:\n"
    ++ "pkgs.python3Packages.buildPythonPackage rec {\n"
    ++ "  installPhase = '' latexmk -cd -pdf tmp/ms.tex '';\n"
    ++ "}\n"
pythonRemoteFixture :: String
pythonRemoteFixture =
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
      "    HOME=\"$(mktemp -d)\" DEBUG=1 PYTHONWARNINGS=error coverage run --source=\"$src\" -m pyinstrument \"$src/main.py\"",
      "    coverage report",
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
      "  nativeInstallCheckInputs = [",
      "    pyPkgs.coverage",
      "    pyPkgs.pyinstrument",
      "  ];",
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
pythonRemoteBaseline :: T.Text
pythonRemoteBaseline =
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
pythonLatexTemplateBaseline :: T.Text
pythonLatexTemplateBaseline =
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
      "    test -x \"$out/bin/${pname}\"",
      "    HOME=\"$(mktemp -d)\" DEBUG=1 PYTHONWARNINGS=error coverage run --source=\"$src\" -m pyinstrument \"$src/main.py\"",
      "    coverage report",
      "    runHook postInstallCheck",
      "  '';",
      "  installPhase = ''",
      "    datadir=\"$out/share/${pname}\"",
      "    install -Dm644 ./main.py ./ms.tex ./ms.bib -t \"$datadir\"",
      "    mkdir -p \"$out/bin\"",
      "    cat > \"$out/bin/${pname}\" <<EOF",
      "    #!/usr/bin/env bash",
      "    set -euo pipefail",
      "    mkdir -p tmp",
      "    ${pythonEnv}/bin/python3 \"$datadir/main.py\"",
      "    cp \"$datadir\"/ms.{tex,bib} tmp/",
      "    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex",
      "    EOF",
      "    chmod +x \"$out/bin/${pname}\"",
      "  '';",
      "  meta.mainProgram = pname;",
      "  nativeInstallCheckInputs = [",
      "    pkgs.python3Packages.coverage",
      "    pkgs.python3Packages.pyinstrument",
      "  ];",
      "  pname = baseNameOf ./.;",
      "  propagatedBuildInputs = [];",
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
