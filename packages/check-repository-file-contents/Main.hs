{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-unsafe #-}
module Main (main) where
import Control.Exception (finally)
import Control.Monad (forM, when)
import Data.Fix (Fix (Fix))
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.List (intercalate, isInfixOf, sort, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO (hClose, openTempFile)
import Test.HUnit (Counts (errors, failures), Test (TestCase, TestList), assertEqual, runTestTT)
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
        allowedDifferenceKeys = defaultAllowedKeys,
        embeddedBaseline = Just haskellTemplateBaseline
      },
    TemplateSpec
      { templateName = "rust_package_baseline",
        matchesTemplate = \_ content -> pure ("rustPlatform.buildRustPackage" `isInfixOf` content),
        allowedDifferenceKeys = defaultAllowedKeys,
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
      let packageRoot = "packages" </> packageName
      hasMsTex <- doesFileExist (packageRoot </> "ms.tex")
      hasRefsBib <- doesFileExist (packageRoot </> "refs.bib")
      hasFiguresDir <- doesDirectoryExist (packageRoot </> "figures")
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
templateSpecByName name = listToMaybe [spec | spec <- templateSpecs, templateName spec == name]
main :: IO ()
main = do
  debug <- lookupEnv "DEBUG"
  case debug of
    Just "1" -> runDebugTests
    _ -> do
      packageNames <- listPackageNames
      issues <- fmap concat (forM packageNames checkPackage)
      if null issues
        then putStrLn "check-repository-file-contents: ok"
        else do
          mapM_ putStrLn issues
          exitFailure
listPackageNames :: IO [FilePath]
listPackageNames = do
  entries <- listDirectory "packages"
  flags <- forM entries $ \name -> doesDirectoryExist ("packages" </> name)
  pure $ sort [name | (name, isDir) <- zip entries flags, isDir]
checkPackage :: FilePath -> IO [String]
checkPackage packageName = do
  let packageDefault = "packages" </> packageName </> "default.nix"
  exists <- doesFileExist packageDefault
  templateIssues <-
    if not exists
      then pure []
      else do
        packageContents <- TIO.readFile packageDefault
        inferredTemplate <- inferTemplateName packageName (T.unpack packageContents)
        case inferredTemplate of
          Nothing ->
            pure
              [ "packages/" ++ packageName ++ "/default.nix: could not infer corresponding template"
              ]
          Just inferredTemplateName -> do
            case templateSpecByName inferredTemplateName of
              Nothing ->
                pure
                  [ "packages/" ++ packageName ++ "/default.nix: unsupported template " ++ inferredTemplateName
                  ]
              Just spec ->
                case embeddedBaseline spec of
                  Just templateContents ->
                    compareWithTemplate packageName packageDefault ("packages" </> inferredTemplateName </> "default.nix") (allowedDifferenceKeys spec) (Just templateContents)
                  Nothing ->
                    pure
                      [ "packages/"
                          ++ packageName
                          ++ "/default.nix: internal error: missing embedded baseline for template "
                          ++ inferredTemplateName
                      ]
  cargoIssues <- checkCargoToml packageName
  cabalIssues <- checkCabalFile packageName
  pure (templateIssues ++ cargoIssues ++ cabalIssues)
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
  bestGroup <- pickLargest bindingGroups
  pure $ Map.fromList bestGroup
pickLargest :: [[(T.Text, T.Text)]] -> Maybe [(T.Text, T.Text)]
pickLargest [] = Nothing
pickLargest groups = Just (foldl1 maxByLength groups)
maxByLength :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
maxByLength left right =
  if length left >= length right
    then left
    else right
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
  [ (T.intercalate "." (mapMaybe keyNameText (toList keyPath)), renderExpr value)
  | NamedVar keyPath value _ <- bindings
  ]
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []
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
          "uncomment template inference"
          (Just "uncomment_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" rustFixture
        assertEqual
          "rust template inference"
          (Just "rust_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" haskellFixture
        assertEqual
          "haskell template inference"
          (Just "haskell_package_baseline")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonFixture
        assertEqual
          "python template inference"
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonRemoteFixture
        assertEqual
          "python remote template inference"
          (Just "python_remote_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" binaryReleaseFixture
        assertEqual
          "binary release structural template inference"
          (Just "binary_release_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" pythonLatexFixture
        assertEqual
          "python latex template inference"
          (Just "python_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "deploy_host_template" deployHostFixture
        assertEqual
          "deploy host template inference"
          (Just "deploy_host_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" cFixture
        assertEqual
          "c template inference"
          (Just "c_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" latexFixture
        assertEqual
          "latex template inference"
          (Just "latex_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" htmlFixture
        assertEqual
          "html template inference"
          (Just "html_template")
          inferred,
      TestCase $ do
        inferred <- inferTemplateName "test" unknownFixture
        assertEqual
          "unknown template inference"
          Nothing
          inferred,
      TestCase $ do
        assertEqual
          "oneLine compacts whitespace"
          "a b c"
          (oneLine " a \n  b\t c "),
      TestCase $ do
        assertEqual
          "issueLine formatting"
          "  - missing key: src"
          (issueLine "missing key" "src"),
      TestCase $ do
        assertEqual
          "extractTomlSection finds package section"
          "name = \"check-repository-directory-structure\"\nversion = \"0.1.0\"\nedition = \"2021\"\ndescription = \"A CLI tool to check the repository directory structure.\"\nlicense = \"MIT\"\nrepository = \"https://github.com/pbizopoulos/canonicalization\"\nreadme = \"../../README\"\nkeywords = [\"check\", \"lint\", \"directory-structure\"]\ncategories = [\"development-tools\"]\n\n"
          (extractTomlSection "package" checkRepositoryDirectoryStructureCargoFixture),
      TestCase $ do
        assertEqual
          "lookupTomlString parses simple key"
          (Just "remove-empty-lines")
          (lookupTomlString "name" (extractTomlSection "package" removeEmptyLinesCargoFixture)),
      TestCase $ do
        assertEqual
          "lookupTomlString parses unsafe_code lint"
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
checkRepositoryDirectoryStructureCargoFixture :: T.Text
checkRepositoryDirectoryStructureCargoFixture =
  T.unlines
    [ "[[bin]]",
      "name = \"check-repository-directory-structure\"",
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
      "name = \"check-repository-directory-structure\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      "description = \"A CLI tool to check the repository directory structure.\"",
      "license = \"MIT\"",
      "repository = \"https://github.com/pbizopoulos/canonicalization\"",
      "readme = \"../../README\"",
      "keywords = [\"check\", \"lint\", \"directory-structure\"]",
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
