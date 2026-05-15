{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-unsafe -Wno-safe #-}
module Main (main) where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import System.Environment (lookupEnv)
import qualified Test.HUnit as H
import Prelude (IO, Maybe (Just), pure, putStrLn, (>>=))
main :: IO ()
main =
  lookupEnv "DEBUG" >>= \case
    Just "1" -> do
      let _ = A.encode (A.object [])
      let _ = BL.empty
      let _ = H.TestCase (pure ())
      putStrLn "test ... ok"
    _ -> putStrLn "Hello World"
