{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.Yaml
import Lib
import Config

messageFromFile :: Either ParseException LevdConfig -> String
messageFromFile (Left _) = "Error in parsing file..."
messageFromFile (Right (LevdConfig fp _ _ _ _ _ _)) = show fp

main :: IO ()
main = do
  file <- decodeFileEither "/Users/rblaffor/workspace/levd/test.yaml" :: IO (Either ParseException LevdConfig)
  putStrLn(messageFromFile file)
