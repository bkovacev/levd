{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.Yaml
import Lib
import Config

messageFromFile :: Either ParseException LevdConfigParse -> String
messageFromFile (Left _) = "Error in parsing file..."
messageFromFile (Right (LevdConfigParse fp _ _ _ _ _ _)) = show (linesByRanges fp)

main :: IO ()
main = do
  file <- decodeFileEither "/Users/rblaffor/workspace/levd/test.yaml" :: IO (Either ParseException LevdConfigParse)
  putStrLn(messageFromFile file)
