module Main where

import Config

import Text.IntellijIml (removeFacet)

main :: IO ()
main = do
  config <- readConfig
  putStrLn "-- Removing the facets --"
  mapM_ overwriteFile $ filesFromConfig config
  putStrLn "-- Removed the facets --"

overwriteFile :: String -> IO ()
overwriteFile fileName = do
  imlFileContents <- readFile fileName
  imlFileWithoutFacet <- fileWithoutFacet $ removeFacet fileName imlFileContents
  writeFile fileName imlFileWithoutFacet
  putStrLn $ "Cleaned up " ++ fileName
  where fileWithoutFacet :: Either String String -> IO String
        fileWithoutFacet (Left errorMsg) = fail errorMsg
        fileWithoutFacet (Right contents) = return contents
