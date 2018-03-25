module Main where

import Data.List (stripPrefix, insert)
import System.IO (getContents)
import System.Environment (getArgs)

data Line = Statement String | RequireStatement FilePath

contentToLine :: String -> Line
contentToLine line = case stripPrefix "require " line of
  Nothing -> Statement line
  (Just filePath) -> RequireStatement filePath

contentToLines :: String -> [Line]
contentToLines = map contentToLine . lines

append :: [a] -> [a] -> [a]
append a b = b ++ a

printScript :: Bool -> [FilePath] -> [Line] -> IO ()
printScript o fp [] = putStrLn ""
printScript o fp (Statement a:b) = do
  putStrLn a
  printScript o fp b
printScript o fp (RequireStatement a:b) = if o && a `elem` fp
    then printScript o fp b
    else (printScript o (insert a fp) . append b . contentToLines) =<< readFile a

input :: [String] -> IO String
input ["-"] = getContents
input [x] = readFile x

hasOnceFlag :: [String] -> Bool
hasOnceFlag [] = False
hasOnceFlag ("-o":r) = True
hasOnceFlag ("--once":r) = True
hasOnceFlag (_:a) = hasOnceFlag a

main :: IO ()
main = do
  args <- getArgs
  (printScript (hasOnceFlag args) [] . contentToLines) =<< input args
