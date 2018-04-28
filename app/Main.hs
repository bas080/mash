module Main where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (stripPrefix, insert, isPrefixOf)
import MashArgs
import Network.HTTP.Conduit
import System.Console.CmdArgs
import System.Environment (getArgs)
import System.IO (getContents)

data Line = Statement String | RequireStatement FilePath

contentToLine :: String -> Line
contentToLine line = case stripPrefix "require " line of
  Nothing -> Statement line
  (Just filePath) -> RequireStatement filePath

contentToLines :: String -> [Line]
contentToLines = map contentToLine . lines

append :: [a] -> [a] -> [a]
append a b = b ++ a

isUrl :: String -> Bool
isUrl = isPrefixOf "http"

readContents :: String -> IO String
readContents item
  | isUrl item = fmap unpack (simpleHttp item)
  | otherwise = readFile item

printScript :: Bool -> [FilePath] -> [Line] -> IO ()
printScript o fp [] = putStrLn ""
printScript o fp (Statement a:b) = do
  putStrLn a
  printScript o fp b
printScript o fp (RequireStatement a:b) = if o && a `elem` fp
    then printScript o fp b
    else (printScript o (insert a fp) . append b . contentToLines) =<< readContents a

input :: [String] -> IO String
input ["-"] = getContents
input [x] = readContents x

hasOnceFlag :: [String] -> Bool
hasOnceFlag [] = False
hasOnceFlag ("-o":r) = True
hasOnceFlag ("--once":r) = True
hasOnceFlag (_:a) = hasOnceFlag a

excludeOptions :: [String] -> [String]
excludeOptions [] = []
excludeOptions (('-':a): x) = excludeOptions x
excludeOptions (a:xs) = a:excludeOptions xs

main :: IO ()
main = do
  args <- getArgs
  mash <- cmdArgs mashArgs
  (printScript (once mash) [] . contentToLines) =<< input (excludeOptions args)
