#!/usr/bin/env stack
-- stack --resolver --package http-conduit --package cmdargs lts-6.15 script
{-# LANGUAGE DeriveDataTypeable #-}

import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (isPrefixOf)
import Network.HTTP.Conduit
import System.Console.CmdArgs
import System.IO (hGetContents)
import System.Process

data Parsed
  = Line String
  | File (IO [Parsed])

readStdout :: String -> IO String
readStdout command = do
  (_, Just hout, Just herr, jHandle)
        -- Replace with some other command on Windows
     <-
    createProcess
      (shell command)
      {cwd = Just ".", std_out = CreatePipe, std_err = CreatePipe}
  hGetContents hout

readBody :: String -> IO String
readBody a = fmap unpack (simpleHttp a)

-- rename File to Source or some other name
parseLine :: String -> Parsed
parseLine line
  | "require http" `isPrefixOf` line =
    File (fmap parse (readBody rest))
  | "require " `isPrefixOf` line = File (fmap parse (readFile rest))
  | "require-run " `isPrefixOf` line = File (fmap parse (readStdout rest))
  where
    rest = unwords $ tail $ words line
parseLine v = Line v

parse :: String -> [Parsed]
parse = map parseLine . lines

lineContents :: Parsed -> String
lineContents (Line a) = a

printParsed :: [Parsed] -> IO ()
printParsed [] = putStrLn "" -- Replace putStrLn with putStr
printParsed (Line a:b) = do
  putStrLn a
  printParsed b
printParsed (File a:b) = do
  printParsed =<< a
  printParsed b

-- args
data MashArgs = MashArgs
  { once :: Bool
  , file :: String
  } deriving (Typeable, Data, Show, Eq)

mashArgs :: MashArgs
mashArgs =
  MashArgs
  { once = False &= help "Require duplicate sources only the first time"
  , file = def &= args &= typ "FILE/URL"
  } &=
  help "More flexible file concatenation" &=
  summary "Mash (C) Bas Huis" &=
  details
    [ "Mash helps you split up files and concat them easier"
    , ""
    , "To output the generated file to stdout: "
    , "  mash ./doc/README.md"
    ]

getInput :: String -> IO String
getInput x
  | "http" `isPrefixOf` x = readBody x
getInput "-" = getContents
getInput x = readFile x

main :: IO ()
main = do
  mash <- cmdArgs mashArgs
  (printParsed . parse) =<< getInput (file mash)
