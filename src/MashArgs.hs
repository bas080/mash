{-# LANGUAGE DeriveDataTypeable #-}

module MashArgs where

import System.Console.CmdArgs

data MashArgs = MashArgs
  { once :: Bool
  , files :: [String]
  } deriving (Typeable, Data, Show, Eq)

mashArgs :: MashArgs
mashArgs =
  MashArgs
  { once = False &= help "Require duplicate sources only the first time"
  , files = def &= args &= typ "FILE/URL"
  } &=
  help "More flexible file concatenation" &=
  summary "Mash (C) Bas Huis" &=
  details
    [ "Mash helps you split up files and concat them easier"
    , ""
    , "To output the generated file to stdout: "
    , "  mash ./doc/README.md"
    ]
