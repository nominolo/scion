module Scion.Server.Options where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )

data ServerPort = AutoPort | FixedPort Int

data ServerOptions = ServerOptions
    { optPort     :: ServerPort
    , optLogLevel :: Int
    }

defaultOptions = ServerOptions
    { optPort     = AutoPort
    , optLogLevel = 2
    }

readOptions :: IO ServerOptions
readOptions = do
    argv <- getArgs
    case getOpt Permute options argv of
        (o, n, []  ) -> return $ foldl (flip id) defaultOptions o
        (_, _, errs) -> putStr (concat errs ++ usageInfo header options) >> exitWith (ExitFailure 1)
    where
        header = "Command line options:"


options :: [OptDescr (ServerOptions -> ServerOptions)]
options =
    [ Option ['p'] ["port"]
        (ReqArg (\p opts -> opts { optPort = FixedPort (read p) }) "<port-number>")
        "port number to listen on"
    , Option
        ['v'] ["verbosity"]
        (ReqArg (\v opts -> opts { optLogLevel = read v }) "<number>")
        "log level (not yet implemented)"
    ]
