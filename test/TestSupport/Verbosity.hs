module TestSupport.Verbosity
  ( Verbosity(..)
  , getVerbosity
  , whenVerbose
  , logVerbose
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import System.Environment (lookupEnv)

-- | Simple verbosity flag used to control optional test logging.
data Verbosity = Quiet | Verbose deriving (Eq, Show)

-- | Read the verbosity from the @TYPUS_TEST_VERBOSE@ environment variable.
-- Any of "1", "true", "yes", "on", or "verbose" (case insensitive) enables
-- verbose output. Everything else defaults to quiet mode.
getVerbosity :: IO Verbosity
getVerbosity = do
  mValue <- lookupEnv "TYPUS_TEST_VERBOSE"
  pure $ maybe Quiet (toVerbosity . normalise) mValue
  where
    toVerbosity value
      | value `elem` ["1", "true", "yes", "on", "verbose"] = Verbose
      | otherwise = Quiet
    normalise = map toLower . trim
    trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Run an action only when verbose logging is enabled.
whenVerbose :: Verbosity -> IO () -> IO ()
whenVerbose Verbose action = action
whenVerbose Quiet _ = pure ()

-- | Print a message only when verbose logging is enabled.
logVerbose :: Verbosity -> String -> IO ()
logVerbose verbosity msg = whenVerbose verbosity (putStrLn msg)
