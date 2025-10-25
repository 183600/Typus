{-# LANGUAGE OverloadedStrings #-}
import Ownership (analyzeOwnership, formatOwnershipErrors)
import System.Environment (getArgs)

main :: IO () = do
  args <- getArgs
  case args of
    [fp] -> do
      code <- readFile fp
      let errs = analyzeOwnership code
      if null errs then putStrLn "NO ERRORS" else putStrLn (formatOwnershipErrors errs)
    _ -> putStrLn "usage: tmp_check_ownership <file>"
