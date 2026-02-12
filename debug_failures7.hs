#!/usr/bin/env runhaskell
import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let input = "\t\t \n\t"
    let inputLines = lines input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length of inputLines: " ++ show (length inputLines)
    putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)
    
    -- Check conditions
    let hasTabs = '\t' `elem` input
    let hasSpaces = ' ' `elem` input
    let notAllSpace = not (all isSpace input)
    putStrLn $ "Has tabs: " ++ show hasTabs
    putStrLn $ "Has spaces: " ++ show hasSpaces
    putStrLn $ "Not all space: " ++ show notAllSpace
    putStrLn $ "Mixed indentation condition: " ++ show (hasTabs && hasSpaces && notAllSpace)