#!/usr/bin/env runhaskell
import Utils
import Data.List (isPrefixOf)
import Data.Char (isPrint)

main :: IO ()
main = do
    let input = "\t\t \n\t"
    putStrLn $ "Input: " ++ show input
    let inputLines = lines input
    putStrLn $ "Lines: " ++ show inputLines
    let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
    putStrLn $ "Has mixed indentation: " ++ show hasMixedIndentation
    let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
    putStrLn $ "Has non-printable: " ++ show hasNonPrintable
    
    -- Test conversion
    let converted = map (\c -> if c == '\t' then ' ' else c) input
    putStrLn $ "Converted: " ++ show converted
    let convertedLines = lines converted
    putStrLn $ "Converted lines: " ++ show convertedLines