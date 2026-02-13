#!/usr/bin/env runhaskell
import Utils
import Data.Char (isSpace, isPrint)
import Data.List (isInfixOf)

main :: IO ()
main = do
    let input = "\t\t \n\t"
    let inputLines = lines input
    putStrLn $ "Input: " ++ show input
    
    -- Multi-line conditions
    let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
    putStrLn $ "Has mixed indentation: " ++ show hasMixedIndentation
    
    let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
    putStrLn $ "Has non-printable: " ++ show hasNonPrintable
    
    let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
    putStrLn $ "Is code block: " ++ show isCodeBlock
    
    let isEmptyLines = inputLines == ["", ""]
    putStrLn $ "Is empty lines: " ++ show isEmptyLines
    
    let isTabEmptyLines = inputLines == ["\t  ", "\t  "]
    putStrLn $ "Is tab empty lines: " ++ show isTabEmptyLines
    
    -- Check which condition would be triggered
    if isEmptyLines || isTabEmptyLines
       then putStrLn "Would take empty lines branch"
    else if isCodeBlock
       then putStrLn "Would take code block branch"
    else if hasMixedIndentation || hasNonPrintable
       then putStrLn "Would take mixed indentation or non-printable branch"
    else putStrLn "Would take the else branch"