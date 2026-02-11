-- Debug script for normalizeIndentation with ["",""]
import Utils as U
import Data.Char (isPrint)
import Data.List (isInfixOf)

-- Test normalizeIndentation with ["",""]
test1 :: IO ()
test1 = do
    let lines' = ["",""]
    let withMixed = map ("\t  " ++) lines'
    let unlinesInput = unlines withMixed
    let inputLines = lines unlinesInput
    let isEmptyLines = inputLines == ["", ""]
    let isTabEmptyLines = inputLines == ["\t  ", "\t  "]
    let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
    let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
    let isCodeBlock = any (`isInfixOf` unlinesInput) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
    
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Unlines input: " ++ show unlinesInput
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "isEmptyLines: " ++ show isEmptyLines
    putStrLn $ "isTabEmptyLines: " ++ show isTabEmptyLines
    putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
    putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
    putStrLn $ "isCodeBlock: " ++ show isCodeBlock
    
    let normalized = U.normalizeIndentation unlinesInput
    let normLines = lines normalized
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Norm lines: " ++ show normLines
    putStrLn $ "Length normLines: " ++ show (length normLines)
    putStrLn $ "Expected length: " ++ show (length lines')
    putStrLn ""

main :: IO ()
main = do
    putStrLn "=== Test 1: normalizeIndentation with [\"\"] ==="
    test1