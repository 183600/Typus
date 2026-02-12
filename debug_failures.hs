#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test prop_normalize_indentation_tabs failure case: " "
  putStrLn "Testing normalizeIndentation with \" \" (single space):"
  let input1 = " "
  let result1 = Utils.normalizeIndentation input1
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "Output: " ++ show result1
  putStrLn ""
  
  -- Test prop_normalize_indentation_multiline_mixed failure case: ["\n\1097959"]
  putStrLn "Testing normalizeIndentation with [\"\\n\\1097959\"]:"
  let input2 = "\n\1097959"
  let result2 = Utils.normalizeIndentation input2
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Output: " ++ show result2
  putStrLn ""
  
  -- Test is_complete_string_literal failure case: "a\""
  putStrLn "Testing isCompleteStringLiteral with \"a\\\"\":"
  let input3 = "a\""
  let result3 = Utils.isCompleteStringLiteral input3
  putStrLn $ "Input: " ++ show input3
  putStrLn $ "Output: " ++ show result3
  putStrLn ""
  
  -- Test is_problematic_unclosed_string failure case: "a\""
  putStrLn "Testing isProblematicUnclosedString with \"a\\\"\":"
  let input4 = "a\""
  let result4 = Utils.isProblematicUnclosedString input4
  putStrLn $ "Input: " ++ show input4
  putStrLn $ "Output: " ++ show result4
  putStrLn ""
  
  -- Test prop_string_lines failure case: "1\n"
  putStrLn "Testing lines with \"1\\n\":"
  let input5 = "1\n"
  let result5 = lines input5
  putStrLn $ "Input: " ++ show input5
  putStrLn $ "Output: " ++ show result5
  putStrLn ""
  
  -- Test prop_normalize_indentation_mixed failure case: "\n"
  putStrLn "Testing normalizeIndentation with \"\\n\":"
  let input6 = "\n"
  let result6 = Utils.normalizeIndentation input6
  putStrLn $ "Input: " ++ show input6
  putStrLn $ "Output: " ++ show result6
  putStrLn ""