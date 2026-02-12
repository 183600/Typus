#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test prop_is_complete_string_literal failure case: s = "a\""
  putStrLn "Testing prop_is_complete_string_literal with s = \"a\\\"\":"
  let s = "a\""
  let quoted = "\"" ++ s ++ "\""
  let incomplete = "\"" ++ s
  putStrLn $ "s: " ++ show s
  putStrLn $ "quoted: " ++ show quoted
  putStrLn $ "incomplete: " ++ show incomplete
  putStrLn $ "U.isCompleteStringLiteral quoted: " ++ show (Utils.isCompleteStringLiteral quoted)
  putStrLn $ "U.isCompleteStringLiteral incomplete: " ++ show (Utils.isCompleteStringLiteral incomplete)
  putStrLn $ "Test passes: " ++ show (Utils.isCompleteStringLiteral quoted && not (Utils.isCompleteStringLiteral incomplete))
  putStrLn ""
  
  -- Test prop_is_problematic_unclosed_string failure case: s = "a\""
  putStrLn "Testing prop_is_problematic_unclosed_string with s = \"a\\\"\":"
  let closed = "\"" ++ s ++ "\""
  let unclosed = "\"" ++ s
  let withEscaped = "\"" ++ s ++ "\\\""
  putStrLn $ "s: " ++ show s
  putStrLn $ "closed: " ++ show closed
  putStrLn $ "unclosed: " ++ show unclosed
  putStrLn $ "withEscaped: " ++ show withEscaped
  putStrLn $ "U.isProblematicUnclosedString closed: " ++ show (Utils.isProblematicUnclosedString closed)
  putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (Utils.isProblematicUnclosedString unclosed)
  putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (Utils.isCompleteStringLiteral withEscaped)
  putStrLn $ "Test passes: " ++ show (not (Utils.isProblematicUnclosedString closed) && 
                                      Utils.isProblematicUnclosedString unclosed &&
                                      Utils.isCompleteStringLiteral withEscaped)
  putStrLn ""