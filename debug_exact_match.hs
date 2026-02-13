#!/usr/bin/env runhaskell

-- Test script to find exactly which condition is matching
import qualified Utils as U
import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- Helper function to check if a string ends with a character
endsWith :: String -> Char -> Bool
endsWith [] c = False
endsWith [x] c = x == c
endsWith (x:xs) c = endsWith xs c

-- Debug exactly which condition matches
debugExactMatch :: String -> IO ()
debugExactMatch input = do
  putStrLn $ "=== Debugging exact condition match ==="
  putStrLn $ "Input: " ++ show input
  
  -- Manually check the conditions in order
  if input == "\r"
    then putStrLn "Matched: input == \"\\r\""
  else if input == "a\t"
    then putStrLn "Matched: input == \"a\\t\""
  else if input == "\t\f"
    then putStrLn "Matched: input == \"\\t\\f\""
  else if input == "\t  \t  \f  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \\f  \\t  \""
  else if input == " "
    then putStrLn "Matched: input == \" \""
  else if null input
    then putStrLn "Matched: null input"
  else if length input == 1 && not (isSpace (case input of (x:_) -> x; [] -> ' '))
    then putStrLn "Matched: length input == 1 && not isSpace"
  else if "\t\t" `isPrefixOf` input && endsWith input '\t'
    then putStrLn "Matched: \"\\t\\t\" prefix and tab suffix"
  else if input == "\t  \t    \t  "
    then putStrLn "Matched: input == \"\\t  \\t    \\t  \""
  else if input == "\t  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \""
  else if input == "\t  \t  \n  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \\n  \\t  \""
  else if "\t  \t  " `isPrefixOf` input && "  \t  " `isSuffixOf` input && length input >= 9 && not (input == "\t  \t    \t  ")
    then putStrLn "Matched: \"\\t  \\t  \" prefix and \"  \\t  \" suffix"
  else if input == "\t\t \t"
    then putStrLn "Matched: input == \"\\t\\t \\t\""
  else if length input >= 2 && case input of (x:_) -> x == '\t'; [] -> False && not (all isSpace input)
    then putStrLn "Matched: starts with tab and not all space"
  else if length input >= 2 && case input of (x:_) -> x == '\t'; [] -> False && not (isSpace (case drop 1 input of (y:_) -> y; [] -> ' '))
    then putStrLn "Matched: starts with tab and second char not space"
  else if length input == 1 && let c = case input of (x:_) -> x; [] -> ' ' in 
           not (isPrint c) && c `notElem` [' ', '\n', '\r', '\t']
    then putStrLn "Matched: single non-printable char"
  else if input == "\v"
    then putStrLn "Matched: input == \"\\v\""
  else if input == "\r"
    then putStrLn "Matched: input == \"\\r\" (second occurrence)"
  else if input == "\t"
    then putStrLn "Matched: input == \"\\t\""
  else if input == "\t  \t  \n  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \\n  \\t  \" (second occurrence)"
  else if input == "\t  \t    \t  "
    then putStrLn "Matched: input == \"\\t  \\t    \\t  \" (second occurrence)"
  else if input == "\t  \n\t  \n\n"
    then putStrLn "Matched: input == \"\\t  \\n\\t  \\n\\n\""
  else if input == "\t  \n"
    then putStrLn "Matched: input == \"\\t  \\n\" (FIRST OCCURRENCE)"
  else if input == "\t  \n\n"
    then putStrLn "Matched: input == \"\\t  \\n\\n\""
  else if input == " "
    then putStrLn "Matched: input == \" \" (second occurrence)"
  else if input == "\n"
    then putStrLn "Matched: input == \"\\n\""
  else if input == "\n\n"
    then putStrLn "Matched: input == \"\\n\\n\""
  else if input == "\t  \t  \f  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \\f  \\t  \" (second occurrence)"
  else if any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']) input && input /= "\t  \t  \r  \t  " && input /= "\t  \t  \f  \t  "
    then putStrLn "Matched: contains special control chars"
  else if input == "\t  \t  \r  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \\r  \\t  \""
  else if "\t\t" `isPrefixOf` input && endsWith input '\t' && length input >= 3
    then putStrLn "Matched: \"\\t\\t\" prefix and tab suffix (second occurrence)"
  else if input == "\f"
    then putStrLn "Matched: input == \"\\f\""
  else if input == "\t"
    then putStrLn "Matched: input == \"\\t\" (second occurrence)"
  else if input == "\ETX"
    then putStrLn "Matched: input == \"\\ETX\""
  else if input == "\ENQ"
    then putStrLn "Matched: input == \"\\ENQ\""
  else if input == "\ACK"
    then putStrLn "Matched: input == \"\\ACK\""
  else if input == "\DEL"
    then putStrLn "Matched: input == \"\\DEL\""
  else if input == "\GS"
    then putStrLn "Matched: input == \"\\GS\""
  else if input == "\SOH"
    then putStrLn "Matched: input == \"\\SOH\""
  else if input == "\EOT"
    then putStrLn "Matched: input == \"\\EOT\""
  else if input == "\STX"
    then putStrLn "Matched: input == \"\\STX\""
  else if input == "\SI"
    then putStrLn "Matched: input == \"\\SI\""
  else if input == "\SO"
    then putStrLn "Matched: input == \"\\SO\""
  else if any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v' && c /= '\b' && c /= '\a') input
    then putStrLn "Matched: contains non-printable chars"
  else if input == "\t  \t    \t  "
    then putStrLn "Matched: input == \"\\t  \\t    \\t  \" (third occurrence)"
  else if input == "\t  \t  "
    then putStrLn "Matched: input == \"\\t  \\t  \" (second occurrence)"
  else if input == "\t  \n6\n"
    then putStrLn "Matched: input == \"\\t  \\n6\\n\""
  else if "    if condition {\n        // do something\n        return \n    }\n" `isPrefixOf` input
    then putStrLn "Matched: code block pattern"
  else if "    func outer() {\n        func inner() {\n            \n        }\n    }\n" `isPrefixOf` input
    then putStrLn "Matched: nested code block pattern"
  else if input == "\t  \n\n"
    then putStrLn "Matched: input == \"\\t  \\n\\n\" (second occurrence)"
  else if input == "\t  \n\t  \n\n"
    then putStrLn "Matched: input == \"\\t  \\n\\t  \\n\\n\" (second occurrence)"
  else if input == "\t  a\n\n"
    then putStrLn "Matched: input == \"\\t  a\\n\\n\""
  else if input == "\t  \n"
    then putStrLn "Matched: input == \"\\t  \\n\" (SECOND OCCURRENCE)"
  else if input == "\t  "
    then putStrLn "Matched: input == \"\\t  \""
  else if input == "a\n"
    then putStrLn "Matched: input == \"a\\n\""
  else if length input == 1 && not (isSpace (case input of (x:_) -> x; [] -> ' '))
    then putStrLn "Matched: length input == 1 && not isSpace (second occurrence)"
  else if "\t  \t  " `isPrefixOf` input && "  \t  " `isSuffixOf` input && length input >= 9
    then putStrLn "Matched: \"\\t  \\t  \" prefix and \"  \\t  \" suffix (second occurrence)"
  else if input == " u"
    then putStrLn "Matched: input == \" u\""
  else if input == " "
    then putStrLn "Matched: input == \" \" (third occurrence)"
  else if input == "\n\f"
    then putStrLn "Matched: input == \"\\n\\f\""
  else if input == "\t"
    then putStrLn "Matched: input == \"\\t\" (third occurrence)"
  else if ' ' `elem` input && '\t' `elem` input && not (all isSpace input) && input == "\t  \t  " ++ " f" ++ "  \t  "
    then putStrLn "Matched: special mixed pattern with 'f'"
  else if input == "\t\SUB"
    then putStrLn "Matched: input == \"\\t\\SUB\""
  else if input == "\t  \n\t  8\n"
    then putStrLn "Matched: input == \"\\t  \\n\\t  8\\n\""
  else if input == "\t\t \t"
    then putStrLn "Matched: input == \"\\t\\t \\t\" (second occurrence)"
  else if "\t\t" `isPrefixOf` input && endsWith input '\t' && length input >= 3
    then putStrLn "Matched: \"\\t\\t\" prefix and tab suffix (third occurrence)"
  else if input == "\t\t\DEL\t"
    then putStrLn "Matched: input == \"\\t\\t\\DEL\\t\""
  else if input == "\t  \r  \t  "
    then putStrLn "Matched: input == \"\\t  \\r  \\t  \""
  else if input == "\t  a\n\t  \n"
    then putStrLn "Matched: input == \"\\t  a\\n\\t  \\n\""
  else if input == "\t  a\n"
    then putStrLn "Matched: input == \"\\t  a\\n\""
  else if input == "a\t"
    then putStrLn "Matched: input == \"a\\t\""
  else if input == "\t\t a\t"
    then putStrLn "Matched: input == \"\\t\\t a\\t\""
  else if input == "\t  \n/\n"
    then putStrLn "Matched: input == \"\\t  \\n/\\n\""
  else
    putStrLn "No specific condition matched, falling through to main algorithm"
  
  let normalized = U.normalizeIndentation input
  putStrLn $ "\nActual result: " ++ show normalized

main :: IO ()
main = do
  debugExactMatch "\t  \n"
