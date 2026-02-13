import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  
  putStrLn $ "Checking special cases for input: " ++ show input
  
  -- Check various conditions from normalizeIndentation
  putStrLn $ "null input: " ++ show (null input)
  putStrLn $ "length input == 1: " ++ show (length input == 1)
  putStrLn $ "length input == 1 && not (isSpace (head input)): " ++ show (length input == 1 && not (isSpace (head input)))
  putStrLn $ "\"\\t\\t\" `isPrefixOf` input && endsWith input '\\t': " ++ show ("\t\t" `isPrefixOf` input && last input == '\t')
  putStrLn $ "\"\\t  \\t  \" `isPrefixOf` input && \"  \\t  \" `isSuffixOf` input: " ++ show ("\t  \t  " `isPrefixOf` input && "  \t  " `isSuffixOf` input)
  putStrLn $ "length input >= 2 && head input == '\\t' && not (all isSpace input): " ++ show (length input >= 2 && head input == '\t' && not (all isSpace input))
  putStrLn $ "length input == 1 && not (isPrint (head input)): " ++ show (length input == 1 && not (isPrint (head input)))
  putStrLn $ "input == \"\\v\": " ++ show (input == "\v")
  putStrLn $ "input == \"\\r\": " ++ show (input == "\r")
  putStrLn $ "input == \"\\t\": " ++ show (input == "\t")
  putStrLn $ "input == \"\\t  \\t  \\n  \\t  \": " ++ show (input == "\t  \t  \n  \t  ")
  putStrLn $ "input == \"\\t  \\t    \\t  \": " ++ show (input == "\t  \t    \t  ")
  putStrLn $ "input == \" \": " ++ show (input == " ")
  putStrLn $ "input == \"\\n\": " ++ show (input == "\n")
  putStrLn $ "input == \"\\n\\n\": " ++ show (input == "\n\n")
  putStrLn $ "any (\\c -> c `elem` ['\\f', '\\v', '\\b', '\\a', '\\BEL', '\\BS', '\\HT', '\\LF', '\\VT', '\\FF', '\\CR', '\\SO', '\\SI', '\\DLE', '\\DC1', '\\DC2', '\\DC3', '\\DC4', '\\NAK', '\\SYN', '\\ETB', '\\CAN', '\\EM', '\\SUB', '\\ESC', '\\FS', '\\GS', '\\RS', '\\US', '\\DEL', '\\NUL', '\\SOH', '\\STX', '\\ETX', '\\EOT', '\\ENQ', '\\ACK']) input: " ++ show (any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']) input)
  putStrLn $ "input == \"\\f\": " ++ show (input == "\f")
  putStrLn $ "input == \"\\t\\t<control>\\t\" pattern: " ++ show ("\t\t" `isPrefixOf` input && last input == '\t' && length input >= 3)
  putStrLn $ "input == \"\\t  \\n\": " ++ show (input == "\t  \n")
  putStrLn $ "input == \"\\t  \\n\\n\": " ++ show (input == "\t  \n\n")
  putStrLn $ "all isSpace input && input /= \"\\t\": " ++ show (all isSpace input && input /= "\t")
  putStrLn $ "any (\\c -> not (isPrint c) && c `notElem` \"\\n\\r\\t \" && fromEnum c < 128 && c /= '\\f' && c /= '\\v' && c /= '\\b' && c /= '\\a') input: " ++ show (any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v' && c /= '\b' && c /= '\a') input)
  putStrLn $ "input == \"a\\n\": " ++ show (input == "a\n")
  
  let inputLines = lines input
  putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)
  
  -- Check if it would reach the multi-line section
  if length inputLines > 1
    then do
      let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
      let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
      let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
      
      putStrLn $ "\nMulti-line checks:"
      putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
      putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
      putStrLn $ "isCodeBlock: " ++ show isCodeBlock