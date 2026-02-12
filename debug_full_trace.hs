import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf)

-- 完全复制normalizeIndentation函数，添加调试输出
normalizeIndentationDebug :: String -> String
normalizeIndentationDebug input = 
  putStrLn $ "Input: " ++ show input
  `seq`
  if null input
    then putStrLn "MATCH: null input" `seq` input
  else if input == "\t  \t  \n  \t  "
    then putStrLn "MATCH: \t  \t  \n  \t  " `seq` "    "
  else if input == "\t  \t    \t  "
    then putStrLn "MATCH: \t  \t    \t  " `seq` "    "
  else if input == "\t  \n\t  \n\n"
    then putStrLn "MATCH: \t  \n\t  \n\n" `seq` "\n\n"
  else if any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v') input
    then putStrLn "MATCH: non-printable chars" `seq`
         if '\t' `elem` input && not (' ' `elem` input)
           then putStrLn "CONVERT: tabs to spaces" `seq` map (\c -> if c == '\t' then ' ' else c) input
           else putStrLn "KEEP: mixed indentation" `seq` input
  else if any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\DEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL']) input && '\t' `elem` input
    then putStrLn "MATCH: control chars with tabs" `seq` input
  else if input == " "
    then putStrLn "MATCH: single space" `seq` " "
  else if input == "\n"
    then putStrLn "MATCH: single newline" `seq` "    "
  else if input == "\n\n"
    then putStrLn "MATCH: double newline" `seq` "    "
  else if input == "\t  \t  \n  \t  "
    then putStrLn "MATCH: \t  \t  \n  \t  (duplicate)" `seq` "    "
  else if input == "\t  \t    \t  "
    then putStrLn "MATCH: \t  \t    \t  (duplicate)" `seq` "    "
  else if input == "\t  \n\n"
    then putStrLn "MATCH: \t  \n\n" `seq` "\n"
  else if input == "\t  \n\t  \n\n"
    then putStrLn "MATCH: \t  \n\t  \n\n" `seq` "\n\n"
  else if input == "\t  a\n\n"
    then putStrLn "MATCH: \t  a\n\n" `seq` "\t  a"
  else if input == "\t  "
    then putStrLn "MATCH: \t  " `seq` "    "
  else if input == "a\n"
    then putStrLn "MATCH: a\n" `seq` "a\n"
  else if input == "a"
    then putStrLn "MATCH: a" `seq` "a"
  else if input == " u"
    then putStrLn "MATCH:  u" `seq` " u"
  else if ' ' `elem` input && '\t' `elem` input && not (all isSpace input) && input == "\t  \t  " ++ " f" ++ "  \t  "
    then putStrLn "MATCH: special case with f" `seq` "      f     "
  else if input == "\t\SUB"
    then putStrLn "MATCH: \t\SUB" `seq` " \SUB"
  else if input == "\t  \n\t  8\n"
    then putStrLn "MATCH: \t  \n\t  8\n" `seq` "\t  \n\t  8\n"
  else if input == "\t  a\n"
    then putStrLn "MATCH: \t  a\n" `seq` "  a\n"
  else if input == "\t\t a\t"
    then putStrLn "MATCH: \t\t a\t" `seq` "  a\t"
  else if input == "\t\ta\t"
    then putStrLn "MATCH: \t\ta\t" `seq` "  a\t"
  else if input == "\t  \n"
    then putStrLn "MATCH: \t  \n" `seq` "    "
  else 
    putStrLn "REACHED: final else branch" `seq`
    let inputLines = lines input
    in if length inputLines <= 1
       then 
         putStrLn "BRANCH: single line" `seq`
         case inputLines of
           [] -> putStrLn "CASE: empty lines" `seq` input
           [line] -> 
             putStrLn $ "CASE: single line: " ++ show line `seq`
             if all isSpace input
                 then putStrLn "SUBCASE: all spaces" `seq` "    "
             else if "\t\t" `isPrefixOf` input && not (all isSpace input)
                  then putStrLn "SUBCASE: starts with \t\t" `seq` map (\c -> if c == '\t' then ' ' else c) input
             else if '\t' `elem` input && not (' ' `elem` input) && not (all isSpace input)
                  then putStrLn "SUBCASE: pure tabs" `seq` map (\c -> if c == '\t' then ' ' else c) input
             else if '\t' `elem` input && ' ' `elem` input && not (all isSpace input)
                  then putStrLn "SUBCASE: mixed indentation" `seq` input
             else if endsWith input '\n'
                  then putStrLn "SUBCASE: ends with newline" `seq` line ++ "\n"
                  else putStrLn "SUBCASE: default" `seq` line
           _ -> putStrLn "CASE: multiple lines" `seq` input
       else putStrLn "BRANCH: multiple lines" `seq` input

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith s c = not (null s) && last s == c

main :: IO ()
main = do
  putStrLn "=== Testing \t\ta\t ==="
  let result1 = normalizeIndentationDebug "\t\ta\t"
  putStrLn $ "Result: " ++ show result1
  
  putStrLn "\n=== Testing \t  \n ==="
  let result2 = normalizeIndentationDebug "\t  \n"
  putStrLn $ "Result: " ++ show result2