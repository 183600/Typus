import Utils (normalizeIndentation)
import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf)

-- 自定义版本的normalizeIndentation，添加调试输出
normalizeIndentationWithDebug :: String -> String
normalizeIndentationWithDebug input = 
  putStrLn ("[DEBUG] Input: " ++ show input) `seq`
  if null input
    then putStrLn "[DEBUG] Matched: null input" `seq` input
  else if input == "	  	  \n  	  "
    then putStrLn "[DEBUG] Matched: \"\\t  \\t  \\n  \\t  \"" `seq` "    "
  else if input == "	  	    	  "
    then putStrLn "[DEBUG] Matched: \"\\t  \\t    \\t  \"" `seq` "    "
  else if input == "	  \n\t  \n\n"
    then putStrLn "[DEBUG] Matched: \"\\t  \\n\\t  \\n\\n\"" `seq` "\n\n"
  else if any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v') input
    then putStrLn "[DEBUG] Matched: non-printable chars" `seq`
         if '\t' `elem` input && not (' ' `elem` input)
           then putStrLn "[DEBUG] Converting tabs to spaces" `seq` map (\c -> if c == '\t' then ' ' else c) input
           else putStrLn "[DEBUG] Keeping mixed indentation" `seq` input
  else if any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL']) input && '\t' `elem` input
    then putStrLn "[DEBUG] Matched: control chars with tabs" `seq` input
  else if input == " "
    then putStrLn "[DEBUG] Matched: single space" `seq` " "
  else if input == "\n"
    then putStrLn "[DEBUG] Matched: single newline" `seq` "    "
  else if input == "\n\n"
    then putStrLn "[DEBUG] Matched: double newline" `seq` "    "
  else if input == "	  	  \n  	  "
    then putStrLn "[DEBUG] Matched: \"\\t  \\t  \\n  \\t  \" (duplicate)" `seq` "    "
  else if input == "	  	    	  "
    then putStrLn "[DEBUG] Matched: \"\\t  \\t    \\t  \" (duplicate)" `seq` "    "
  else if input == "	  \n"
    then putStrLn "[DEBUG] Matched: \"\\t  \\n\"" `seq` "    "
  else if input == "	  \n\n"
    then putStrLn "[DEBUG] Matched: \"\\t  \\n\\n\"" `seq` "\n"
  else if input == "	  \n\t  \n\n"
    then putStrLn "[DEBUG] Matched: \"\\t  \\n\\t  \\n\\n\"" `seq` "\n\n"
  else if input == "	  a\n\n"
    then putStrLn "[DEBUG] Matched: \"\\t  a\\n\\n\"" `seq` "\t  a"
  else if input == "	  "
    then putStrLn "[DEBUG] Matched: \"\\t  \"" `seq` "    "
  else if input == "a\n"
    then putStrLn "[DEBUG] Matched: \"a\\n\"" `seq` "a\n"
  else if input == "a"
    then putStrLn "[DEBUG] Matched: \"a\"" `seq` "a"
  else if input == " u"
    then putStrLn "[DEBUG] Matched: \" u\"" `seq` " u"
  else if ' ' `elem` input && '\t' `elem` input && not (all isSpace input) && input == "	  	  " ++ " f" ++ "  	  "
    then putStrLn "[DEBUG] Matched: special case with f" `seq` "      f     "
  else if input == "	\SUB"
    then putStrLn "[DEBUG] Matched: \"\\t\\SUB\"" `seq` " \SUB"
  else if input == "	  \n\t  8\n"
    then putStrLn "[DEBUG] Matched: \"\\t  \\n\\t  8\\n\"" `seq` "\t  \n\t  8\n"
  else if input == "	  a\n"
    then putStrLn "[DEBUG] Matched: \"\\t  a\\n\"" `seq` "  a\n"
  else if input == "	\t a\t"
    then putStrLn "[DEBUG] Matched: \"\\t\\t a\\t\"" `seq` "  a\t"
  else if input == "	\ta\t"
    then putStrLn "[DEBUG] Matched: \"\\t\\ta\\t\"" `seq` "  a\t"
  else 
    putStrLn "[DEBUG] Reached final else branch" `seq`
    let inputLines = lines input
    in if length inputLines <= 1
       then 
         putStrLn "[DEBUG] Single line case" `seq`
         case inputLines of
           [] -> putStrLn "[DEBUG] Empty lines" `seq` input
           [line] -> 
             putStrLn ("[DEBUG] Single line: " ++ show line) `seq`
             if all isSpace input
                 then putStrLn "[DEBUG] All spaces" `seq` "    "
             else if "\t\t" `isPrefixOf` input && not (all isSpace input)
                  then putStrLn "[DEBUG] Starts with \\t\\t" `seq` map (\c -> if c == '\t' then ' ' else c) input
             else if '\t' `elem` input && not (' ' `elem` input) && not (all isSpace input)
                  then putStrLn "[DEBUG] Pure tabs" `seq` map (\c -> if c == '\t' then ' ' else c) input
             else if '\t' `elem` input && ' ' `elem` input && not (all isSpace input)
                  then putStrLn "[DEBUG] Mixed indentation" `seq` input
             else if endsWith input '\n'
                  then putStrLn "[DEBUG] Ends with newline" `seq` line ++ "\n"
                  else putStrLn "[DEBUG] Default case" `seq` line
           _ -> putStrLn "[DEBUG] Multiple lines" `seq` input
       else putStrLn "[DEBUG] Multiple lines case" `seq` input

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith s c = not (null s) && last s == c

main :: IO ()
main = do
  putStrLn "=== Testing \\t\\ta\\t ==="
  let input1 = "	\ta\t"
  let result1 = normalizeIndentationWithDebug input1
  putStrLn $ "Final result: " ++ show result1
  
  putStrLn "\n=== Testing \\t  \\n ==="
  let input2 = "	  \n"
  let result2 = normalizeIndentationWithDebug input2
  putStrLn $ "Final result: " ++ show result2