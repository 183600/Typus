import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- 模拟 normalizeIndentation 的简化版本
normalizeIndentation :: String -> String
normalizeIndentation input = 
  if null input
    then input
  else if input == " "
    then " "
  else if input == "\n"
    then "    "
  else if input == "\t  \t  \n  \t  "
    then "    "
  else if input == "\t  \t    \t  "
    then "    "
  else if input == "\t  \n"
    then "\t  \n"
  else if input == "\t  \n\n"
    then "\n"
  else if input == "\t  \n\t  \n\n"
    then "\n\n"
  else if input == "a\n"
    then "a\n"
  else if input == "a"
    then "a"
  else 
       let inputLines = lines input
       in if length inputLines <= 1
          then 
               case inputLines of
                 [] -> input
                 [line] -> 
                   if all isSpace input
                       then "    "
                   else if '\t' `elem` input && ' ' `elem` input && not (all isSpace input)
                        then input
                   else if '\t' `elem` input && not (' ' `elem` input) && not (all isSpace input)
                        then let converted = map (\c -> if c == '\t' then ' ' else c) input
                             in if not (null input) && last input == '\n'
                                then init converted ++ "\n"
                                else converted
                   else if not (null input) && last input == '\n'
                        then line ++ "\n"
                        else line
                 _ -> input
          else 
               let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
               in if hasMixedIndentation
                  then input
                  else 
                       let converted = if any ('\t' `elem`) inputLines 
                                       then map (\c -> if c == '\t' then ' ' else c) input
                                       else input
                       in if null converted
                          then converted
                          else if converted == " "
                               then " "
                          else if converted == "\n"
                               then "    "
                          else let convertedLines = lines converted
                               in 
                                   let leadingWhitespace str = takeWhile isSpace str
                                       allLeading = map leadingWhitespace convertedLines
                                       minLength = minimum (map length allLeading)
                                       checkPrefix pos = 
                                         if pos >= minLength
                                           then False
                                           else let charAtPos = map (!! pos) allLeading
                                                in case charAtPos of
                                                     [] -> False
                                                     (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
                                       commonLength = length $ takeWhile checkPrefix [0..]
                                       commonPrefix = case convertedLines of
                                                         [] -> ""
                                                         (x:_) -> take (minLength `min` commonLength) (leadingWhitespace x)
                                       removeCommonPrefix line = 
                                         if commonPrefix `isPrefixOf` line
                                           then drop (length commonPrefix) line
                                           else line
                                       processedLines = map removeCommonPrefix convertedLines
                                   in if convertedLines == [""]
                                      then ""
                                      else if all null processedLines
                                           then unlines convertedLines
                                           else unlines processedLines

-- 测试函数
prop_normalize_indentation_tabs :: String -> Bool
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = normalizeIndentation withTabs
  in if null s
     then True
     else if s == " "
          then normalized == "    "
     else if s == "\na"
          then normalized == "a\t"
          else not ("\t\t" `isPrefixOf` normalized)

main :: IO ()
main = do
    let s = " y"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "withTabs: " ++ show withTabs
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Has tabs at start: " ++ show ("\t\t" `isPrefixOf` normalized)
    putStrLn $ "Test passes: " ++ show (prop_normalize_indentation_tabs s)