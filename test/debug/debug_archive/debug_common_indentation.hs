import Utils
import Data.List
import Data.Char

main :: IO ()
main = do
    let input = "    if condition {\n        // do something\n        return \n    }\n"
    let inputLines = lines input
    putStrLn $ "Input lines: " ++ show inputLines
    
    let nonEmptyLines = filter (not . all isSpace) inputLines
    putStrLn $ "Non-empty lines: " ++ show nonEmptyLines
    
    let commonPrefix = findCommonIndentation nonEmptyLines
    putStrLn $ "Common prefix: " ++ show (commonPrefix ++ " (length: " ++ show (length commonPrefix) ++ ")")
    
    mapM_ (\line -> 
        putStrLn $ "Line: " ++ show line ++ " has prefix: " ++ show (commonPrefix `isPrefixOf` line)
        ) inputLines

findCommonIndentation :: [String] -> String
findCommonIndentation [] = ""
findCommonIndentation (x:xs) = 
  let isIndentChar c = c == ' ' || c == '\t'
      indentOfLine = takeWhile isIndentChar x
  in foldl commonIndent indentOfLine xs
  where
    commonIndent acc line = 
      let common = takeWhile (\(a, b) -> a == b) $ zip acc line
      in map fst common