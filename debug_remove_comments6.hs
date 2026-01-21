import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)

-- 模拟isUnclosedStringLiteral函数
isUnclosedStringLiteral :: String -> Bool
isUnclosedStringLiteral [] = False
isUnclosedStringLiteral str = 
  let (inString, _) = foldl trackState (False, 0) str
      trackState (state, pos) c
        | c == '"' && not (isEscaped str pos) = (not state, pos + 1)
        | otherwise = (state, pos + 1)
      isEscaped str' pos = 
        if pos <= 0 then False
        else 
          let beforePos = take pos str'
              countBackslashes = length $ takeWhile (== '\\') $ reverse beforePos
          in countBackslashes `mod` 2 == 1
  in inString

-- 模拟hasCommentOutsideStrings函数
hasCommentOutsideStrings :: String -> Bool
hasCommentOutsideStrings s = hasLineCommentOutsideStrings || hasBlockCommentOutsideStrings
  where
    hasLineCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "//" s
    hasBlockCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "/*" s
    
    findAllIndices pat str = 
      let go _ [] = []
          go n s' = if pat `isPrefixOf` s'
                   then n : go (n + length pat) (drop (length pat) s')
                   else go (n + 1) (drop 1 s')
      in go (0 :: Int) str
    
    isInStringAt idx = 
      let before = take idx s
          inString = scanForStringState before
      in inString
      where
        scanForStringState [] = False
        scanForStringState str = 
          let (inString, _) = foldl trackState (False, 0) str
              trackState (state, count) c
                | c == '"' && not (isEscaped str (count - 1)) = (not state, count + 1)
                | otherwise = (state, count + 1)
              isEscaped str' pos = 
                if pos < 0 then False
                else 
                  let beforePos = take (pos + 1) str'
                      countBackslashes = length $ takeWhile (== '\\') $ reverse beforePos
                  in countBackslashes `mod` 2 == 1
          in inString

main :: IO ()
main = do
    let testCase = "\"string // not comment\" // real comment"
        isUnclosed = isUnclosedStringLiteral testCase
        hasCommentOutside = hasCommentOutsideStrings testCase
        allSpace = all isSpace testCase
    
    putStrLn $ "Input: " ++ show testCase
    putStrLn $ "Is unclosed string literal: " ++ show isUnclosed
    putStrLn $ "Has comment outside strings: " ++ show hasCommentOutside
    putStrLn $ "All space: " ++ show allSpace
    putStrLn $ "Equal to //: " ++ show (testCase == "//")
    putStrLn $ "Length 1: " ++ show (length testCase == 1)
    
    if isUnclosed
        then putStrLn $ "Would return original (unclosed string)"
    else if not hasCommentOutside
        then putStrLn $ "Would return original (no comment outside)"
    else if allSpace
        then putStrLn $ "Would return original (all space)"
    else if testCase == "//"
        then putStrLn $ "Would return empty"
    else if length testCase == 1
        then putStrLn $ "Would return original (single char)"
    else putStrLn $ "Would process with goNormal"