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

-- 简化版的removeComments函数
removeComments :: String -> String
removeComments s = 
  putStrLn $ "Checking: " ++ show s >>
  putStrLn $ "Is unclosed: " ++ show (isUnclosedStringLiteral s) >>
  putStrLn $ "Has comment outside: " ++ show (hasCommentOutsideStrings s) >>
  putStrLn $ "All space: " ++ show (all isSpace s) >>
  if isUnclosedStringLiteral s
    then s
  else if not (hasCommentOutsideStrings s)
    then s
  else if all isSpace s
    then s
  else if s == "//"
    then ""
  else if length s == 1
    then s
  else
    goNormal s
  where
    goNormal [] = []
    goNormal ('"':xs) = '"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal (c:cs) = c : goNormal cs

    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine (_:xs) = skipLine xs

    skipBlock [] _depth = []
    skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth
    skipBlock ('/':'*':xs) depth = skipBlock xs (depth + 1)
    skipBlock ('*':'/':xs) 0 = goNormal xs
    skipBlock ('*':'/':xs) depth = skipBlock xs (depth - 1)
    skipBlock (_:xs) _depth = skipBlock xs _depth
    
    goInString [] = []
    goInString ('\n':xs) = '\n' : goNormal xs
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '"' : goNormal xs
    goInString (c:cs) = c : goInString cs

    goInChar [] = []
    goInChar ('\n':xs) = '\n' : goNormal xs
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
    let testCase = "\"string // not comment\" // real comment"
    result <- removeComments testCase
    putStrLn $ "Result: " ++ show result