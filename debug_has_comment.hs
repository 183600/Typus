import Data.List as L

main :: IO ()
main = do
  let input = "//\""
  putStrLn $ "Input: " ++ show input
  
  -- 检查是否有注释在字符串外
  let hasCommentOutside = hasCommentOutsideStrings input
  putStrLn $ "hasCommentOutsideStrings: " ++ show hasCommentOutside
  
  -- 查找所有//的位置
  let indices = findAllIndices "//" input
  putStrLn $ "Indices of //: " ++ show indices
  
  -- 检查每个位置是否在字符串内
  mapM_ (\idx -> do
            let before = take idx input
            let inString = scanForStringState before
            putStrLn $ "At index " ++ show idx ++ ", before: " ++ show before ++ ", inString: " ++ show inString
         ) indices

findAllIndices :: String -> String -> [Int]
findAllIndices pat str' = 
  let go _ [] = []
      go n s'' = if pat `L.isPrefixOf` s''
               then n : go (n + length pat) (drop (length pat) s'')
               else go (n + 1) (drop 1 s'')
  in go (0 :: Int) str'

scanForStringState :: String -> Bool
scanForStringState str' = 
  let (inString, _) = foldl trackState (False, 0) str'
      trackState (state, pos) c
        | c == '"' && not (isEscaped str' pos) = (not state, pos + 1)
        | otherwise = (state, pos + 1)
      isEscaped str'' pos = 
        if pos <= 0 then False
        else 
          let beforePos = take pos str''
              countBackslashes = length $ takeWhile (== '\\') $ reverse beforePos
          in countBackslashes `mod` 2 == 1
  in inString

hasCommentOutsideStrings :: String -> Bool
hasCommentOutsideStrings str = hasLineCommentOutsideStrings || hasBlockCommentOutsideStrings
  where
    hasLineCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "//" str
    hasBlockCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "/*" str
    
    isInStringAt idx = 
      let before = take idx str
          inString = scanForStringState before
      in inString