import Data.List (isPrefixOf)
import Data.Char

-- 测试hasCommentOutsideStrings函数的问题
testHasCommentOutsideStrings :: String -> IO ()
testHasCommentOutsideStrings s = do
    let hasLineCommentOutsideStrings = any (not . isInStringAt s) $ findAllIndices "//" s
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Has // outside strings: " ++ show hasLineCommentOutsideStrings
    putStrLn $ "All // indices: " ++ show (findAllIndices "//" s)
    putStrLn $ "Is each // in string: " ++ show (map (isInStringAt s) (findAllIndices "//" s))
    putStrLn "---"

findAllIndices :: String -> String -> [Int]
findAllIndices pat str = 
  let go _ [] = []
      go n s' = if pat `isPrefixOf` s'
               then n : go (n + length pat) (drop (length pat) s')
               else go (n + 1) (drop 1 s')
  in go (0 :: Int) str

isInStringAt :: String -> Int -> Bool
isInStringAt str idx = 
  let before = take idx str
      inString = scanForStringState before
  in inString
  where
    scanForStringState [] = False
    scanForStringState str = 
      let (inString, inChar) = foldl trackState (False, False) str
          trackState (stateS, stateC) c
            | c == '"' && not (isEscaped str (length (takeWhile (/= c) str))) = (not stateS, False)
            | c == '\'' && not (isEscaped str (length (takeWhile (/= c) str))) = (False, not stateC)
            | otherwise = (stateS, stateC)
          isEscaped str' pos = 
            if pos <= 0 then False
            else 
              let beforePos = take pos str'
                  countBackslashes = length $ takeWhile (== '\\') $ reverse beforePos
              in countBackslashes `mod` 2 == 1
      in inString || inChar

main :: IO ()
main = do
    let testCases = 
            [ "code // comment"
            , "\"string // not comment\" // real comment"
            , "'c' // comment"
            , "\"string // not comment\""
            , "'c // not comment'"
            ]
    
    mapM_ testHasCommentOutsideStrings testCases