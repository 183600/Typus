import Data.Char (isPrint)

-- 测试非打印字符判断
testNonPrintable :: IO ()
testNonPrintable = do
  let mixed = "\t  \t  a  \t  "
  putStrLn $ "mixed: " ++ show mixed
  putStrLn $ "concat [mixed]: " ++ show (concat [mixed])
  putStrLn $ "any (not . isPrint) (concat [mixed]): " ++ show (any (not . isPrint) (concat [mixed]))
  putStrLn $ "any (\\c -> not (isPrint c) && c `notElem` \"\\n\\r\\t \") (concat [mixed]): " ++ show (any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat [mixed]))
  
  -- 测试每个字符
  putStrLn "\n=== Testing each character ==="
  mapM_ (\c -> do
            putStrLn $ "char: " ++ show c ++ ", fromEnum: " ++ show (fromEnum c) ++ ", isPrint: " ++ show (isPrint c) ++ ", in "\n\r\t ": " ++ show (c `elem` "\n\r\t ")
         ) mixed

main :: IO ()
main = testNonPrintable