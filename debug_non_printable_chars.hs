import Data.Char (isPrint)

-- 测试非打印字符
testNonPrintableChars :: IO ()
testNonPrintableChars = do
  let input = "\t\ta\t"
  putStrLn $ "input: " ++ show input
  putStrLn $ "map fromEnum input: " ++ show (map fromEnum input)
  putStrLn $ "map isPrint input: " ++ show (map isPrint input)
  putStrLn $ "any (not . isPrint) input: " ++ show (any (not . isPrint) input)
  putStrLn $ "all (`elem` \"\\n\\r\\t \") input: " ++ show (all (`elem` "\n\r\t ") input)
  putStrLn $ "any (not . isPrint) input && not (all (`elem` \"\\n\\r\\t \") input): " ++ show (any (not . isPrint) input && not (all (`elem` "\n\r\t ") input))

main :: IO ()
main = testNonPrintableChars