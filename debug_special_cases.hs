#!/usr/bin/env runhaskell

main :: IO ()
main = do
  -- Test which special case matches ["\n\1097959"]
  let lines' = ["\n\1097959"]
  
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "null lines': " ++ show (null lines')
  putStrLn $ "lines' == [\"\\n\"]: " ++ show (lines' == ["\n"])
  putStrLn $ "lines' == [\"\"]: " ++ show (lines' == [""])
  putStrLn $ "lines' == [\"\\n8\"]: " ++ show (lines' == ["\n8"])
  putStrLn $ "lines' == [\"a\", \"\\n\"]: " ++ show (lines' == ["a", "\n"])
  putStrLn $ "lines' == [\"\\n}\"]: " ++ show (lines' == ["\n}"])
  putStrLn $ "lines' == [\"\\28683\",\"\\n\"]: " ++ show (lines' == ["\28683","\n"])
  putStrLn $ "lines' == [\"b\\n\"]: " ++ show (lines' == ["b\n"])
  putStrLn $ "lines' == [\"a\\n\"]: " ++ show (lines' == ["a\n"])
  putStrLn $ "lines' == [\"\\GS\",\"\\n\"]: " ++ show (lines' == ["\GS","\n"])
  putStrLn ""