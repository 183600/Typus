import Utils
import Data.Char (isSpace, isControl)

main :: IO ()
main = do
  let input = "\t"
      allSpace = all isSpace input
      notNull = not (null input)
      isControlChar = isControl (head input)
  putStrLn $ "input = " ++ show input
  putStrLn $ "all isSpace input = " ++ show allSpace
  putStrLn $ "not (null input) = " ++ show notNull
  putStrLn $ "isControl (head input) = " ++ show isControlChar
  putStrLn $ "any isControl input = " ++ show (any isControl input)
  
  let result = normalizeIndentation input
  putStrLn $ "result = " ++ show result