import Utils (removeLineComments)
import Data.Char (isSpace)

-- Recreate just the 5th condition of removeLineComments
testCondition5 :: String -> Bool
testCondition5 s = all isSpace s && not (null s) && s /= "\n"

main :: IO ()
main = do
  let input = "\n\n"
  putStrLn $ "Input: " ++ show input
  
  putStrLn $ "Condition 5 (all isSpace s && not (null s) && s /= \"\\n\"): " ++ show (testCondition5 input)
  
  putStrLn $ "removeLineComments result: " ++ show (removeLineComments input)
  
  -- Let's also test some other inputs
  putStrLn "\n--- Other tests ---"
  let test1 = "\n"
  putStrLn $ "Input: " ++ show test1
  putStrLn $ "Condition 5: " ++ show (testCondition5 test1)
  putStrLn $ "removeLineComments result: " ++ show (removeLineComments test1)
  
  let test2 = "  "
  putStrLn $ "\nInput: " ++ show test2
  putStrLn $ "Condition 5: " ++ show (testCondition5 test2)
  putStrLn $ "removeLineComments result: " ++ show (removeLineComments test2)