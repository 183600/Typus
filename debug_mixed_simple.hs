import qualified Utils as U
import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "Testing specific failing case:"
  let testInput = "a"
  let mixed = "  \t  " ++ testInput
  let result = U.normalizeIndentation mixed
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Contains tab: " ++ show ("\t" `isInfixOf` result)
  putStrLn $ "Test passes: " ++ show (not ("\t" `isInfixOf` result))
  
  putStrLn "\nTesting character by character:"
  putStrLn $ "Result chars: " ++ show (map (\c -> if c == '\t' then "TAB" else show c) result)