import qualified Utils as U (normalizeIndentation)

main :: IO ()
main = do
  let input = "  \t  a"
  let result = U.normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Contains tab: " ++ show ('\t' `elem` result)