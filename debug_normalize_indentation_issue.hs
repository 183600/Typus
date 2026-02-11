import qualified Utils as U

main :: IO ()
main = do
  let s = "a"
  let mixed = "\t  \t  " ++ s ++ "  \t  "
  putStrLn $ "Input string: " ++ show mixed
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show mixed
  putStrLn $ "Equal: " ++ show (normalized == mixed)