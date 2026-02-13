import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  let testInput = "\r"
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  
  putStrLn $ "Mixed string: " ++ show mixed
  putStrLn $ "Mixed string chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") mixed
  
  let prefix = "\t  \t  "
  let suffix = "  \t  "
  putStrLn $ "Prefix: " ++ show prefix
  putStrLn $ "Prefix chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") prefix
  putStrLn $ "Suffix: " ++ show suffix
  putStrLn $ "Suffix chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") suffix
  
  putStrLn $ "Prefix check: " ++ show (prefix `isPrefixOf` mixed)
  putStrLn $ "Suffix check: " ++ show (suffix `isSuffixOf` mixed)
  
  let dropped = drop 4 mixed
  putStrLn $ "After drop 4: " ++ show dropped
  putStrLn $ "After drop 4 chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") dropped
  
  let takenLength = length mixed - 4 - 6
  putStrLn $ "Take length: " ++ show takenLength
  let dropped2 = drop 2 dropped  -- Drop the two spaces after the prefix
  putStrLn $ "After drop 2 more: " ++ show dropped2
  putStrLn $ "After drop 2 more chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") dropped2
  let taken = take 1 dropped2
  putStrLn $ "After take: " ++ show taken
  putStrLn $ "After take chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") taken