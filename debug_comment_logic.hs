import Utils

-- Debug the comment removal logic
main :: IO ()
main = do
  let s = "a'// comment"
  putStrLn $ "s: " ++ show s
  putStrLn $ "\"//\" `isInfixOf` s: " ++ show ("//" `isInfixOf` s)
  putStrLn $ "not (\"\\\"\" `isInfixOf` s): " ++ show (not ("\"" `isInfixOf` s))
  putStrLn $ "not ('\\n' `elem` s): " ++ show (not ('\n' `elem` s))
  let (before, after) = break (== '/') s
  putStrLn $ "before: " ++ show before
  putStrLn $ "after: " ++ show after
  putStrLn $ "null before: " ++ show (null before)
  putStrLn $ "all isSpace before: " ++ show (all isSpace before)