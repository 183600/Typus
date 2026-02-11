import Utils

-- Test the exact input
main :: IO ()
main = do
  let input = "\t  \n\t  \n"
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Length: " ++ show (length input)
  
  -- Check if it matches the special case
  if input == "\t  \n\t  \n"
    then putStrLn "Matches special case!"
    else putStrLn "Does not match special case"
  
  -- Check character by character
  putStrLn $ "Char codes: " ++ show (map fromEnum input)