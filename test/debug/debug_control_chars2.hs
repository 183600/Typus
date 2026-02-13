main :: IO ()
main = do
  let input = "\t\t\v\t"
  let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL']
  let hasControlChar = any (`elem` controlChars) input
  let hasTab = '\t' `elem` input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Has control char: " ++ show hasControlChar
  putStrLn $ "Has tab: " ++ show hasTab
  putStrLn $ "Control chars condition matches: " ++ show (hasControlChar && hasTab)
  
  putStrLn "\nChecking which control chars are in input:"
  mapM_ (\c -> if c `elem` controlChars then putStrLn $ show c ++ " is a control char" else putStrLn $ show c ++ " is not a control char") input