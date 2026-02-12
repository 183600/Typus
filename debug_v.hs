import Data.Char (isPrint)

main :: IO ()
main = do
  let input = "\v"
  let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL']
  let hasControlChar = any (`elem` controlChars) input
  let hasTab = '\t' `elem` input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Has control char: " ++ show hasControlChar
  putStrLn $ "Has tab: " ++ show hasTab
  putStrLn $ "Condition matches: " ++ show (hasControlChar && hasTab)
  
  putStrLn "\nFor \t\t\v\t:"
  let input2 = "\t\t\v\t"
  let hasControlChar2 = any (`elem` controlChars) input2
  let hasTab2 = '\t' `elem` input2
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Has control char: " ++ show hasControlChar2
  putStrLn $ "Has tab: " ++ show hasTab2
  putStrLn $ "Condition matches: " ++ show (hasControlChar2 && hasTab2)