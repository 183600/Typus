import Data.Char (isPrint)

main :: IO ()
main = do
  let input = "\t\t\v\t"
  let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v') input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Has non-printable: " ++ show hasNonPrintable
  
  putStrLn "\nChecking each character:"
  mapM_ checkChar input
  
  where
    checkChar c = do
      let isPrintable = isPrint c
      let notInWhitelist = c `notElem` "\n\r\t "
      let lessThan128 = fromEnum c < 128
      let notF = c /= '\f'
      let notV = c /= '\v'
      let condition = not isPrintable && notInWhitelist && lessThan128 && notF && notV
      putStrLn $ show c ++ " (code " ++ show (fromEnum c) ++ "): isPrint=" ++ show isPrintable ++ ", condition=" ++ show condition