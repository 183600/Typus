import Data.Char (isPrint)

main :: IO ()
main = do
  let input = "\t\t\v\t"
  let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v') input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Has non-printable: " ++ show hasNonPrintable
  
  putStrLn "\nChecking each character:"
  mapM_ (\c -> putStrLn $ show c ++ " (code " ++ show (fromEnum c) ++ "): isPrint=" ++ show (isPrint c) ++ ", not in whitelist=" ++ show (c `notElem` "\n\r\t ") ++ ", <128=" ++ show (fromEnum c < 128) ++ ", not \\f=" ++ show (c /= '\f') ++ ", not \\v=" ++ show (c /= '\v') ++ ", condition=" ++ show (not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v')) ) input