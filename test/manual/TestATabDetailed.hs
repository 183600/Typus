import qualified Utils as U
import Data.Char (isPrint, isSpace)
import qualified Data.List as L

main :: IO ()
main = do
  let testInput = "a\t"
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Length: " ++ show (length testInput)
  putStrLn $ "Null: " ++ show (null testInput)
  putStrLn $ "Length 1 and not space: " ++ show (length testInput == 1 && not (isSpace (head testInput)))
  putStrLn $ "Starts with \t\t and ends with \t: " ++ show ("\t\t" `isPrefixOf` testInput && last testInput == '\t')
  putStrLn $ "Mixed format: " ++ show ("\t  \t  " `isPrefixOf` testInput && "  \t  " `L.isSuffixOf` testInput && length testInput >= 9)
  putStrLn $ "Starts with \t and not all space: " ++ show (length testInput >= 2 && head testInput == '\t' && not (all isSpace testInput))
  putStrLn $ "Starts with \t and next not space: " ++ show (length testInput >= 2 && head testInput == '\t' && not (isSpace (testInput !! 1)))
  putStrLn $ "Length 1 and not printable: " ++ show (length testInput == 1 && not (isPrint (head testInput)) && head testInput `notElem` [' ', '\n', '\r', '\t'])
  putStrLn $ "Is \v: " ++ show (testInput == "\v")
  putStrLn $ "Is \r: " ++ show (testInput == "\r")
  putStrLn $ "Is \t: " ++ show (testInput == "\t")
  putStrLn $ "Is a\t: " ++ show (testInput == "a\t")
  
  let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f' && c /= '\v' && c /= '\b' && c /= '\a') testInput
  putStrLn $ "Has non-printable: " ++ show hasNonPrintable
  
  let normalized = U.normalizeIndentation testInput
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if normalized == "a " then "PASSED" else "FAILED"

  where
    isPrefixOf = flip L.isPrefixOf