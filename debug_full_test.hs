import Utils
import Data.List (isInfixOf)

-- Test the full test case
main :: IO ()
main = do
  let s = "\r"
  let withEmpty = s ++ "\n\n"
  putStrLn $ "s: " ++ show s
  putStrLn $ "withEmpty: " ++ show withEmpty
  let normalized = normalizeIndentation withEmpty
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "contains \n\n: " ++ show ("\n\n" `isInfixOf` normalized)
  putStrLn ""