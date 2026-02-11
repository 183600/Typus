import qualified Utils as U
import Test.QuickCheck

prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = U.normalizeIndentation mixed
  in property $ not ("\t" `isInfixOf` normalized)

main :: IO ()
main = do
  putStrLn "Testing specific failing case:"
  let testInput = "a"
  let mixed = "  \t  " ++ testInput
  let result = U.normalizeIndentation mixed
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Contains tab: " ++ show ('\t' `isInfixOf` result)
  putStrLn $ "Test passes: " ++ show (not ("\t" `isInfixOf` result))
  
  putStrLn "\nRunning QuickCheck with specific seed:"
  quickCheckWith stdArgs {replay = Just (SMGen 5170823637723241295 11704665671693045983,1)} prop_normalize_indentation_mixed