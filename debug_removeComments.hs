import Utils (removeComments)
import Test.QuickCheck
import Data.List (isInfixOf)

-- Test that removeComments removes all line and block comments
testRemoveComments :: String -> Bool
testRemoveComments s = 
  let result = removeComments s
      hasLineComment = "//" `isInfixOf` result
      hasBlockComment = "/*" `isInfixOf` result
  in not (hasLineComment || hasBlockComment)

main :: IO ()
main = do
  -- Test some simple cases
  putStrLn "Testing simple cases..."
  print $ testRemoveComments "a // comment"
  print $ testRemoveComments "a /* comment */ b"
  print $ testRemoveComments "// only comment"
  print $ testRemoveComments "/* only comment */"
  
  -- Run quickcheck to find a failing case
  putStrLn "\nRunning QuickCheck to find failing case..."
  quickCheck testRemoveComments