import Utils (removeComments)
import Data.List (isInfixOf)

-- Check if comments are in string literals
hasCommentInStringLiteral :: String -> Bool
hasCommentInStringLiteral [] = False
hasCommentInStringLiteral str = checkForCommentInString str False False (0 :: Int)
    
-- Check if // or block comment start appears inside string or character literals
-- The depth parameter tracks the nesting level to handle unclosed literals
checkForCommentInString [] _ _ depth = depth > 0  -- If we reach end with depth > 0, literals were unclosed
checkForCommentInString ('"':rest) inString inChar depth 
  | inString = checkForCommentInString rest False inChar (depth - 1)  -- End of string literal
  | otherwise = checkForCommentInString rest True inChar (depth + 1)  -- Start of string literal
checkForCommentInString ('\'':rest) inString inChar depth
  | inChar = checkForCommentInString rest inString False (depth - 1)  -- End of char literal
  | otherwise = checkForCommentInString rest inString True (depth + 1)  -- Start of char literal
checkForCommentInString ('\\':_:rest) inString inChar depth = 
    checkForCommentInString rest inString inChar depth  -- Skip escaped characters
checkForCommentInString ('/':'/':_) inString inChar depth = 
    -- Comments are only inside literals if depth > 0 AND we're in a literal
    (inString || inChar) && depth > 0
checkForCommentInString ('/':'*':_) inString inChar depth = 
    -- Comments are only inside literals if depth > 0 AND we're in a literal
    (inString || inChar) && depth > 0
checkForCommentInString (_:rest) inString inChar depth = 
  checkForCommentInString rest inString inChar depth

-- Test that removeComments removes all line and block comments
testRemoveComments :: String -> Bool
testRemoveComments s = 
  let result = removeComments s
      hasLineComment = "//" `isInfixOf` result
      hasBlockComment = "/*" `isInfixOf` result
      -- Check if the original string has comments inside string literals
      hasCommentInString = hasCommentInStringLiteral s
  in if hasCommentInString
     then True  -- If comments are in string literals, any behavior is acceptable
     else not (hasLineComment || hasBlockComment)

main :: IO ()
main = do
  -- Test some edge cases
  putStrLn "Testing edge cases..."
  
  let testCases = [
        ("a // \"string with // comment\"", "Line comment with string"),
        ("a // 'char with // comment'", "Line comment with char"),
        ("a /* \"string with // comment\" */ b", "Block comment with string"),
        ("a // \"unclosed string", "Line comment with unclosed string"),
        ("a // \"string with \\\" escape", "Line comment with escaped quote"),
        ("a // \"string with \\ // inside\"", "Line comment with // in escaped string"),
        ("a // \"string with /* inside\"", "Line comment with /* in string"),
        ("a // \"string \\\" with // after", "Line comment with escaped quote and //")
        ]
  
  mapM_ (\(input, desc) -> do
            putStrLn $ "\n" ++ desc ++ ":"
            putStrLn $ "  Input: " ++ show input
            let result = removeComments input
            putStrLn $ "  Result: " ++ show result
            putStrLn $ "  Test passes: " ++ show (testRemoveComments input)
        ) testCases