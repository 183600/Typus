import Utils
import Data.List (isPrefixOf)

-- 测试失败的测试用例
main :: IO ()
main = do
  putStrLn "Testing removeComments multiline function..."
  
  -- 测试多行注释
  putStrLn "\n=== Test multiline comments ==="
  let before = "code before"
  let after = "code after"
  let commentLines = [ "/* comment line 1 */", "/* comment line 2 */" ]
  let comments = unlines commentLines
  let codeWithComment = unlines [before, comments, after]
  let withoutComments = removeComments codeWithComment
  
  putStrLn $ "Input:\n" ++ codeWithComment
  putStrLn $ "Output:\n" ++ withoutComments
  putStrLn $ "Has /* prefix: " ++ show (any (isPrefixOf "/*") (lines withoutComments))
  
  -- 测试单行注释
  putStrLn "\n=== Test single line comment ==="
  let singleLineComment = "code before /* comment */ code after"
  let singleLineResult = removeComments singleLineComment
  putStrLn $ "Input: " ++ show singleLineComment
  putStrLn $ "Output: " ++ show singleLineResult