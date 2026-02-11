import Utils

-- Test the exact test case
main :: IO ()
main = do
  let s = "a'"
  let withComment = s ++ "// comment"
  let processed = Utils.removeLineComments withComment
  putStrLn $ "s: " ++ show s
  putStrLn $ "withComment: " ++ show withComment
  putStrLn $ "processed: " ++ show processed