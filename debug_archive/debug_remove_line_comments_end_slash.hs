import Utils (removeLineComments)

main :: IO ()
main = do
  -- Test the specific failing case
  let s = "/"
      withComment = s ++ "// comment"
      processed = removeLineComments withComment
  putStrLn $ "s: " ++ show s
  putStrLn $ "withComment: " ++ show withComment
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "processed == s: " ++ show (processed == s)