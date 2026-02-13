import qualified Utils as U

main :: IO ()
main = do
  let s = "'&"
  putStrLn $ "Input s: " ++ show s
  
  let withComment = s ++ "// comment"
  putStrLn $ "withComment: " ++ show withComment
  
  let processed = U.removeLineComments withComment
  putStrLn $ "processed: " ++ show processed
  
  putStrLn $ "\nExpected: " ++ show s
  putStrLn $ "Test passes: " ++ show (processed == s)