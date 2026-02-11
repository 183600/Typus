import Utils

-- Test the failing case
main :: IO ()
main = do
  let input = "a'"
  putStrLn $ "input: " ++ show input
  putStrLn $ "removeLineComments input: " ++ show (Utils.removeLineComments input)