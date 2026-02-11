import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing removeLineComments with \"\\n\":"
  print $ U.removeLineComments "\n"
  
  putStrLn "Testing removeComments with \"//\\n \":"
  print $ U.removeComments "//\n "
  
  putStrLn "Testing removeComments with \"//\\n\":"
  print $ U.removeComments "//\n"