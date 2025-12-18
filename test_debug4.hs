import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
  let before = ""
      middle = ""
      after = ""
      content = before ++ "/* outer /* inner */" ++ middle ++ after
      result = removeComments content
  putStrLn $ "content: " ++ show content
  putStrLn $ "result: " ++ show result
  putStrLn $ "not (\"/* outer\" `isInfixOf` result): " ++ show (not ("/* outer" `isInfixOf` result))
  putStrLn $ "not (\"/* inner\" `isInfixOf` result): " ++ show (not ("/* inner" `isInfixOf` result))
  putStrLn $ "middle `isInfixOf` result: " ++ show (middle `isInfixOf` result)
  putStrLn $ "after `isInfixOf` result: " ++ show (after `isInfixOf` result)
