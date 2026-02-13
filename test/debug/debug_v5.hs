import Data.Char (isSpace)
import Data.List (isPrefixOf)

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith s c = last s == c

main :: IO ()
main = do
  let input = "\t\t\v\t"
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "all isSpace input: " ++ show (all isSpace input)
  putStrLn $ "not (all isSpace input): " ++ show (not (all isSpace input))
  putStrLn $ "startsWithTabs: " ++ show ("\t\t" `isPrefixOf` input)
  putStrLn $ "endsWithTab: " ++ show (endsWith input '\t')
  putStrLn $ "All conditions: " ++ show (("\t\t" `isPrefixOf` input && endsWith input '\t' && not (all isSpace input)))