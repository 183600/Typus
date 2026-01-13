module Main where
import Utils

main :: IO ()
main = do
    let input = "' // comment"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show (removeLineComments input)