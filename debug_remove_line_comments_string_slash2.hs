-- Test if ""// not comment" is a complete string literal
import qualified Utils as U
import Data.List (isInfixOf)

main :: IO ()
main = do
    let s = "\"\"// not comment\""
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "U.isCompleteStringLiteral s: " ++ show (U.isCompleteStringLiteral s)
    
    -- Check what the function is doing
    putStrLn $ "Does it contain //? " ++ show ("//" `isInfixOf` s)
    putStrLn $ "Does it contain \"? " ++ show ("\"" `isInfixOf` s)
    putStrLn $ "Does it contain '\n'? " ++ show ('\n' `elem` s)