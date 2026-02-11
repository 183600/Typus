import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    -- Test case 1: s = "a"
    let s1 = "a"
    let mixed1 = "\t  \t  " ++ s1 ++ "  \t  "
    let normalized1 = normalizeIndentation mixed1
    putStrLn $ "Test 1 - s = \"a\":"
    putStrLn $ "  Input: " ++ show mixed1
    putStrLn $ "  Output: " ++ show normalized1
    putStrLn $ "  null s: " ++ show (null s1)
    putStrLn $ "  all isSpace mixed: " ++ show (all isSpace mixed1)
    putStrLn $ ""
    
    -- Test case 2: s = ""
    let s2 = ""
    let mixed2 = "\t  \t  " ++ s2 ++ "  \t  "
    let normalized2 = normalizeIndentation mixed2
    putStrLn $ "Test 2 - s = \"\":"
    putStrLn $ "  Input: " ++ show mixed2
    putStrLn $ "  Output: " ++ show normalized2
    putStrLn $ "  null s: " ++ show (null s2)
    putStrLn $ "  all isSpace mixed: " ++ show (all isSpace mixed2)
    putStrLn $ ""
    
    -- Test case 3: multiline with [""]
    let lines3 = [""]
    let withMixed3 = map ("\t  " ++) lines3
    let normalized3 = normalizeIndentation (unlines withMixed3)
    putStrLn $ "Test 3 - lines = [\"\"]:"
    putStrLn $ "  Input lines: " ++ show withMixed3
    putStrLn $ "  Input string: " ++ show (unlines withMixed3)
    putStrLn $ "  Output: " ++ show normalized3