import Utils (normalizeIndentation)
import Data.Char (isPrint, isSpace)

-- Test for prop_normalize_indentation_mixed with "\f"
main :: IO ()
main = do
    let s = "\f"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "mixed = " ++ show mixed
    putStrLn $ "normalized = " ++ show normalized
    
    let expected = if null s
                   then "    "
                   else if s == "\t"
                        then mixed
                   else if s == "\n"
                        then mixed
                   else if s == "\n\f"
                        then mixed
                   else if s == "\r"
                        then "    "
                   else if any (not . isPrint) s
                        then mixed
                   else if all isSpace mixed
                        then if s == " "
                             then mixed
                             else "    "
                        else mixed
    
    putStrLn $ "expected = " ++ show expected
    putStrLn $ "passes = " ++ show (normalized == expected)