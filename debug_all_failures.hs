import Utils (normalizeIndentation)
import Data.List (isInfixOf)

-- Test cases based on the failing properties
main :: IO ()
main = do
    -- Test normalize_indentation_empty_lines
    putStrLn "Testing normalize_indentation_empty_lines:"
    let s1 = ""
    let withEmpty1 = s1 ++ "\n\n"
    let normalized1 = normalizeIndentation withEmpty1
    putStrLn $ "  s = " ++ show s1
    putStrLn $ "  withEmpty = " ++ show withEmpty1
    putStrLn $ "  normalized = " ++ show normalized1
    putStrLn $ "  Expected = \"    \""
    putStrLn $ "  Test passes: " ++ show (normalized1 == "    ")
    
    -- Test normalize_indentation_relative
    putStrLn "\nTesting normalize_indentation_relative:"
    -- Test with single space
    let s2 = " "
    let normalized2 = normalizeIndentation s2
    putStrLn $ "  s = " ++ show s2
    putStrLn $ "  normalized = " ++ show normalized2
    putStrLn $ "  Expected = \" \""
    putStrLn $ "  Test passes: " ++ show (normalized2 == " ")
    
    -- Test normalize_indentation_mixed
    putStrLn "\nTesting normalize_indentation_mixed:"
    -- Test with empty string
    let s3 = ""
    let mixed3 = "\t  \t  " ++ s3 ++ "  \t  "
    let normalized3 = normalizeIndentation mixed3
    putStrLn $ "  s = " ++ show s3
    putStrLn $ "  mixed = " ++ show mixed3
    putStrLn $ "  normalized = " ++ show normalized3
    putStrLn $ "  Expected = \"    \""
    putStrLn $ "  Test passes: " ++ show (normalized3 == "    ")
    
    -- Test with tab
    let s4 = "\t"
    let mixed4 = "\t  \t  " ++ s4 ++ "  \t  "
    let normalized4 = normalizeIndentation mixed4
    putStrLn $ "  s = " ++ show s4
    putStrLn $ "  mixed = " ++ show mixed4
    putStrLn $ "  normalized = " ++ show normalized4
    putStrLn $ "  Expected = " ++ show mixed4
    putStrLn $ "  Test passes: " ++ show (normalized4 == mixed4)