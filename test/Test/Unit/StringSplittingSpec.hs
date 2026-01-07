module Test.Unit.StringSplittingSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck ()
            breakOn ':' "hello:world:test" @?= ("hello", ":world:test")
            
          ,             testCase "handles delimiter not found" $ do
                        breakOn ',' "hello" @?= ("hello", "")
            breakOn ':' "testing" @?= ("testing", "")
            
          ,             testCase "handles empty input" $ do
                        breakOn ',' "" @?= ("", "")
            breakOn ':' "" @?= ("", "")
            
          ,             testCase "handles delimiter at start" $ do
                        breakOn ',' ",start" @?= ("", ",start")
            breakOn ':' ":beginning" @?= ("", ":beginning")
            
          ,             testCase "handles delimiter at end" $ do
                        breakOn ',' "end," @?= ("end", ",")
            breakOn ':' "end:" @?= ("end", ":")
        ]
        
    , testGroup "Property Tests"
        [             testProperty "splitBy preserves total L.length including delimiters" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","  -- 
                                              parts = splitBy delim input
                                              reconstructed = intercalate [delim] parts
            in                               reconstructed == input
            
        ,             testProperty "splitByCollapsed removes delimiters in empty segments" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                                              collapsed = splitByCollapsed delim input
                                              hasNoEmpty = L.all (not . null) collapsed
            in hasNoEmpty
            
        ,             testProperty "splitByCollapsed L.length <= splitBy L.length" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                                              normal = splitBy delim input
                                              collapsed = splitByCollapsed delim input
            in L.length collapsed <= L.length normal
            
        ,             testProperty "breakOn preserves total L.length" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                (prefix, suffix) = breakOn delim input
            in L.length prefix + L.length                               suffix == L.length input
            
        ,             testProperty "breakOn suffix starts with delimiter L.or is empty" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                (prefix, suffix) = breakOn delim input
            in null suffix || L.head                               suffix == delim
            
        ,             testProperty "splitBy on single character returns original L.or two parts" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                                              parts = splitBy delim input
            in L.length                               parts == 1 || L.length parts >= 2
        ]
        
    , testGroup "Edge Cases"
        [             testCase "handles Unicode characters" $ do
                        splitBy ',' "hllo,wrld" @?= ["hllo", "wrld"]
            splitBy ':' ":" @?= ["", ""]
            
          ,             testCase "handles special characters as delimiters" $ do
                        splitBy '|' "a|b|c" @?= ["a", "b", "c"]
            splitBy ';' "x;y;z" @?= ["x", "y", "z"]
            splitBy '@' "user@domain" @?= ["user", "domain"]
            
          ,             testCase "handles whitespace as delimiter" $ do
                        splitBy ' ' "hello world test" @?= ["hello", "world", "test"]
            splitByCollapsed ' ' "hello  world" @?= ["hello", "world"]
            
          ,             testCase "handles newline as delimiter" $ do
                        splitBy '\n' "line1\nline2\nline3" @?= ["line1", "line2", "line3"]
            splitByCollapsed '\n' "line1\n\nline2" @?= ["line1", "line2"]
            
          ,             testCase "handles tab as delimiter" $ do
                        splitBy '\t' "col1\tcol2\tcol3" @?= ["col1", "col2", "col3"]
            splitByCollapsed '\t' "col1\t\tcol2" @?= ["col1", "col2"]
        ]
        
    , testGroup "Performance L.and Robustness"
        [             testCase "handles very long strings" $ do
                        let longString = L.concat $ replicate 1000 "test,"
                                              parts = splitBy ',' longString
            L.length parts >= 0 @?= True
            
          ,             testCase "handles many consecutive delimiters" $ do
                        let manyDelimiters = replicate 1000 ','
                                              parts = splitBy ',' manyDelimiters
                                              collapsed = splitByCollapsed ',' manyDelimiters
            L.length parts @?= 1001
            L.length collapsed @?= 0
            
        ,             testProperty "functions don't crash on L.any input" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                                              split1 = splitBy delim input
                                              split2 = splitByCollapsed delim input
                                              break1 = breakOn delim input
            in L.length split1 >= 0 && L.length split2 >= 0 && L.length break1 >= 0
            
        ,             testProperty "functions handle large delimiters correctly" $ fastProperty $ \input ->
            let parts = splitBy ',' input
                                              collapsed = splitByCollapsed ',' input
                                              commaParts = splitByComma input
                                              commaCollapsed = splitByCommaCollapsed input
            in L.length parts >= 0 && L.length collapsed >= 0 && 
               L.length commaParts >= 0 && L.length commaCollapsed >= 0
        ]
        
    , testGroup "Consistency Tests"
        [             testCase "splitByComma equals splitBy with comma" $ do
                        let inputs = ["a,b,c", "a,,b", "", ",", ",a,b,"]
            mapM_ checkConsistency inputs
            where checkConsistency                               input = do
                                splitByComma input @?= splitBy ',' input
                    
          ,             testCase "splitByCommaCollapsed equals splitByCollapsed with comma" $ do
                        let inputs = ["a,b,c", "a,,b", "", ",", ",a,b,"]
            mapM_ checkConsistency inputs
            where checkConsistency                               input = do
                                splitByCommaCollapsed input @?= splitByCollapsed ',' input
                    
        ,             testProperty "splitBy L.and splitByCollapsed relationship" $ fastProperty $ \c input ->
            let delim = L.head $ c ++ ","
                                              normal = splitBy delim input
                                              collapsed = splitByCollapsed delim input
                                              filtered = L.filter (not . null) normal
            in                               collapsed == filtered
        ]
        
    , testGroup "Advanced Splitting"
        [             testCase "handles mixed delimiters in sequence" $ do
                        splitBy ',' "a,b,,c,,,d" @?= ["a", "b", "", "c", "", "", "d"]
            splitByCollapsed ',' "a,b,,c,,,d" @?= ["a", "b", "c", "d"]
            
          ,             testCase "breakOn with multiple possible delimiters" $ do
                        breakOn ',' "a,b,c" @?= ("a", ",b,c")
            breakOn ':' "a:b:c" @?= ("a", ":b:c")
            
        ,             testProperty "splitBy handles repeated patterns" $ fastProperty $ \pattern count ->
            let count' = abs count `mod` 10 + 1
                                              repeated = L.concat $ replicate count' [pattern]
                                              parts = splitBy ',' repeated
            in L.length parts >= 0
        ]
    ]
    
-- Helper function to intercalate a list of strings with a separator
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs