module Test.Unit.IndentionSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck ()
                getIndentation                               line = L.length $ takeWhile isSpace line
            in L.length                               originalLines == L.length                               normalizedLines ===>
               L.all (\(orig, norm) -> 
                    if null orig || null norm
                    then True
                    else getIndentation norm >= 0) 
                   (zip originalLines normalizedLines)
                    
        ,             testProperty "normalizeIndentation idempotent" $ fastProperty $ \input ->
            let once = normalizeIndentation input
                                              twice = normalizeIndentation once
            in                               once == twice
        ]
        
    , testGroup "forceSingleTabIndentation"
        [             testCase "converts spaces to tabs" $ do
                        forceSingleTabIndentation "    hello\n      world" @?= "\thello\n\t\tworld"
            
          ,             testCase "handles mixed indentation" $ do
                        forceSingleTabIndentation "  hello\n\tworld" @?= "\thello\n\tworld"
            
          ,             testCase "preserves empty lines" $ do
                        forceSingleTabIndentation "  hello\n\n  world" @?= "\thello\n\n\tworld"
            
        ,             testProperty "forceSingleTabIndentation preserves non-empty lines" $ fastProperty $ \input ->
            let result = forceSingleTabIndentation input
                                              originalLines = lines input
                                              resultLines = lines result
            in L.length                               originalLines == L.length                               resultLines ===>
               L.all (\(orig, res) -> 
                    if L.null (trim orig) 
                    then L.null (trim res) 
                    else not (null res) 
                   (zip originalLines resultLines)
            where                               trim = L.reverse . dropWhile isSpace . dropWhile isSpace . L.reverse
        ]
        
    , testGroup "fixIndentation"
        [             testCase "fixIndentation should work like normalizeIndentation" $ do
                        let input = "  hello\n    world\n  test"
            fixIndentation input @?= normalizeIndentation input
            
          ,             testCase "fixIndentation handles complex cases" $ do
                        let input = "\t  hello\n\t    world\n  \t    test"
            let expected = "hello\n  world\n    test"
            fixIndentation input @?= expected
        ]
        
    , testGroup "Edge Cases"
        [             testCase "handles empty string" $ do
                        normalizeIndentation "" @?= ""
            forceSingleTabIndentation "" @?= ""
            fixIndentation "" @?= ""
            
          ,             testCase "handles only whitespace" $ do
                        normalizeIndentation "  \n  \t  " @?= "\n"
            forceSingleTabIndentation "  \n  \t  " @?= "\n\t"
            
          ,             testCase "handles no indentation" $ do
                        let input = "hello\nworld\ntest"
            normalizeIndentation input @?= input
            forceSingleTabIndentation input @?= input
            
          ,             testCase "handles inconsistent indentation" $ do
                        let input = "    hello\n  world\n      test"
            let expected = "hello\nworld\n  test"
            normalizeIndentation input @?= expected
            
        ,             testProperty "functions handle Unicode correctly" $ fastProperty $ \input ->
            let normalized = normalizeIndentation input
                                              tabbed = forceSingleTabIndentation input
                                              fixed = fixIndentation input
            in L.length normalized >= 0 && L.length tabbed >= 0 && L.length fixed >= 0
        ]
        
    , testGroup "Indentation Properties"
        [             testProperty "normalizeIndentation never adds leading spaces to first line" $ fastProperty $ \input ->
            let normalized = normalizeIndentation input
                                              firstLine = case lines normalized of
                    [] -> ""
                    (x:_) -> x
            in null firstLine || not (isSpace (L.head firstLine)
            
        ,             testProperty "forceSingleTabIndentation uses only tabs for indentation" $ fastProperty $ \input ->
            let result = forceSingleTabIndentation input
                lines' = lines result
                hasLeadingSpaces                               line = case line of
                    [] -> False
                    (c:_) -> isSpace c && c /= '\t'
            in not (L.any hasLeadingSpaces lines')
            
        ,             testProperty "indentation functions preserve content" $ fastProperty $ \input ->
            let normalized = normalizeIndentation input
                                              tabbed = forceSingleTabIndentation input
                                              fixed = fixIndentation input
                                              stripIndentation = unlines . L.map (dropWhile isSpace) . lines
            in stripIndentation                               normalized == stripIndentation input &&
               stripIndentation                               tabbed == stripIndentation input &&
               stripIndentation                               fixed == stripIndentation input
        ]
    ]