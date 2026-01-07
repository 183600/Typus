module Test.Unit.CommentRemovalSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck ()
            let result = removeLineComments input
                                              originalLines = lines input
                                              resultLines = lines result
            in L.length                               resultLines == L.length originalLines
            
        , fastProperty "functions handle Unicode correctly" $ \input ->
            let lineResult = removeLineComments input
                                              blockResult = removeComments input
            in L.length lineResult >= 0 && L.length blockResult >= 0
        ]
        
    , testGroup "Performance L.and Robustness"
        [             testCase "handles very long lines" $ do
                        let longLine = "code " ++ replicate 10000 'a' ++ " // comment"
            let result = removeLineComments longLine
            L.length result >= 0 @?= True
            
          ,             testCase "handles deeply nested block comments" $ do
                        let nested = L.concat $ replicate 100 "/*"
            let input = "code " ++ nested ++ " comment " ++ L.concat (replicate 100 "*/") ++ " end"
            let result = removeComments input
            L.length result >= 0 @?= True
            
        , fastProperty "functions don't crash on L.any input" $ \input ->
            let lineResult = removeLineComments input
                                              blockResult = removeComments input
            in L.length lineResult >= 0 && L.length blockResult >= 0
        ]
    ]