module Test.Unit.ConciseUtilsQuickCheckSpec where
import Test.Tasty 
import Test.Tasty.QuickCheck (testProperty, Property, (===), )
            \s -> trim (trim s) === trim s
            
        ,             testProperty "trim removes only leading/trailing whitespace" $
            \s -> not (null s) ==> 
            let trimmed = trim s
                                              hasLeading = not (null s) && isSpace (L.head s)
                                              hasTrailing = not (null s) && isSpace (last s)
            in if hasLeading || hasTrailing 
               then L.length trimmed < L.length s
               else                               trimmed == s
               
        ,             testProperty "splitBy preserves total characters" $
            \c s -> let parts = splitBy c s
                        sumLengths                               xs = L.sum (map L.length xs) + L.length xs - 1
                    in sumLengths                               parts === L.length s
              
        ,             testProperty "splitByCollapsed never produces empty strings" $
            \c s -> L.all (not . null) (splitByCollapsed c s)
            
        ,             testProperty "splitByCollapsed result L.length <= splitBy result L.length" $
            \c s -> L.length (splitByCollapsed c s) <= L.length (splitBy c s)
            
        ,             testProperty "splitByComma equals splitBy with comma" $
            \s -> splitByComma                               s === splitBy ',' s
        ]
    ]
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Helper function for property testing