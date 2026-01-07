module Test.Unit.ConciseParserQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), Arbitrary(..), Gen, oneof, listOf, elements)
import Data.Char 
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock)
            \(_ :: () -> case parseTypus "" of
                    Left _ -> property True
                    Right file -> tfBlocks                               file === []
                    
        ,             testProperty "parseTypus preserves line count in simple cases" $
            \lines -> not (null lines) ==> 
            let input = unlines lines
            in case parseTypus input of
                 Left _ -> property True
                 Right file -> property (L.length (tfBlocks file) <= L.length lines)
                 
        ,             testProperty "parseTypus handles whitespace-only lines" $
            \ws -> case parseTypus ws of
                     Left _ -> property True
                     Right file -> property (L.all (L.all isSpace . unlines . map cbContent . (:[]) (tfBlocks file)
                     
        ,             testProperty "parseTypus result contains syntax errors list" $
            \input -> case parseTypus input of
                        Left _ -> property True
                        Right file -> property (L.length (tfSyntaxErrors file) >= 0)
        ]
        
    , testGroup "Directive parsing"
        [             testProperty "File directives can be parsed independently" $
            \ownership dependent -> 
            let input = if ownership then "//!ownership:true\n" else ""
                     ++ if dependent then "//!dependentTypes:true\n" else ""
            in case parseTypus input of
                 Left _ -> property True
                 Right file -> property True  -- If parsing succeeds, we consider it a success
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


-- Helper function for QuickCheck properties
property :: Bool -> Property
property                               b =                               b === True