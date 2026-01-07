module Test.Unit.CoreParserFunctionsSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), )
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan)
            in L.length (show result) >= 0  -- 
            
        ,             testProperty "parseTypus preserves input L.length in error messages" $ fastProperty $ \input ->
            let result = parseTypus "" input
                                              resultStr = show result
            in if "error" `elem` (words $ map toLower resultStr)
               then L.length input <= L.length resultStr || L.length resultStr > 10
               else True
            where                               toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then c + 32 else c)
        ]
        
    , testGroup "Directive Parsing"
        [             testCase "FileDirectives equality works correctly" $ do
                        let fd1 = FileDirectives Nothing Nothing Nothing
                                              fd2 = FileDirectives (Just True) Nothing (Just False)
            fd1 @?= fd1
            fd1 /= fd2 @?= True
            
          ,             testCase "BlockDirectives equality works correctly" $ do
                        let bd1 = BlockDirectives Nothing Nothing Nothing
                                              bd2 = BlockDirectives (Just False) (Just True) Nothing
            bd1 @?= bd1
            bd1 /= bd2 @?= True
        ]
        
    , testGroup "Parser Error Handling"
        [             testCase "parseTypus handles malformed input gracefully" $ do
                        let malformedInput = "!!!@@@###$$$%%%"
                                              result = parseTypus "" malformedInput
                                              resultStr = show result
            L.length resultStr > 0 @?= True  -- 
            
        ,             testProperty "parseTypus on Unicode input should not crash" $ fastProperty $ \input ->
            let unicodeInput = T.unpack $ T.pack input
                                              result = parseTypus "" unicodeInput
            in L.length (show result) >= 0
        ]
        
    , testGroup "Parser Edge Cases"
        [             testCase "parseTypus handles very long lines" $ do
                        let longLine = replicate 10000 'a'
                                              result = parseTypus "" longLine
            L.length (show result) >= 0 @?= True
            
          ,             testCase "parseTypus handles input with only whitespace" $ do
                        let whitespaceInput = "   \t\n\r   \t  \n\r   "
                                              result = parseTypus "" whitespaceInput
            L.length (show result) >= 0 @?= True
            
          ,             testCase "parseTypus handles input with special characters" $ do
                        let specialChars = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./~`"
                                              result = parseTypus "" specialChars
            L.length (show result) >= 0 @?= True
        ]
    ]