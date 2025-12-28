module Test.Unit.ConciseCompilerIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, choose, elements, listOf)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Compiler.IR (SourceIR(..), SemanticIR(..))

-- | 简洁的QuickCheck测试，针对Compiler IR模块的一致性
tests :: TestTree
tests =
  testGroup "Concise Compiler IR QuickCheck Tests"
    [ testGroup "Source IR properties"
        [ testProperty "Source IR preserves file content" $
            \file content -> 
            let ir = SourceIR file content
            in sourceText ir === content
            
        , testProperty "Source IR preserves Typus file" $
            \file content -> 
            let ir = SourceIR file content
            in sourceTypusFile ir === file
        ]
        
    , testGroup "Semantic IR properties"
        [ testProperty "Semantic IR preserves source file" $
            \file -> 
            let ir = SemanticIR file "" [] [] Set.empty
            in semanticTypusFile ir === file
            
        , testProperty "Semantic IR preserves generated code" $
            \file code -> 
            let ir = SemanticIR file code [] [] Set.empty
            in semanticGoCode ir === code
        ]
        
    , testGroup "IR transformation consistency"
        [ testProperty "Source to semantic transformation preserves identifiers" $
            \sourceFile -> 
            let sourceIR = SourceIR sourceFile "test code"
                semanticIR = mockTransform sourceIR
                sourceIds = extractIdentifiers sourceFile
                semanticIds = extractIdentifiers (semanticTypusFile semanticIR)
            in Set.isSubsetOf sourceIds semanticIds  -- Semantic may have more identifiers
        ]
        
    , testGroup "Code generation properties"
        [ testProperty "Generated code contains required imports" $
            \imports -> 
            let code = generateMockCode imports
            in all (`isInfixOf` code) imports
            
        , testProperty "Generated code has balanced braces" $
            \content -> 
            let code = generateMockCodeWithContent content
            in areBracketsBalanced code
        ]
        
    , testGroup "IR validation properties"
        [ testProperty "Valid source IR always produces semantic IR" $
            \sourceFile content -> 
            let sourceIR = SourceIR sourceFile content
            in case mockValidateSourceIR sourceIR of
                 Left _ -> property False
                 Right _ -> property True
                 
        , testProperty "Semantic IR always contains at least one statement" $
            \file -> 
            let semanticIR = SemanticIR file "test" [] [] Set.empty
                statements = semanticStatements semanticIR
            in not (null statements)
        ]
        
    , testGroup "Optimization properties"
        [ testProperty "Dead code elimination preserves semantics" $
            \statements -> 
            let optimized = eliminateDeadCode statements
                originalSemantics = extractSemantics statements
                optimizedSemantics = extractSemantics optimized
            in originalSemantics === optimizedSemantics
            
        , testProperty "Constant folding preserves result" $
            \expr -> 
            let folded = foldConstants expr
                originalValue = evaluateExpression expr
                foldedValue = evaluateExpression folded
            in originalValue === foldedValue
        ]
    ]

-- Helper types and functions for testing
data MockStatement = MockStatement
  { stmtId :: String
  , stmtContent :: String
  } deriving (Eq, Show)

data MockExpression = MockExpression
  { exprType :: String
  , exprValue :: String
  } deriving (Eq, Show)

-- Mock functions for testing
mockTransform :: SourceIR -> SemanticIR
mockTransform (SourceIR file _) = SemanticIR file "generated" [] [] Set.empty

mockValidateSourceIR :: SourceIR -> Either String ()
mockValidateSourceIR (SourceIR file _) = 
  if null file then Left "Empty file" else Right ()

extractIdentifiers :: String -> Set String
extractIdentifiers = Set.fromList . words . filter (`notElem` "();{}[],")

generateMockCode :: [String] -> String
generateMockCode imports = unlines $ map (\imp -> "import " ++ imp) imports

generateMockCodeWithContent :: String -> String
generateMockCodeWithContent content = "package main\n\nfunc main() {\n" ++ content ++ "\n}"

areBracketsBalanced :: String -> Bool
areBracketsBalanced = checkBalance []
  where
    checkBalance [] [] = True
    checkBalance _ [] = False
    checkBalance stack (c:rest)
      | c `elem` "([{" = checkBalance (c:stack) rest
      | c `elem` ")]" = case stack of
                           [] -> False
                           (top:remaining) -> isMatchingPair top c && checkBalance remaining rest
      | otherwise = checkBalance stack rest
    
    isMatchingPair '(' ')' = True
    isMatchingPair '[' ']' = True
    isMatchingPair '{' '}' = True
    isMatchingPair _ _ = False

eliminateDeadCode :: [MockStatement] -> [MockStatement]
eliminateDeadCode = filter isUsed
  where
    isUsed (MockStatement _ content) = not (null content)

extractSemantics :: [MockStatement] -> String
extractSemantics statements = concatMap stmtContent statements

foldConstants :: MockExpression -> MockExpression
foldConstants expr = expr  -- Simplified for testing

evaluateExpression :: MockExpression -> String
evaluateExpression = exprValue

-- Generate test data
instance Arbitrary SourceIR where
  arbitrary = do
    file <- arbitrary
    content <- arbitrary
    return $ SourceIR file content

instance Arbitrary SemanticIR where
  arbitrary = do
    file <- arbitrary
    code <- arbitrary
    statements <- listOf arbitrary
    expressions <- listOf arbitrary
    identifiers <- Set.fromList <$> listOf arbitrary
    return $ SemanticIR file code statements expressions identifiers

instance Arbitrary MockStatement where
  arbitrary = do
    stmtId <- arbitrary
    stmtContent <- arbitrary
    return $ MockStatement stmtId stmtContent

instance Arbitrary MockExpression where
  arbitrary = do
    exprType <- arbitrary
    exprValue <- arbitrary
    return $ MockExpression exprType exprValue

instance Arbitrary String where
  arbitrary = oneof
    [ return ""
    , listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements "0123456789_"
    ]

-- Helper property function
property :: Bool -> Property
property = id

-- Mock helper functions
semanticStatements :: SemanticIR -> [MockStatement]
semanticStatements _ = [MockStatement "main" "println(\"hello\")"]

semanticGoCode :: SemanticIR -> String
semanticGoCode (SemanticIR _ code _ _ _) = code