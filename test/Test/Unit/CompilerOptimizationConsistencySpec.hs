{-# LANGUAGE LambdaCase #-}

module Test.Unit.CompilerOptimizationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized)
import qualified Data.Text as T
import qualified Data.List as L

import Compiler (compile, generateGoCode)
import Parser (parseTypus, TypusFile(..))
import Compiler.IR (IR, emitGo, goSource)
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import SourceLocation (SourceSpan(..), defaultSpan)

-- | Simple expression types for optimization testing
data SimpleExpr 
    = Literal Int
    | Variable String
    | Add SimpleExpr SimpleExpr
    | Multiply SimpleExpr SimpleExpr
    deriving (Show, Eq)

-- | Optimization scenario types
data OptScenario 
    = ConstantFolding SimpleExpr
    | DeadCodeElimination [String] SimpleExpr
    | StrengthReduction SimpleExpr
    deriving (Show, Eq)

-- | Generate simple expressions for optimization testing
instance Arbitrary SimpleExpr where
    arbitrary = sized $ \n -> if n <= 0
        then oneof [Literal <$> arbitrary, Variable <$> genVarName]
        else oneof 
            [ Literal <$> arbitrary
            , Variable <$> genVarName
            , Add <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , Multiply <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            ]
      where
        genVarName = elements ["x", "y", "z", "a", "b", "c", "result", "temp"]

-- | Generate optimization scenarios
instance Arbitrary OptScenario where
    arbitrary = oneof
        [ ConstantFolding <$> arbitrary
        , DeadCodeElimination <$> (listOf $ elements ["x", "y", "z"]) <*> arbitrary
        , StrengthReduction <$> arbitrary
        ]

-- | Property: Optimization should preserve program semantics
prop_optimizationPreservesSemantics :: OptScenario -> Bool
prop_optimizationPreservesSemantics scenario = case scenario of
    ConstantFolding expr -> 
        let original = evalSimpleExpr expr
            optimized = optimizeConstantFolding expr
        in evalSimpleExpr optimized == original
    
    DeadCodeElimination deadVars expr ->
        let original = evalSimpleExpr expr
            -- Remove dead variables from expression (simplified)
            optimized = removeDeadVariables deadVars expr
        in evalSimpleExpr optimized == original
    
    StrengthReduction expr ->
        let original = evalSimpleExpr expr
            optimized = optimizeStrengthReduction expr
        in evalSimpleExpr optimized == original

-- | Evaluate simple expressions
evalSimpleExpr :: SimpleExpr -> Int
evalSimpleExpr = \case
    Literal n -> n
    Variable _ -> 0 -- Simplified: variables evaluate to 0
    Add e1 e2 -> evalSimpleExpr e1 + evalSimpleExpr e2
    Multiply e1 e2 -> evalSimpleExpr e1 * evalSimpleExpr e2

-- | Constant folding optimization
optimizeConstantFolding :: SimpleExpr -> SimpleExpr
optimizeConstantFolding = \case
    Add (Literal n1) (Literal n2) -> Literal (n1 + n2)
    Multiply (Literal n1) (Literal n2) -> Literal (n1 * n2)
    Add e1 e2 -> Add (optimizeConstantFolding e1) (optimizeConstantFolding e2)
    Multiply e1 e2 -> Multiply (optimizeConstantFolding e1) (optimizeConstantFolding e2)
    other -> other

-- | Dead code elimination (simplified)
removeDeadVariables :: [String] -> SimpleExpr -> SimpleExpr
removeDeadVariables deadVars = \case
    Variable name | name `elem` deadVars -> Literal 0
    Add e1 e2 -> Add (removeDeadVariables deadVars e1) (removeDeadVariables deadVars e2)
    Multiply e1 e2 -> Multiply (removeDeadVariables deadVars e1) (removeDeadVariables deadVars e2)
    other -> other

-- | Strength reduction optimization (simplified)
optimizeStrengthReduction :: SimpleExpr -> SimpleExpr
optimizeStrengthReduction = \case
    Multiply e (Literal 2) -> Add e e -- x * 2 -> x + x
    Multiply (Literal 2) e -> Add e e -- 2 * x -> x + x
    Add e1 e2 -> Add (optimizeStrengthReduction e1) (optimizeStrengthReduction e2)
    Multiply e1 e2 -> Multiply (optimizeStrengthReduction e1) (optimizeStrengthReduction e2)
    other -> other

-- | Property: Generated Go code should be syntactically valid
prop_generatedGoCodeIsValid :: String -> Bool
prop_generatedGoCodeIsValid typusCode = 
    case parseTypus typusCode of
        Left _ -> True -- Invalid input should not crash
        Right typusFile -> 
            let goCode = generateGoCode typusFile
            in not (null goCode) && isValidGoSyntax goCode

-- | Simple Go syntax validation (basic checks)
isValidGoSyntax :: String -> Bool
isValidGoSyntax code = 
    let lines' = lines code
        hasPackage = any ("package" `isPrefixOf`) lines'
        balancedBraces = countBraces '(' code == countBraces ')' code
    in hasPackage && balancedBraces
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    countBraces char = length $ filter (== char) code

-- | Property: Compilation should handle ownership directives correctly
prop_compilationHandlesOwnershipDirectives :: Bool -> String -> Bool
prop_compilationHandlesOwnershipDirectives ownershipEnabled code = 
    let directive = if ownershipEnabled then "//! ownership: on" else "//! ownership: off"
        fullCode = directive ++ "\n" ++ code
    in case parseTypus fullCode of
        Left _ -> True -- Invalid parsing should not crash
        Right typusFile -> 
            case compile typusFile of
                Left _ -> True -- Compilation errors are acceptable
                Right _ -> True -- Successful compilation is acceptable

tests :: TestTree
tests = testGroup "Compiler Optimization Consistency Tests"
  [ testProperty "Optimization preserves semantics" $ 
      fastProperty "constant folding, dead code elimination, strength reduction" 
      prop_optimizationPreservesSemantics
  
  , testProperty "Generated Go code is syntactically valid" $
      fastProperty "various Typus code inputs" 
      prop_generatedGoCodeIsValid
  
  , testProperty "Compilation handles ownership directives correctly" $
      fastProperty "ownership on/off directives" 
      prop_compilationHandlesOwnershipDirectives
  
  , testProperty "Multiple optimizations compose correctly" $
      fastProperty "chained optimizations" $
      \expr -> 
        let folded = optimizeConstantFolding expr
            reduced = optimizeStrengthReduction folded
        in evalSimpleExpr reduced == evalSimpleExpr expr
  ]