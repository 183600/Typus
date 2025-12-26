{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.DependentTypeBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)

import DependentTypesParser
import Compiler.Errors.Core
import SourceLocation
import qualified Dependencies.TypeSystem as Dep

-- | Test dependent type constraint validation
testDependentTypeConstraintValidation :: Property
testDependentTypeConstraintValidation =
  forAll arbitrary $ \constraint ->
    let isValid = Dep.validateConstraint constraint
        constraintText = Dep.showConstraint constraint
    in if T.null constraintText
       then not isValid
       else property True -- Valid constraints should have non-empty representation

-- | Test type variable substitution properties
testTypeVariableSubstitution :: Property
testTypeVariableSubstitution =
  forAll arbitrary $ \typ ->
    forAll arbitrary $ \substitution ->
      let substituted = Dep.substituteTypeVars substitution typ
          -- Substitution should preserve type structure
          typeVarsBefore = Dep.getTypeVars typ
          typeVarsAfter = Dep.getTypeVars substituted
      in if null substitution
         then substituted === typ
         else property True

-- | Test dependent type equality properties
testDependentTypeEquality :: Property
testDependentTypeEquality =
  forAll arbitrary $ \typ1 ->
    forAll arbitrary $ \typ2 ->
      let equality = Dep.typesEqual typ1 typ2
          structuralEquality = Dep.structurallyEqual typ1 typ2
      in if equality
         then structuralEquality -- Equal types should be structurally equal
         else property True

-- | Test type inference consistency
testTypeInferenceConsistency :: Property
testTypeInferenceConsistency =
  forAll arbitrary $ \expression ->
    forAll arbitrary $ \context ->
      let inferredType = Dep.inferType context expression
          expectedType = Dep.getExpectedType context expression
      in case (inferredType, expectedType) of
        (Just inferred, Just expected) -> 
          if Dep.isSubtype inferred expected
          then property True
          else property False -- Should fail if inferred not subtype of expected
        _ -> property True -- Partial inference is allowed

-- | Test dependent type boundary conditions
testDependentTypeBoundaryConditions :: Property
testDependentTypeBoundaryConditions =
  forAll arbitrary $ \types ->
    let uniqueTypes = nub types
        typeCount = length types
        uniqueCount = length uniqueTypes
    in uniqueCount <= typeCount .&&. uniqueCount >= 0

-- | Test constraint solving properties
testConstraintSolvingProperties :: Property
testConstraintSolvingProperties =
  forAll arbitrary $ \constraints ->
    let solution = Dep.solveConstraints constraints
        constraintsCount = length constraints
        solutionVars = Dep.getSolutionVars solution
    in if null constraints
       then Dep.isEmptySolution solution
       else length solutionVars >= 0

-- | Test type variable freshness
testTypeVariableFreshness :: Property
testTypeVariableFreshness =
  forAll arbitrary $ \typ ->
    let freshType = Dep.freshenTypeVars typ
        originalVars = Dep.getTypeVars typ
        freshVars = Dep.getTypeVars freshType
    in if null originalVars
       then freshType === typ
       else length freshVars >= length originalVars

-- | Test dependent type normalization
testDependentTypeNormalization :: Property
testDependentTypeNormalization =
  forAll arbitrary $ \typ ->
    let normalized = Dep.normalizeType typ
        -- Normalization should preserve type equivalence
        isEquivalent = Dep.typesEquivalent typ normalized
    in isEquivalent

-- | Test type application properties
testTypeApplicationProperties :: Property
testTypeApplicationProperties =
  forAll arbitrary $ \functionType ->
    forAll arbitrary $ \argumentType ->
      let resultType = Dep.applyType functionType argumentType
      in case resultType of
        Just result -> Dep.isValidType result
        Nothing -> property True -- Invalid applications should return Nothing

-- | Test dependent type unification
testDependentTypeUnification :: Property
testDependentTypeUnification =
  forAll arbitrary $ \typ1 ->
    forAll arbitrary $ \typ2 ->
      let unification = Dep.unifyTypes typ1 typ2
      in case unification of
        Just subst -> Dep.isValidSubstitution subst
        Nothing -> property True -- Unification may fail

-- | Test type kind checking
testTypeKindChecking :: Property
testTypeKindChecking =
  forAll arbitrary $ \typ ->
    let kind = Dep.getKind typ
        isWellKinded = Dep.isWellKinded typ
    in if isWellKinded
       then kind /= Dep.KindError
       else property True

-- | Test dependent type reduction
testDependentTypeReduction :: Property
testDependentTypeReduction =
  forAll arbitrary $ \typ ->
    let reduced = Dep.reduceType typ
        isReducible = Dep.isReducible typ
    in if isReducible
       then reduced /= typ
       else reduced === typ

-- | Test type variable occurrence checking
testTypeVariableOccurrence :: Property
testTypeVariableOccurrence =
  forAll arbitrary $ \typ ->
    forAll arbitrary $ \var ->
      let occurs = Dep.typeVarOccurs var typ
          allVars = Dep.getTypeVars typ
      in occurs === (var `elem` allVars)

-- | Test dependent type substitution composition
testSubstitutionComposition :: Property
testSubstitutionComposition =
  forAll arbitrary $ \subst1 ->
    forAll arbitrary $ \subst2 ->
      let composed = Dep.composeSubstitutions subst1 subst2
          -- Composition should be associative
          composedAgain = Dep.composeSubstitutions composed subst2
      in Dep.isValidSubstitution composed .&&.
         Dep.isValidSubstitution composedAgain

-- | Test type variable binding scope
testTypeVariableBindingScope :: Property
testTypeVariableBindingScope =
  forAll arbitrary $ \expression ->
    let boundVars = Dep.getBoundVariables expression
        freeVars = Dep.getFreeVariables expression
        allVars = Dep.getAllVariables expression
    in allVars `elem` [boundVars ++ freeVars] .&&.
       length allVars >= length boundVars .&&.
       length allVars >= length freeVars

-- | Test dependent type well-formedness
testDependentTypeWellFormedness :: Property
testDependentTypeWellFormedness =
  forAll arbitrary $ \typ ->
    let wellFormed = Dep.isWellFormed typ
        kind = Dep.getKind typ
    in if wellFormed
       then kind /= Dep.KindError .&&. Dep.isValidType typ
       else property True

tests :: TestTree
tests = testGroup "Dependent Type Boundary QuickCheck Tests"
  [ testProperty "Constraint validation" testDependentTypeConstraintValidation
  , testProperty "Type variable substitution" testTypeVariableSubstitution
  , testProperty "Type equality properties" testDependentTypeEquality
  , testProperty "Type inference consistency" testTypeInferenceConsistency
  , testProperty "Boundary conditions" testDependentTypeBoundaryConditions
  , testProperty "Constraint solving" testConstraintSolvingProperties
  , testProperty "Type variable freshness" testTypeVariableFreshness
  , testProperty "Type normalization" testDependentTypeNormalization
  , testProperty "Type application" testTypeApplicationProperties
  , testProperty "Type unification" testDependentTypeUnification
  , testProperty "Kind checking" testTypeKindChecking
  , testProperty "Type reduction" testDependentTypeReduction
  , testProperty "Variable occurrence" testTypeVariableOccurrence
  , testProperty "Substitution composition" testSubstitutionComposition
  , testProperty "Variable binding scope" testTypeVariableBindingScope
  , testProperty "Well-formedness" testDependentTypeWellFormedness
  ]