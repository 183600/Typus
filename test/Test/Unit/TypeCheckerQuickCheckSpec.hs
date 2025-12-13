{-# LANGUAGE CPP #-}

module Test.Unit.TypeCheckerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , FunctionParam(..)
  , FunctionSignature(..)
  , CallExpr(..)
  , TypeError(..)
  , TypeCheckDiagnostic(..)
  )
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

-- Property: TypeName preserves name
prop_typename_preserves :: String -> Property
prop_typename_preserves name =
  let typ = TypeName name
  in case typ of
    TypeName n -> n === name
    _ -> property False

-- Property: UnknownType is always UnknownType
prop_unknowntype_constant :: Property
prop_unknowntype_constant =
  UnknownType === UnknownType

-- Property: Type equality
prop_type_eq :: Type -> Type -> Property
prop_type_eq t1 t2 =
  (t1 == t2) === case (t1, t2) of
    (TypeName n1, TypeName n2) -> n1 == n2
    (UnknownType, UnknownType) -> True
    _ -> False

-- Property: FunctionParam with all fields
prop_functionparam_all :: Maybe String -> Type -> Bool -> Property
prop_functionparam_all name typ variadic =
  let param = FunctionParam name typ variadic
  in fpName param === name &&
     fpType param === typ &&
     fpVariadic param === variadic

-- Property: FunctionParam with no name
prop_functionparam_no_name :: Type -> Bool -> Property
prop_functionparam_no_name typ variadic =
  let param = FunctionParam Nothing typ variadic
  in fpName param === Nothing &&
     fpType param === typ &&
     fpVariadic param === variadic

-- Property: FunctionParam with name
prop_functionparam_with_name :: String -> Type -> Bool -> Property
prop_functionparam_with_name name typ variadic =
  let param = FunctionParam (Just name) typ variadic
  in fpName param === Just name &&
     fpType param === typ &&
     fpVariadic param === variadic

-- Property: FunctionSignature with params and returns
prop_functionsignature_params_returns :: [FunctionParam] -> [Type] -> Property
prop_functionsignature_params_returns params returns =
  let sig = FunctionSignature params returns
  in fsParams sig === params &&
     fsReturns sig === returns

-- Property: FunctionSignature with empty params
prop_functionsignature_empty_params :: [Type] -> Property
prop_functionsignature_empty_params returns =
  let sig = FunctionSignature [] returns
  in null (fsParams sig) &&
     fsReturns sig === returns

-- Property: FunctionSignature with empty returns
prop_functionsignature_empty_returns :: [FunctionParam] -> Property
prop_functionsignature_empty_returns params =
  let sig = FunctionSignature params []
  in fsParams sig === params &&
     null (fsReturns sig)

-- Property: FunctionSignature with both empty
prop_functionsignature_empty_both :: Property
prop_functionsignature_empty_both =
  let sig = FunctionSignature [] []
  in null (fsParams sig) && null (fsReturns sig)

-- Property: TypeEnv with var and function types
prop_typeenv_vars_functions :: [(String, Type)] -> [(String, FunctionSignature)] -> Property
prop_typeenv_vars_functions varPairs funcPairs =
  let varMap = Map.fromList varPairs
      funcMap = Map.fromList funcPairs
      env = TypeEnv varMap funcMap
  in varTypes env === varMap &&
     functionTypes env === funcMap

-- Property: TypeEnv with empty maps
prop_typeenv_empty :: Property
prop_typeenv_empty =
  let env = TypeEnv Map.empty Map.empty
  in Map.null (varTypes env) && Map.null (functionTypes env)

-- Property: TypeEnv with only vars
prop_typeenv_only_vars :: [(String, Type)] -> Property
prop_typeenv_only_vars varPairs =
  let varMap = Map.fromList varPairs
      env = TypeEnv varMap Map.empty
  in varTypes env === varMap && Map.null (functionTypes env)

-- Property: TypeEnv with only functions
prop_typeenv_only_functions :: [(String, FunctionSignature)] -> Property
prop_typeenv_only_functions funcPairs =
  let funcMap = Map.fromList funcPairs
      env = TypeEnv Map.empty funcMap
  in Map.null (varTypes env) && functionTypes env === funcMap

-- Property: CallExpr with name and args
prop_callexpr_name_args :: String -> [String] -> Property
prop_callexpr_name_args name args =
  let expr = CallExpr name args
  in callName expr === name &&
     callArgs expr === args

-- Property: CallExpr with no args
prop_callexpr_no_args :: String -> Property
prop_callexpr_no_args name =
  let expr = CallExpr name []
  in callName expr === name &&
     null (callArgs expr)

-- Property: CallExpr with empty name
prop_callexpr_empty_name :: [String] -> Property
prop_callexpr_empty_name args =
  let expr = CallExpr "" args
  in null (callName expr) &&
     callArgs expr === args

-- Property: TypeError with context and message
prop_typeerror_context_message :: Maybe String -> String -> Property
prop_typeerror_context_message context message =
  let err = TypeError context message
  in teContext err === context &&
     teMessage err === message

-- Property: TypeError with no context
prop_typeerror_no_context :: String -> Property
prop_typeerror_no_context message =
  let err = TypeError Nothing message
  in teContext err === Nothing &&
     teMessage err === message

-- Property: TypeError with context
prop_typeerror_with_context :: String -> String -> Property
prop_typeerror_with_context context message =
  let err = TypeError (Just context) message
  in teContext err === Just context &&
     teMessage err === message

-- Property: TypeCheckDiagnostic with context and message
prop_typecheckdiagnostic_context_message :: Maybe String -> String -> Property
prop_typecheckdiagnostic_context_message context message =
  let diag = TypeCheckDiagnostic context message
  in tcdContext diag === context &&
     tcdMessage diag === message

-- Property: TypeCheckDiagnostic with no context
prop_typecheckdiagnostic_no_context :: String -> Property
prop_typecheckdiagnostic_no_context message =
  let diag = TypeCheckDiagnostic Nothing message
  in tcdContext diag === Nothing &&
     tcdMessage diag === message

-- Property: TypeCheckDiagnostic with context
prop_typecheckdiagnostic_with_context :: String -> String -> Property
prop_typecheckdiagnostic_with_context context message =
  let diag = TypeCheckDiagnostic (Just context) message
  in tcdContext diag === Just context &&
     tcdMessage diag === message

-- Property: FunctionParam equality
prop_functionparam_eq :: FunctionParam -> FunctionParam -> Property
prop_functionparam_eq p1 p2 =
  (p1 == p2) === (fpName p1 == fpName p2 && 
                  fpType p1 == fpType p2 && 
                  fpVariadic p1 == fpVariadic p2)

-- Property: FunctionSignature equality
prop_functionsignature_eq :: FunctionSignature -> FunctionSignature -> Property
prop_functionsignature_eq s1 s2 =
  (s1 == s2) === (fsParams s1 == fsParams s2 && 
                  fsReturns s1 == fsReturns s2)

-- Property: CallExpr equality
prop_callexpr_eq :: CallExpr -> CallExpr -> Property
prop_callexpr_eq e1 e2 =
  (e1 == e2) === (callName e1 == callName e2 && 
                  callArgs e1 == callArgs e2)

-- Property: TypeError equality
prop_typeerror_eq :: TypeError -> TypeError -> Property
prop_typeerror_eq e1 e2 =
  (e1 == e2) === (teContext e1 == teContext e2 && 
                  teMessage e1 == teMessage e2)

-- Property: TypeCheckDiagnostic equality
prop_typecheckdiagnostic_eq :: TypeCheckDiagnostic -> TypeCheckDiagnostic -> Property
prop_typecheckdiagnostic_eq d1 d2 =
  (d1 == d2) === (tcdContext d1 == tcdContext d2 && 
                  tcdMessage d1 == tcdMessage d2)

-- Property: Type ordering
prop_type_ordering :: Type -> Type -> Property
prop_type_ordering t1 t2 =
  let result = compare t1 t2
  in (result == LT || result == EQ || result == GT) === True

-- Property: FunctionParam ordering
prop_functionparam_ordering :: FunctionParam -> FunctionParam -> Property
prop_functionparam_ordering p1 p2 =
  let result = compare p1 p2
  in (result == LT || result == EQ || result == GT) === True

-- Property: FunctionSignature ordering
prop_functionsignature_ordering :: FunctionSignature -> FunctionSignature -> Property
prop_functionsignature_ordering s1 s2 =
  let result = compare s1 s2
  in (result == LT || result == EQ || result == GT) === True

-- Property: CallExpr ordering
prop_callexpr_ordering :: CallExpr -> CallExpr -> Property
prop_callexpr_ordering e1 e2 =
  let result = compare e1 e2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeError ordering
prop_typeerror_ordering :: TypeError -> TypeError -> Property
prop_typeerror_ordering e1 e2 =
  let result = compare e1 e2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeCheckDiagnostic ordering
prop_typecheckdiagnostic_ordering :: TypeCheckDiagnostic -> TypeCheckDiagnostic -> Property
prop_typecheckdiagnostic_ordering d1 d2 =
  let result = compare d1 d2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeName show
prop_typename_show :: String -> Property
prop_typename_show name =
  let typ = TypeName name
      shown = show typ
  in name `isInfixOf` shown

-- Property: UnknownType show
prop_unknowntype_show :: Property
prop_unknowntype_show =
  let shown = show UnknownType
  in "UnknownType" `isInfixOf` shown

-- Property: FunctionParam show
prop_functionparam_show :: Maybe String -> Type -> Bool -> Property
prop_functionparam_show name typ variadic =
  let param = FunctionParam name typ variadic
      shown = show param
  in not (null shown)

-- Property: FunctionSignature show
prop_functionsignature_show :: [FunctionParam] -> [Type] -> Property
prop_functionsignature_show params returns =
  let sig = FunctionSignature params returns
      shown = show sig
  in not (null shown)

-- Property: CallExpr show
prop_callexpr_show :: String -> [String] -> Property
prop_callexpr_show name args =
  let expr = CallExpr name args
      shown = show expr
  in name `isInfixOf` shown

-- Property: TypeError show
prop_typeerror_show :: Maybe String -> String -> Property
prop_typeerror_show context message =
  let err = TypeError context message
      shown = show err
  in message `isInfixOf` shown

-- Property: TypeCheckDiagnostic show
prop_typecheckdiagnostic_show :: Maybe String -> String -> Property
prop_typecheckdiagnostic_show context message =
  let diag = TypeCheckDiagnostic context message
      shown = show diag
  in message `isInfixOf` shown

-- Property: TypeEnv with duplicate keys (last wins)
prop_typeenv_duplicate_keys :: String -> Type -> Type -> String -> FunctionSignature -> FunctionSignature -> Property
prop_typeenv_duplicate_keys varName type1 type2 funcName sig1 sig2 =
  let varMap = Map.fromList [(varName, type1), (varName, type2)]
      funcMap = Map.fromList [(funcName, sig1), (funcName, sig2)]
      env = TypeEnv varMap funcMap
  in Map.lookup varName (varTypes env) === Just type2 &&
     Map.lookup funcName (functionTypes env) === Just sig2

-- Property: FunctionParam with variadic flag
prop_functionparam_variadic :: Maybe String -> Type -> Property
prop_functionparam_variadic name typ =
  let nonVariadic = FunctionParam name typ False
      variadic = FunctionParam name typ True
  in fpVariadic nonVariadic === False &&
     fpVariadic variadic === True &&
     fpName nonVariadic === fpName variadic &&
     fpType nonVariadic === fpType variadic

-- Property: CallExpr with multiple args
prop_callexpr_multiple_args :: String -> String -> String -> String -> Property
prop_callexpr_multiple_args name arg1 arg2 arg3 =
  let args = [arg1, arg2, arg3]
      expr = CallExpr name args
  in callArgs expr === args &&
     length (callArgs expr) === 3

-- Property: TypeError with empty message
prop_typeerror_empty_message :: Maybe String -> Property
prop_typeerror_empty_message context =
  let err = TypeError context ""
  in teMessage err === "" &&
     teContext err === context

-- Property: TypeCheckDiagnostic with empty message
prop_typecheckdiagnostic_empty_message :: Maybe String -> Property
prop_typecheckdiagnostic_empty_message context =
  let diag = TypeCheckDiagnostic context ""
  in tcdMessage diag === "" &&
     tcdContext diag === context

-- Property: Type with different names
prop_type_different_names :: String -> String -> Property
prop_type_different_names name1 name2 =
  let t1 = TypeName name1
      t2 = TypeName name2
  in (t1 == t2) === (name1 == name2)

-- Property: FunctionParam with different types
prop_functionparam_different_types :: Maybe String -> Type -> Type -> Property
prop_functionparam_different_types name type1 type2 =
  let p1 = FunctionParam name type1 False
      p2 = FunctionParam name type2 False
  in (p1 == p2) === (type1 == type2)

-- Property: FunctionSignature with different params
prop_functionsignature_different_params :: [FunctionParam] -> [FunctionParam] -> [Type] -> Property
prop_functionsignature_different_params params1 params2 returns =
  let s1 = FunctionSignature params1 returns
      s2 = FunctionSignature params2 returns
  in (s1 == s2) === (params1 == params2)

-- Property: FunctionSignature with different returns
prop_functionsignature_different_returns :: [FunctionParam] -> [Type] -> [Type] -> Property
prop_functionsignature_different_returns params returns1 returns2 =
  let s1 = FunctionSignature params returns1
      s2 = FunctionSignature params returns2
  in (s1 == s2) === (returns1 == returns2)

tests :: TestTree
tests = testGroup "TypeChecker QuickCheck tests"
  [ fastProperty "TypeName preserves name" prop_typename_preserves
  , fastProperty "UnknownType is always UnknownType" prop_unknowntype_constant
  , fastProperty "Type equality" prop_type_eq
  , fastProperty "FunctionParam with all fields" prop_functionparam_all
  , fastProperty "FunctionParam with no name" prop_functionparam_no_name
  , fastProperty "FunctionParam with name" prop_functionparam_with_name
  , fastProperty "FunctionSignature with params and returns" prop_functionsignature_params_returns
  , fastProperty "FunctionSignature with empty params" prop_functionsignature_empty_params
  , fastProperty "FunctionSignature with empty returns" prop_functionsignature_empty_returns
  , fastProperty "FunctionSignature with both empty" prop_functionsignature_empty_both
  , fastProperty "TypeEnv with var and function types" prop_typeenv_vars_functions
  , fastProperty "TypeEnv with empty maps" prop_typeenv_empty
  , fastProperty "TypeEnv with only vars" prop_typeenv_only_vars
  , fastProperty "TypeEnv with only functions" prop_typeenv_only_functions
  , fastProperty "CallExpr with name and args" prop_callexpr_name_args
  , fastProperty "CallExpr with no args" prop_callexpr_no_args
  , fastProperty "CallExpr with empty name" prop_callexpr_empty_name
  , fastProperty "TypeError with context and message" prop_typeerror_context_message
  , fastProperty "TypeError with no context" prop_typeerror_no_context
  , fastProperty "TypeError with context" prop_typeerror_with_context
  , fastProperty "TypeCheckDiagnostic with context and message" prop_typecheckdiagnostic_context_message
  , fastProperty "TypeCheckDiagnostic with no context" prop_typecheckdiagnostic_no_context
  , fastProperty "TypeCheckDiagnostic with context" prop_typecheckdiagnostic_with_context
  , fastProperty "FunctionParam equality" prop_functionparam_eq
  , fastProperty "FunctionSignature equality" prop_functionsignature_eq
  , fastProperty "CallExpr equality" prop_callexpr_eq
  , fastProperty "TypeError equality" prop_typeerror_eq
  , fastProperty "TypeCheckDiagnostic equality" prop_typecheckdiagnostic_eq
  , fastProperty "Type ordering" prop_type_ordering
  , fastProperty "FunctionParam ordering" prop_functionparam_ordering
  , fastProperty "FunctionSignature ordering" prop_functionsignature_ordering
  , fastProperty "CallExpr ordering" prop_callexpr_ordering
  , fastProperty "TypeError ordering" prop_typeerror_ordering
  , fastProperty "TypeCheckDiagnostic ordering" prop_typecheckdiagnostic_ordering
  , fastProperty "TypeName show" prop_typename_show
  , fastProperty "UnknownType show" prop_unknowntype_show
  , fastProperty "FunctionParam show" prop_functionparam_show
  , fastProperty "FunctionSignature show" prop_functionsignature_show
  , fastProperty "CallExpr show" prop_callexpr_show
  , fastProperty "TypeError show" prop_typeerror_show
  , fastProperty "TypeCheckDiagnostic show" prop_typecheckdiagnostic_show
  , fastProperty "TypeEnv with duplicate keys (last wins)" prop_typeenv_duplicate_keys
  , fastProperty "FunctionParam with variadic flag" prop_functionparam_variadic
  , fastProperty "CallExpr with multiple args" prop_callexpr_multiple_args
  , fastProperty "TypeError with empty message" prop_typeerror_empty_message
  , fastProperty "TypeCheckDiagnostic with empty message" prop_typecheckdiagnostic_empty_message
  , fastProperty "Type with different names" prop_type_different_names
  , fastProperty "FunctionParam with different types" prop_functionparam_different_types
  , fastProperty "FunctionSignature with different params" prop_functionsignature_different_params
  , fastProperty "FunctionSignature with different returns" prop_functionsignature_different_returns
  ]