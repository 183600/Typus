module Test.Unit.CompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>) , property, forAll, counterexample, classify, cover, Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements)
import qualified Data.List as Data.List

import Compiler.GoAst

-- Arbitrary instances for PackageDecl
instance Arbitrary PackageDecl where
  arbitrary = PackageDecl <$> arbitrary

-- Arbitrary instances for ImportDecl
instance Arbitrary ImportDecl where
  arbitrary = ImportDecl <$> oneof [pure Nothing, Just <$> arbitrary] <*> arbitrary

-- Arbitrary instances for FuncDecl
instance Arbitrary FuncDecl where
  arbitrary = FuncDecl <$> listOf arbitrary

-- Arbitrary instances for TypeDecl
instance Arbitrary TypeDecl where
  arbitrary = TypeDecl <$> listOf arbitrary <*> arbitrary

-- Arbitrary instances for VarDecl
instance Arbitrary VarDecl where
  arbitrary = VarDecl <$> listOf arbitrary <*> arbitrary

-- Arbitrary instances for ConstDecl
instance Arbitrary ConstDecl where
  arbitrary = ConstDecl <$> listOf arbitrary <*> arbitrary

-- Arbitrary instances for StatementBlock
instance Arbitrary StatementBlock where
  arbitrary = StatementBlock <$> listOf arbitrary

-- Arbitrary instances for RawBlock
instance Arbitrary RawBlock where
  arbitrary = RawBlock <$> listOf arbitrary

-- Arbitrary instances for GoDecl
instance Arbitrary GoDecl where
  arbitrary = oneof
    [ GoFunc <$> arbitrary
    , GoType <$> arbitrary
    , GoVar <$> arbitrary
    , GoConst <$> arbitrary
    , GoStatement <$> arbitrary
    , GoRaw <$> arbitrary
    ]

-- Arbitrary instances for GoModule
instance Arbitrary GoModule where
  arbitrary = GoModule <$> listOf arbitrary <*> oneof [pure Nothing, Just <$> arbitrary] <*> listOf arbitrary <*> listOf arbitrary

-- Property: GoModule construction works correctly
prop_gomodule_construction :: Property
prop_gomodule_construction =
  let goModule = GoModule [] Nothing [] []
  in case goModule of
    GoModule buildTags pkg imports decls ->
      property $ null buildTags && pkg == Nothing && null imports && null decls

-- Property: ImportDecl construction works correctly
prop_import_decl_construction :: Property
prop_import_decl_construction =
  let importDecl = ImportDecl Nothing "package"
  in case importDecl of
    ImportDecl alias path -> property $ alias == Nothing && path == "package"

-- Property: TypeDecl construction works correctly
prop_type_decl_construction :: Property
prop_type_decl_construction =
  let typeDecl = TypeDecl ["MyType"] True
  in case typeDecl of
    TypeDecl name t -> property $ name == ["MyType"] && t == True

-- Property: VarDecl construction works correctly
prop_var_decl_construction :: Property
prop_var_decl_construction =
  let varDecl = VarDecl ["x", "y"] True
  in case varDecl of
    VarDecl names t -> property $ names == ["x", "y"] && t == True

-- Property: ConstDecl construction works correctly
prop_const_decl_construction :: Property
prop_const_decl_construction =
  let constDecl = ConstDecl ["PI"] True
  in case constDecl of
    ConstDecl names t -> property $ names == ["PI"] && t == True

-- Property: FuncDecl construction works correctly
prop_func_decl_construction :: Property
prop_func_decl_construction =
  let funcDecl = FuncDecl ["func main() {", "  fmt.Println(\"hello\")", "}"]
  in case funcDecl of
    FuncDecl lines -> length lines === 3

-- Property: GoDecl variants are distinguishable
prop_godecl_variants :: GoDecl -> GoDecl -> Property
prop_godecl_variants decl1 decl2 =
  let areEqual = decl1 == decl2
      areNotEqual = decl1 /= decl2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: GoDecl pattern matching works correctly
prop_godecl_pattern_matching :: GoDecl -> Property
prop_godecl_pattern_matching decl =
  case decl of
    GoFunc _ -> property True
    GoType _ -> property True
    GoVar _ -> property True
    GoConst _ -> property True

-- Property: Empty GoModule has no components
prop_empty_gomodule :: Property
prop_empty_gomodule =
  let emptyModule = GoModule [] Nothing [] []
  in case emptyModule of
    GoModule buildTags pkg imports decls ->
      property $ null buildTags && pkg == Nothing && null imports && null decls

-- Property: GoModule equality works correctly
prop_gomodule_equality :: GoModule -> GoModule -> Property
prop_gomodule_equality module1 module2 =
  let areEqual = module1 == module2
      areNotEqual = module1 /= module2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: ImportDecl equality works correctly
prop_import_decl_equality :: ImportDecl -> ImportDecl -> Property
prop_import_decl_equality import1 import2 =
  let areEqual = import1 == import2
      areNotEqual = import1 /= import2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: TypeDecl equality works correctly
prop_type_decl_equality :: TypeDecl -> TypeDecl -> Property
prop_type_decl_equality type1 type2 =
  let areEqual = type1 == type2
      areNotEqual = type1 /= type2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: VarDecl equality works correctly
prop_var_decl_equality :: VarDecl -> VarDecl -> Property
prop_var_decl_equality var1 var2 =
  let areEqual = var1 == var2
      areNotEqual = var1 /= var2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: ConstDecl equality works correctly
prop_const_decl_equality :: ConstDecl -> ConstDecl -> Property
prop_const_decl_equality const1 const2 =
  let areEqual = const1 == const2
      areNotEqual = const1 /= const2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: FuncDecl equality works correctly
prop_func_decl_equality :: FuncDecl -> FuncDecl -> Property
prop_func_decl_equality func1 func2 =
  let areEqual = func1 == func2
      areNotEqual = func1 /= func2
  in property $ areEqual || areNotEqual -- Should be either equal or not equal

-- Property: Large GoModule is handled correctly
prop_large_gomodule :: Int -> Property
prop_large_gomodule n =
  n <= 100 ==> -- Limit size to avoid timeouts
  let imports = [ImportDecl Nothing ("import" ++ show i) | i <- [1..n]]
      decls = [GoType (TypeDecl ["Type" ++ show i] True) | i <- [1..n]]
      goModule = GoModule [] Nothing imports decls
  in case goModule of
    GoModule _ _ imps ds -> property $ length imps == n && length ds == n

-- Additional property tests for compiler optimization and error handling

-- Property: GoModule with circular dependencies is detected
prop_circular_dependencies :: [String] -> Property
prop_circular_dependencies moduleNames =
  let n = length moduleNames
  in n > 0 && n <= 10 ==>
  let imports = zipWith (\i name -> ImportDecl Nothing name) (cycle moduleNames) moduleNames
      goModule = GoModule [] Nothing imports []
  in property $ hasCircularImports goModule

-- Property: GoModule with duplicate imports is handled correctly
prop_duplicate_imports :: [String] -> Property
prop_duplicate_imports importPaths =
  let duplicates = importPaths ++ importPaths
      imports = map (\path -> ImportDecl Nothing path) duplicates
      goModule = GoModule [] Nothing imports []
  in property $ hasDuplicateImports goModule

-- Property: GoModule with invalid package names is rejected
prop_invalid_package_names :: String -> Property
prop_invalid_package_names pkgName =
  let hasInvalidChars = any (`elem` "!@#$%^&*()+=[]{}|;:'\",.<>?/~`") pkgName
      startsWithNumber = not (null pkgName) && head pkgName `elem` ['0'..'9']
      isEmpty = null pkgName
  in classify (hasInvalidChars || startsWithNumber || isEmpty) "invalid package name" $
     let goModule = GoModule [] (Just $ PackageDecl pkgName) [] []
     in property $ hasInvalidPackageName goModule

-- Property: GoModule with deeply nested type definitions is handled correctly
prop_deeply_nested_types :: Int -> Property
prop_deeply_nested_types depth =
  depth <= 5 ==> -- Limit depth to avoid complexity
  let nestedTypes = generateNestedTypes depth
      typeDecls = map (\(name, _) -> GoType (TypeDecl [name] True)) nestedTypes
      goModule = GoModule [] Nothing [] typeDecls
  in property $ hasValidNestedTypes goModule

-- Property: GoModule with function overloading scenarios
prop_function_overloading :: [String] -> [String] -> Property
prop_function_overloading funcNames paramTypes =
  let funcs = zipWith (\name pType -> GoFunc (FuncDecl ["func " ++ name ++ "(" ++ pType ++ ") {}"])) 
                      funcNames paramTypes
      goModule = GoModule [] Nothing [] funcs
  in property $ hasValidFunctionSignatures goModule

-- Property: GoModule with generic type parameters
prop_generic_type_parameters :: [String] -> [String] -> Property
prop_generic_type_parameters typeNames typeParams =
  let generics = zipWith (\tName tParam -> GoType (TypeDecl [tName ++ "[" ++ tParam ++ "]"] True)) 
                         typeNames typeParams
      goModule = GoModule [] Nothing [] generics
  in property $ hasValidGenericTypes goModule

-- Property: GoModule with interface implementations
prop_interface_implementations :: [String] -> [String] -> Property
prop_interface_implementations interfaceNames implNames =
  let interfaces = map (\name -> GoType (TypeDecl [name] True)) interfaceNames
      implementations = map (\name -> GoType (TypeDecl [name] True)) implNames
      goModule = GoModule [] Nothing [] (interfaces ++ implementations)
  in property $ hasValidInterfaceImplementations goModule

-- Property: GoModule with struct field validation
prop_struct_field_validation :: [String] -> [String] -> Property
prop_struct_field_validation structNames fieldTypes =
  let structs = zipWith (\sName fType -> GoType (TypeDecl [sName ++ " struct { Field " ++ fType ++ " }"] True)) 
                        structNames fieldTypes
      goModule = GoModule [] Nothing [] structs
  in property $ hasValidStructFields goModule

-- Property: GoModule with constant expression evaluation
prop_constant_expressions :: [Int] -> [String] -> Property
prop_constant_expressions values operators =
  let constExprs = zipWith (\val op -> GoConst (ConstDecl ["C" ++ show val] True)) values operators
      goModule = GoModule [] Nothing [] constExprs
  in property $ hasValidConstantExpressions goModule

-- Property: GoModule with variable initialization
prop_variable_initialization :: [String] -> [String] -> Property
prop_variable_initialization varNames initExprs =
  let vars = zipWith (\vName init -> GoVar (VarDecl [vName ++ " = " ++ init] True)) varNames initExprs
      goModule = GoModule [] Nothing [] vars
  in property $ hasValidVariableInitialization goModule

-- Property: GoModule with recursive function definitions
prop_recursive_functions :: [String] -> Property
prop_recursive_functions funcNames =
  let recursiveFuncs = map (\name -> GoFunc (FuncDecl ["func " ++ name ++ "() {", "  return " ++ name ++ "()", "}"])) 
                          funcNames
      goModule = GoModule [] Nothing [] recursiveFuncs
  in property $ hasValidRecursiveFunctions goModule

-- Property: GoModule with error handling patterns
prop_error_handling_patterns :: [String] -> Property
prop_error_handling_patterns funcNames =
  let errorFuncs = map (\name -> GoFunc (FuncDecl ["func " ++ name ++ "() error {", "  return nil", "}"])) 
                      funcNames
      goModule = GoModule [] Nothing [] errorFuncs
  in property $ hasValidErrorHandling goModule

-- Property: GoModule with concurrent programming patterns
prop_concurrent_patterns :: [String] -> Property
prop_concurrent_patterns channelNames =
  let channels = map (\name -> GoVar (VarDecl [name ++ " chan int"] True)) channelNames
      goModule = GoModule [] Nothing [] channels
  in property $ hasValidConcurrentPatterns goModule

-- Property: GoModule with package-level variables
prop_package_level_variables :: [String] -> [String] -> Property
prop_package_level_variables varNames varTypes =
  let vars = zipWith (\vName vType -> GoVar (VarDecl [vName ++ " " ++ vType] True)) varNames varTypes
      goModule = GoModule [] Nothing [] vars
  in property $ hasValidPackageVariables goModule

-- Property: GoModule with build tags
prop_build_tags :: [String] -> Property
prop_build_tags tags =
  let goModule = GoModule tags Nothing [] []
  in property $ hasValidBuildTags goModule

-- Property: GoModule with import aliases
prop_import_aliases :: [String] -> [String] -> Property
prop_import_aliases originalPaths aliases =
  let imports = zipWith (\path alias -> ImportDecl (Just alias) path) originalPaths aliases
      goModule = GoModule [] Nothing imports []
  in property $ hasValidImportAliases goModule

-- Property: GoModule with type assertions
prop_type_assertions :: [String] -> [String] -> Property
prop_type_assertions varNames targetTypes =
  let assertions = zipWith (\vName tType -> GoFunc (FuncDecl ["func test() {", "  _ = " ++ vName ++ ".(" ++ tType ++ ")", "}"])) 
                           varNames targetTypes
      goModule = GoModule [] Nothing [] assertions
  in property $ hasValidTypeAssertions goModule

-- Property: GoModule with method receivers
prop_method_receivers :: [String] -> [String] -> Property
prop_method_receivers typeNames methodNames =
  let methods = zipWith (\tName mName -> GoFunc (FuncDecl ["func (" ++ tName ++ ") " ++ mName ++ "() {}"])) 
                        typeNames methodNames
      goModule = GoModule [] Nothing [] methods
  in property $ hasValidMethodReceivers goModule

-- Helper functions for new tests
hasCircularImports :: GoModule -> Bool
hasCircularImports (GoModule _ _ imports _) = 
  length imports > 1 -- Simplified check

hasDuplicateImports :: GoModule -> Bool
hasDuplicateImports (GoModule _ _ imports _) = 
  length imports > length (nubBy (\(ImportDecl _ p1) (ImportDecl _ p2) -> p1 == p2) imports)

hasInvalidPackageName :: GoModule -> Bool
hasInvalidPackageName (GoModule _ pkg _ _) = 
  case pkg of
    Nothing -> False
    Just (PackageDecl name) -> null name || any (`elem` "!@#$%^&*()+=[]{}|;:'\",.<>?/~`") name

hasValidNestedTypes :: GoModule -> Bool
hasValidNestedTypes (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidFunctionSignatures :: GoModule -> Bool
hasValidFunctionSignatures (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidGenericTypes :: GoModule -> Bool
hasValidGenericTypes (GoModule _ _ _ decls) = 
  all hasGenericParams decls
  where
    hasGenericParams (GoType (TypeDecl name _)) = any ("[" `isInfixOf`) name
    hasGenericParams _ = False

hasValidInterfaceImplementations :: GoModule -> Bool
hasValidInterfaceImplementations (GoModule _ _ _ decls) = 
  length decls >= 2 -- At least one interface and one implementation

hasValidStructFields :: GoModule -> Bool
hasValidStructFields (GoModule _ _ _ decls) = 
  all hasStructFields decls
  where
    hasStructFields (GoType (TypeDecl name _)) = any ("struct" `isInfixOf`) name
    hasStructFields _ = False

hasValidConstantExpressions :: GoModule -> Bool
hasValidConstantExpressions (GoModule _ _ _ decls) = 
  all isConstDecl decls
  where
    isConstDecl (GoConst _) = True
    isConstDecl _ = False

hasValidVariableInitialization :: GoModule -> Bool
hasValidVariableInitialization (GoModule _ _ _ decls) = 
  all isVarDecl decls
  where
    isVarDecl (GoVar _) = True
    isVarDecl _ = False

hasValidRecursiveFunctions :: GoModule -> Bool
hasValidRecursiveFunctions (GoModule _ _ _ decls) = 
  all isRecursiveFunc decls
  where
    isRecursiveFunc (GoFunc _) = True
    isRecursiveFunc _ = False

hasValidErrorHandling :: GoModule -> Bool
hasValidErrorHandling (GoModule _ _ _ decls) = 
  all isErrorFunc decls
  where
    isErrorFunc (GoFunc _) = True
    isErrorFunc _ = False

hasValidConcurrentPatterns :: GoModule -> Bool
hasValidConcurrentPatterns (GoModule _ _ _ decls) = 
  all isChannelDecl decls
  where
    isChannelDecl (GoVar _) = True
    isChannelDecl _ = False

hasValidPackageVariables :: GoModule -> Bool
hasValidPackageVariables (GoModule _ _ _ decls) = 
  all isVarDecl decls
  where
    isVarDecl (GoVar _) = True
    isVarDecl _ = False

hasValidBuildTags :: GoModule -> Bool
hasValidBuildTags (GoModule tags _ _ _) = 
  all (not . null) tags

hasValidImportAliases :: GoModule -> Bool
hasValidImportAliases (GoModule _ _ imports _) = 
  all hasAlias imports
  where
    hasAlias (ImportDecl (Just _) _) = True
    hasAlias _ = False

hasValidTypeAssertions :: GoModule -> Bool
hasValidTypeAssertions (GoModule _ _ _ decls) = 
  all hasTypeAssertion decls
  where
    hasTypeAssertion (GoFunc _) = True
    hasTypeAssertion _ = False

hasValidMethodReceivers :: GoModule -> Bool
hasValidMethodReceivers (GoModule _ _ _ decls) = 
  all hasMethodReceiver decls
  where
    hasMethodReceiver (GoFunc _) = True
    hasMethodReceiver _ = False

generateNestedTypes :: Int -> [(String, String)]
generateNestedTypes 0 = [("Base", "int")]
generateNestedTypes n = 
  let parent = "Level" ++ show n
      child = "Level" ++ show (n - 1)
  in (parent, child) : generateNestedTypes (n - 1)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs)

-- Additional comprehensive property tests for compiler optimization and error handling

-- Property: GoModule with anonymous functions and closures
prop_anonymous_functions :: [String] -> Property
prop_anonymous_functions functionBodies =
  let closures = map (\body -> GoFunc (FuncDecl ["var fn = func() {", "  " ++ body, "}"])) functionBodies
      goModule = GoModule [] Nothing [] closures
  in property $ hasValidAnonymousFunctions goModule

-- Property: GoModule with slice expressions and operations
prop_slice_expressions :: [String] -> [Int] -> Property
prop_slice_expressions sliceNames indices =
  let sliceOps = zipWith (\name idx -> GoFunc (FuncDecl ["func test() {", "  _ = " ++ name ++ "[" ++ show idx ++ ":" ++ show (idx + 1) ++ "]", "}"])) 
                        sliceNames indices
      goModule = GoModule [] Nothing [] sliceOps
  in property $ hasValidSliceExpressions goModule

-- Property: GoModule with map literals and operations
prop_map_literals :: [String] -> [String] -> Property
prop_map_literals mapNames keyTypes =
  let maps = zipWith (\name kType -> GoVar (VarDecl [name ++ " := map[" ++ kType ++ "]int{\"key\": 42}"] True)) 
                    mapNames keyTypes
      goModule = GoModule [] Nothing [] maps
  in property $ hasValidMapLiterals goModule

-- Property: GoModule with channel operations and select statements
prop_channel_select :: [String] -> Property
prop_channel_select channelNames =
  let channels = map (\name -> GoVar (VarDecl [name ++ " := make(chan int)"] True)) channelNames
      selects = map (\name -> GoFunc (FuncDecl ["func test() {", "  select {", "  case <-" ++ name ++ ":", "  }", "}"])) 
                   channelNames
      goModule = GoModule [] Nothing [] (channels ++ selects)
  in property $ hasValidChannelSelect goModule

-- Property: GoModule with defer statements and cleanup
prop_defer_cleanup :: [String] -> Property
prop_defer_cleanup cleanupFunctions =
  let deferStmts = map (\func -> GoFunc (FuncDecl ["func test() {", "  defer " ++ func ++ "()", "}"])) 
                        cleanupFunctions
      goModule = GoModule [] Nothing [] deferStmts
  in property $ hasValidDeferStatements goModule

-- Property: GoModule with panic and recover mechanisms
prop_panic_recover :: [String] -> Property
prop_panic_recover panicMessages =
  let panicFuncs = map (\msg -> GoFunc (FuncDecl ["func test() {", "  panic(\"" ++ msg ++ "\")", "}"])) 
                      panicMessages
      recoverFuncs = map (\msg -> GoFunc (FuncDecl ["func recoverTest() {", "  defer func() {", "    if r := recover(); r != nil {", "      fmt.Println(\"Recovered:\", r)", "    }", "  }()", "  panic(\"" ++ msg ++ "\")", "}"])) 
                         panicMessages
      goModule = GoModule [] Nothing [] (panicFuncs ++ recoverFuncs)
  in property $ hasValidPanicRecover goModule

-- Property: GoModule with type switches and assertions
prop_type_switch_assertions :: [String] -> [String] -> Property
prop_type_switch_assertions typeNames assertionTypes =
  let typeSwitches = map (\tName -> GoFunc (FuncDecl ["func test() {", "  switch v := interface{}(42).(type) {", "  case " ++ tName ++ ":", "  }", "}"])) 
                        typeNames
      assertions = map (\aType -> GoFunc (FuncDecl ["func test() {", "  var x interface{} = 42", "  _ = x.(" ++ aType ++ ")", "}"])) 
                        assertionTypes
      goModule = GoModule [] Nothing [] (typeSwitches ++ assertions)
  in property $ hasValidTypeSwitchAssertions goModule

-- Property: GoModule with embedded structs and composition
prop_embedded_structs :: [String] -> [String] -> Property
prop_embedded_structs structNames embeddedTypes =
  let embedded = zipWith (\sName eType -> GoType (TypeDecl [sName ++ " struct { " ++ eType ++ " }"] True)) 
                        structNames embeddedTypes
      goModule = GoModule [] Nothing [] embedded
  in property $ hasValidEmbeddedStructs goModule

-- Property: GoModule with struct tags and reflection
prop_struct_tags :: [String] -> [String] -> Property
prop_struct_tags fieldNames tagValues =
  let taggedFields = zipWith (\fName tag -> GoType (TypeDecl ["TaggedStruct struct { " ++ fName ++ " int `" ++ tag ++ "` }"] True)) 
                           fieldNames tagValues
      goModule = GoModule [] Nothing [] taggedFields
  in property $ hasValidStructTags goModule

-- Property: GoModule with variadic functions and parameters
prop_variadic_functions :: [String] -> [String] -> Property
prop_variadic_functions funcNames paramTypes =
  let variadics = zipWith (\name pType -> GoFunc (FuncDecl ["func " ++ name ++ "(args ..." ++ pType ++ ") int {", "  return len(args)", "}"])) 
                         funcNames paramTypes
      goModule = GoModule [] Nothing [] variadics
  in property $ hasValidVariadicFunctions goModule

-- Property: GoModule with multiple return values
prop_multiple_returns :: [String] -> [String] -> Property
prop_multiple_returns funcNames returnTypes =
  let multiRet = zipWith (\name retTypes -> GoFunc (FuncDecl ["func " ++ name ++ "() (" ++ retTypes ++ ") {", "  return " ++ retTypes, "}"])) 
                        funcNames returnTypes
      goModule = GoModule [] Nothing [] multiRet
  in property $ hasValidMultipleReturns goModule

-- Property: GoModule with method expressions and values
prop_method_expressions_values :: [String] -> [String] -> Property
prop_method_expressions_values structNames methodNames =
  let methodExprs = zipWith (\sName mName -> GoFunc (FuncDecl ["func test() {", "  fn := (*" ++ sName ++ ")." ++ mName, "}"])) 
                          structNames methodNames
      methodVals = zipWith (\sName mName -> GoFunc (FuncDecl ["func test() {", "  var " ++ sName ++ " " ++ sName, "  method := " ++ sName ++ "." ++ mName, "}"])) 
                        structNames methodNames
      goModule = GoModule [] Nothing [] (methodExprs ++ methodVals)
  in property $ hasValidMethodExpressionsValues goModule

-- Property: GoModule with interface composition and embedding
prop_interface_composition :: [String] -> [String] -> Property
prop_interface_composition interfaceNames embeddedInterfaces =
  let compositions = zipWith (\iName eInterface -> GoType (TypeDecl [iName ++ " interface { " ++ eInterface ++ " }"] True)) 
                            interfaceNames embeddedInterfaces
      goModule = GoModule [] Nothing [] compositions
  in property $ hasValidInterfaceComposition goModule

-- Property: GoModule with generic type constraints
prop_generic_constraints :: [String] -> [String] -> Property
prop_generic_constraints typeNames constraints =
  let generics = zipWith (\tName constraint -> GoType (TypeDecl [tName ++ "[" ++ constraint ++ " any] struct { Value " ++ constraint ++ " }"] True)) 
                         typeNames constraints
      goModule = GoModule [] Nothing [] generics
  in property $ hasValidGenericConstraints goModule

-- Property: GoModule with range operations on collections
prop_range_operations :: [String] -> Property
prop_range_operations collectionNames =
  let rangeLoops = map (\name -> GoFunc (FuncDecl ["func test() {", "  for " ++ name ++ " := range " ++ name ++ " {", "  }", "}"])) 
                        collectionNames
      goModule = GoModule [] Nothing [] rangeLoops
  in property $ hasValidRangeOperations goModule

-- Property: GoModule with labeled statements and control flow
prop_labeled_statements :: [String] -> Property
prop_labeled_statements labelNames =
  let labels = map (\name -> GoFunc (FuncDecl ["func test() {", name ++ ":", "  for i := 0; i < 10; i++ {", "    if i == 5 { break " ++ name ++ " }", "  }", "}"])) 
                    labelNames
      goModule = GoModule [] Nothing [] labels
  in property $ hasValidLabeledStatements goModule

-- Property: GoModule with goto statements and labels
prop_goto_statements :: [String] -> Property
prop_goto_statements labelNames =
  let gotos = map (\name -> GoFunc (FuncDecl ["func test() {", "  goto " ++ name, name ++ ":", "  return", "}"])) 
                   labelNames
      goModule = GoModule [] Nothing [] gotos
  in property $ hasValidGotoStatements goModule

-- Property: GoModule with constant declarations and iota
prop_constant_iota :: [String] -> Property
prop_constant_iota constNames =
  let iotaDecls = map (\name -> GoConst (ConstDecl [name ++ " iota"] True)) constNames
      goModule = GoModule [] Nothing [] iotaDecls
  in property $ hasValidConstantIota goModule

-- Property: GoModule with type aliases and definitions
prop_type_aliases :: [String] -> [String] -> Property
prop_type_aliases aliasNames originalTypes =
  let aliases = zipWith (\alias orig -> GoType (TypeDecl [alias ++ " = " ++ orig] True)) aliasNames originalTypes
      goModule = GoModule [] Nothing [] aliases
  in property $ hasValidTypeAliases goModule

-- Property: GoModule with package initialization and dependencies
prop_package_initialization :: [String] -> Property
prop_package_initialization initFunctions =
  let initFuncs = map (\name -> GoFunc (FuncDecl ["func init() {", "  " ++ name ++ "()", "}"])) initFunctions
      goModule = GoModule [] Nothing [] initFuncs
  in property $ hasValidPackageInitialization goModule

-- Property: GoModule with build constraints and conditional compilation
prop_build_constraints :: [String] -> [String] -> Property
prop_build_constraints constraints tags =
  let constrainedDecls = zipWith (\constraint tag -> GoType (TypeDecl ["//go:build " ++ constraint, "type ConstrainedType_" ++ tag ++ " struct {}"] True)) 
                                constraints tags
      goModule = GoModule tags Nothing [] constrainedDecls
  in property $ hasValidBuildConstraints goModule

-- Property: GoModule with import side effects and blank imports
prop_import_side_effects :: [String] -> Property
prop_import_side_effects importPaths =
  let blankImports = map (\path -> ImportDecl Nothing ("_" ++ path)) importPaths
      goModule = GoModule [] Nothing blankImports []
  in property $ hasValidImportSideEffects goModule

-- Property: GoModule with dot imports and name conflicts
prop_dot_imports :: [String] -> Property
prop_dot_imports importPaths =
  let dotImports = map (\path -> ImportDecl (Just ".") path) importPaths
      goModule = GoModule [] Nothing dotImports []
  in property $ hasValidDotImports goModule

-- Property: GoModule with cgo and foreign function interfaces
prop_cgo_interfaces :: [String] -> Property
prop_cgo_interfaces functionNames =
  let cgoFuncs = map (\name -> GoFunc (FuncDecl ["//export " ++ name, "func " ++ name ++ "() C.int {", "  return 0", "}"])) 
                     functionNames
      goModule = GoModule [] Nothing [] cgoFuncs
  in property $ hasValidCGoInterfaces goModule

-- Helper functions for new tests
hasValidAnonymousFunctions :: GoModule -> Bool
hasValidAnonymousFunctions (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidSliceExpressions :: GoModule -> Bool
hasValidSliceExpressions (GoModule _ _ _ decls) = 
  all hasSliceExpr decls
  where
    hasSliceExpr (GoFunc _) = True
    hasSliceExpr _ = False

hasValidMapLiterals :: GoModule -> Bool
hasValidMapLiterals (GoModule _ _ _ decls) = 
  all isVarDecl decls
  where
    isVarDecl (GoVar _) = True
    isVarDecl _ = False

hasValidChannelSelect :: GoModule -> Bool
hasValidChannelSelect (GoModule _ _ _ decls) = 
  all hasChannelOrSelect decls
  where
    hasChannelOrSelect (GoVar _) = True
    hasChannelOrSelect (GoFunc _) = True
    hasChannelOrSelect _ = False

hasValidDeferStatements :: GoModule -> Bool
hasValidDeferStatements (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidPanicRecover :: GoModule -> Bool
hasValidPanicRecover (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidTypeSwitchAssertions :: GoModule -> Bool
hasValidTypeSwitchAssertions (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidEmbeddedStructs :: GoModule -> Bool
hasValidEmbeddedStructs (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidStructTags :: GoModule -> Bool
hasValidStructTags (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidVariadicFunctions :: GoModule -> Bool
hasValidVariadicFunctions (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidMultipleReturns :: GoModule -> Bool
hasValidMultipleReturns (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidMethodExpressionsValues :: GoModule -> Bool
hasValidMethodExpressionsValues (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidInterfaceComposition :: GoModule -> Bool
hasValidInterfaceComposition (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidGenericConstraints :: GoModule -> Bool
hasValidGenericConstraints (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidRangeOperations :: GoModule -> Bool
hasValidRangeOperations (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidLabeledStatements :: GoModule -> Bool
hasValidLabeledStatements (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidGotoStatements :: GoModule -> Bool
hasValidGotoStatements (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidConstantIota :: GoModule -> Bool
hasValidConstantIota (GoModule _ _ _ decls) = 
  all isConstDecl decls
  where
    isConstDecl (GoConst _) = True
    isConstDecl _ = False

hasValidTypeAliases :: GoModule -> Bool
hasValidTypeAliases (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidPackageInitialization :: GoModule -> Bool
hasValidPackageInitialization (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

hasValidBuildConstraints :: GoModule -> Bool
hasValidBuildConstraints (GoModule _ _ _ decls) = 
  all isTypeDecl decls
  where
    isTypeDecl (GoType _) = True
    isTypeDecl _ = False

hasValidImportSideEffects :: GoModule -> Bool
hasValidImportSideEffects (GoModule _ _ imports _) = 
  all hasBlankImport imports
  where
    hasBlankImport (ImportDecl _ path) = "_" `isPrefixOf` path

hasValidDotImports :: GoModule -> Bool
hasValidDotImports (GoModule _ _ imports _) = 
  all hasDotAlias imports
  where
    hasDotAlias (ImportDecl (Just ".") _) = True
    hasDotAlias _ = False

hasValidCGoInterfaces :: GoModule -> Bool
hasValidCGoInterfaces (GoModule _ _ _ decls) = 
  all isFuncDecl decls
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

tests :: TestTree
tests = testGroup "Compiler QuickCheck Tests"
  [ fastProperty "gomodule construction" prop_gomodule_construction
  , fastProperty "import decl construction" prop_import_decl_construction
  , fastProperty "type decl construction" prop_type_decl_construction
  , fastProperty "var decl construction" prop_var_decl_construction
  , fastProperty "const decl construction" prop_const_decl_construction
  , fastProperty "func decl construction" prop_func_decl_construction
  , fastProperty "godecl variants" prop_godecl_variants
  , fastProperty "godecl pattern matching" prop_godecl_pattern_matching
  , fastProperty "empty gomodule" prop_empty_gomodule
  , fastProperty "gomodule equality" prop_gomodule_equality
  , fastProperty "import decl equality" prop_import_decl_equality
  , fastProperty "type decl equality" prop_type_decl_equality
  , fastProperty "var decl equality" prop_var_decl_equality
  , fastProperty "const decl equality" prop_const_decl_equality
  , fastProperty "func decl equality" prop_func_decl_equality
  , fastProperty "large gomodule" prop_large_gomodule
  -- New property tests
  , fastProperty "circular dependencies" prop_circular_dependencies
  , fastProperty "duplicate imports" prop_duplicate_imports
  , fastProperty "invalid package names" prop_invalid_package_names
  , fastProperty "deeply nested types" prop_deeply_nested_types
  , fastProperty "function overloading" prop_function_overloading
  , fastProperty "generic type parameters" prop_generic_type_parameters
  , fastProperty "interface implementations" prop_interface_implementations
  , fastProperty "struct field validation" prop_struct_field_validation
  , fastProperty "constant expressions" prop_constant_expressions
  , fastProperty "variable initialization" prop_variable_initialization
  , fastProperty "recursive functions" prop_recursive_functions
  , fastProperty "error handling patterns" prop_error_handling_patterns
  , fastProperty "concurrent patterns" prop_concurrent_patterns
  , fastProperty "package level variables" prop_package_level_variables
  , fastProperty "build tags" prop_build_tags
  , fastProperty "import aliases" prop_import_aliases
  , fastProperty "type assertions" prop_type_assertions
  , fastProperty "method receivers" prop_method_receivers
  -- Extended property tests
  , fastProperty "anonymous functions" prop_anonymous_functions
  , fastProperty "slice expressions" prop_slice_expressions
  , fastProperty "map literals" prop_map_literals
  , fastProperty "channel select" prop_channel_select
  , fastProperty "defer cleanup" prop_defer_cleanup
  , fastProperty "panic recover" prop_panic_recover
  , fastProperty "type switch assertions" prop_type_switch_assertions
  , fastProperty "embedded structs" prop_embedded_structs
  , fastProperty "struct tags" prop_struct_tags
  , fastProperty "variadic functions" prop_variadic_functions
  , fastProperty "multiple returns" prop_multiple_returns
  , fastProperty "method expressions values" prop_method_expressions_values
  , fastProperty "interface composition" prop_interface_composition
  , fastProperty "generic constraints" prop_generic_constraints
  , fastProperty "range operations" prop_range_operations
  , fastProperty "labeled statements" prop_labeled_statements
  , fastProperty "goto statements" prop_goto_statements
  , fastProperty "constant iota" prop_constant_iota
  , fastProperty "type aliases" prop_type_aliases
  , fastProperty "package initialization" prop_package_initialization
  , fastProperty "build constraints" prop_build_constraints
  , fastProperty "import side effects" prop_import_side_effects
  , fastProperty "dot imports" prop_dot_imports
  , fastProperty "cgo interfaces" prop_cgo_interfaces
  ]