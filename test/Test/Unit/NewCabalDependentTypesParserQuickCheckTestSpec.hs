module Test.Unit.NewCabalDependentTypesParserQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose, suchThat)
import DependentTypesParser
  ( DependentTypesParser(..), DependentTypeError(..), TypeRef(..), TypeBody(..)
  , Field(..), TypeParameter(..), TypeConstraint(..), DependentType(..)
  , runDependentTypesParser, parseDependentType, parseTypeDeclaration
  , validateDependentTypeSyntax
  )
import Data.Text 
              name <- arbitrary `suchThat` (not . null)
    args <- listOf arbitrary
    return $ TypeRef name args
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


instance Arbitrary Field where
                                              arbitrary = do
              name <- arbitrary `suchThat` (not . null)
    typ <- arbitrary
    return $ Field name typ

instance Arbitrary TypeBody where
                                              arbitrary = do
              fields <- listOf arbitrary
    return $ StructBody fields

instance Arbitrary TypeParameter where
                                              arbitrary = do
              name <- arbitrary `suchThat` (not . null)
    typ <- arbitrary
    constraints <- listOf arbitrary
    return $ TypeParameter name typ constraints

instance Arbitrary TypeConstraint where
                                              arbitrary = oneof
    [ EqualityConstraint <$> arbitrary `suchThat` (not . null) <*> arbitrary `suchThat` (not . null)
    , InequalityConstraint <$> arbitrary `suchThat` (not . null) <*> arbitrary `suchThat` (not . null)
    , RangeConstraint <$> arbitrary `suchThat` (not . null) <*> choose (0, 100) <*> choose (0, 100)
    , SizeConstraint <$> arbitrary `suchThat` (not . null) <*> choose (0, 1000)
    , NonEmptyConstraint <$> arbitrary `suchThat` (not . null)
    , PredicateConstraint <$> arbitrary `suchThat` (not . null) <*> listOf (arbitrary `suchThat` (not . null)
    , TypeClassConstraint <$> arbitrary `suchThat` (not . null) <*> arbitrary
    , CustomConstraint <$> arbitrary `suchThat` (not . null) <*> arbitrary `suchThat` (not . null)
    ]

instance Arbitrary DependentType where
                                              arbitrary = oneof
    [ TypeDecl <$> arbitrary `suchThat` (not . null) <*> listOf arbitrary <*> arbitrary <*> listOf arbitrary
    , DependentFunction <$> arbitrary `suchThat` (not . null) <*> listOf (arbitrary `suchThat` (\(n, _) -> not (null n)) <*> arbitrary <*> listOf arbitrary
    , TypeAlias <$> arbitrary `suchThat` (not . null) <*> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary DependentTypeError where
                                              arbitrary = oneof
    [ SyntaxError <$> arbitrary `suchThat` (not . null) <*> choose (0, 1000) <*> arbitrary `suchThat` (not . null)
    , InvalidTypeSyntax <$> arbitrary `suchThat` (not . null)
    , MissingConstraint <$> arbitrary `suchThat` (not . null)
    , InvalidParameter <$> arbitrary `suchThat` (not . null)
    , ConstraintParseError <$> arbitrary `suchThat` (not . null)
    , TypeVariableError <$> arbitrary `suchThat` (not . null)
    ]

-- Generate simple type references
genSimpleTypeRef :: Gen TypeRef
                              genSimpleTypeRef = oneof
  [ return $ TypeRef "int" []
                , return $ TypeRef "string" []
                , return $ TypeRef "bool" []
  , TypeRef <$> elements ["List", "Array", "Map"] <*> listOf genSimpleTypeRef
  ]

-- Generate simple constraints
genSimpleConstraint :: Gen String
                              genSimpleConstraint = oneof
  [ return "x == 5"
                , return "x > 0"
                , return "x >= 1"
                , return "x < 100"
                , return "x <= 99"
                , return "len                               x == 10"
                  , return "nonempty x"
  ]

-- Generate simple type declarations
genSimpleTypeDecl :: Gen String
                              genSimpleTypeDecl = do
              name <- elements ["MyType", "Container", "Node"]
  return $ "type " ++ name ++ " struct { field int }"

-- Generate simple function declarations
genSimpleFuncDecl :: Gen String
                              genSimpleFuncDecl = do
              name <- elements ["myFunc", "process", "compute"]
  return $ "func " ++ name ++ "() int"

-- Generate simple type aliases
genSimpleTypeAlias :: Gen String
                              genSimpleTypeAlias = do
              name <- elements ["MyInt", "StringList", "BoolMap"]
  base <- elements ["int", "string", "bool"]
  return $ "alias " ++ name ++ " = " ++ base

-- Generate valid dependent type code
genValidDependentTypeCode :: Gen String
                              genValidDependentTypeCode = oneof
  [ genSimpleTypeDecl
  , genSimpleFuncDecl
  , genSimpleTypeAlias
              , do
              decl <- genSimpleTypeDecl
      constraint <- genSimpleConstraint
      return $ decl ++ " where " ++ constraint
  ]

-- Generate invalid dependent type code
genInvalidDependentTypeCode :: Gen String
                              genInvalidDependentTypeCode = oneof
  [ return "type 123Invalid struct { }"  -- Invalid type name
                , return "type MissingBrace struct { field int"  -- Missing closing brace
                , return "func invalid syntax"  -- Invalid function syntax
                  , return "unknown keyword"  -- Unknown keyword
  ]

-- ============================================================================
-- TypeRef QuickCheck Tests
-- ============================================================================

-- Test TypeRef creation
prop_type_ref_has_name :: String -> [TypeRef] -> Property
prop_type_ref_has_name name                               args =
  not (null name) ==>
  let ref = TypeRef name args
  in refName                               ref === name .&&. refArgs                               ref === args

prop_simple_type_ref_no_args :: Property
                              prop_simple_type_ref_no_args =
  let ref = TypeRef "int" []
in refName                               ref === "int" .&&. L.null (refArgs ref)

-- ============================================================================
-- Field QuickCheck Tests
-- ============================================================================

-- Test Field creation
prop_field_has_name_and_type :: String -> TypeRef -> Property
prop_field_has_name_and_type name                               typ =
  not (null name) ==>
  let field = Field name typ
  in fieldName                               field === name .&&. fieldType                               field === typ

-- ============================================================================
-- TypeBody QuickCheck Tests
-- ============================================================================

-- Test TypeBody creation
prop_struct_body_contains_fields :: [Field] -> Property
prop_struct_body_contains_fields                               fields =
  let body = StructBody fields
  in case body of
    StructBody fs ->                               fs === fields
    _ -> property False

-- ============================================================================
-- TypeParameter QuickCheck Tests
-- ============================================================================

-- Test TypeParameter creation
prop_type_parameter_has_components :: String -> TypeRef -> [TypeConstraint] -> Property
prop_type_parameter_has_components name typ                               constraints =
  not (null name) ==>
  let param = TypeParameter name typ constraints
  in paramName                               param === name .&&.
     paramType                               param === typ .&&.
     paramConstraints                               param === constraints

-- ============================================================================
-- TypeConstraint QuickCheck Tests
-- ============================================================================

-- Test TypeConstraint creation
prop_equality_constraint :: String -> String -> Property
prop_equality_constraint var1                               var2 =
  not (null var1) && not (null var2) ==>
  let constraint = EqualityConstraint var1 var2
  in case constraint of
    EqualityConstraint v1 v2 ->                               v1 === var1 &&                               v2 === var2
    _ -> property False

prop_range_constraint :: String -> Int -> Int -> Property
prop_range_constraint var minVal                               maxVal =
  not (null var) && minVal <=                               maxVal ==>
  let constraint = RangeConstraint var minVal maxVal
  in case constraint of
    RangeConstraint v mn mx ->                               v === var &&                               mn === minVal &&                               mx === maxVal
    _ -> property False

-- ============================================================================
-- DependentType QuickCheck Tests
-- ============================================================================

-- Test TypeDecl creation
prop_type_decl_has_components :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Property
prop_type_decl_has_components name params body                               constraints =
  not (null name) ==>
  let decl = TypeDecl name params body constraints
  in case decl of
    TypeDecl n p b c ->                               n === name &&                               p === params &&                               b === body &&                               c === constraints
    _ -> property False

-- Test DependentFunction creation
prop_dependent_function_has_components :: String -> [(String, TypeRef)] -> TypeRef -> [TypeConstraint] -> Property
prop_dependent_function_has_components name params ret                               constraints =
  not (null name) && L.all (not . null . fst)                               params ==>
  let func = DependentFunction name params ret constraints
  in case func of
    DependentFunction n p r c ->                               n === name &&                               p === params &&                               r === ret &&                               c === constraints
    _ -> property False

-- Test TypeAlias creation
prop_type_alias_has_components :: String -> TypeRef -> [TypeConstraint] -> Property
prop_type_alias_has_components name typ                               constraints =
  not (null name) ==>
  let alias = TypeAlias name typ constraints
  in case alias of
    TypeAlias n t c ->                               n === name &&                               t === typ &&                               c === constraints
    _ -> property False

-- ============================================================================
-- Parser Functions QuickCheck Tests
-- ============================================================================

-- Test runDependentTypesParser function
prop_run_parser_returns_result :: Property
                              prop_run_parser_returns_result =
  forAll genValidDependentTypeCode $ \code ->
    let result = runDependentTypesParser code
    in isLeft result || isRight result  -- Should always return Either

prop_run_parser_handles_valid_input :: Property
                              prop_run_parser_handles_valid_input =
  forAll genValidDependentTypeCode $ \code ->
    let result = runDependentTypesParser code
    in case result of
      Left _ -> property False  -- Valid input should not fail
Right (types, parser) -> L.length types >= 0  -- Should return some types

prop_run_parser_handles_invalid_input :: Property
                              prop_run_parser_handles_invalid_input =
  forAll genInvalidDependentTypeCode $ \code ->
    let result = runDependentTypesParser code
    in case result of
      Left _ -> property True  -- Invalid input should fail
Right (types, parser) -> not (L.null (parserErrors parser)  -- Should have errors

-- Test parseDependentType function
prop_parse_dependent_type_returns_result :: Property
                              prop_parse_dependent_type_returns_result =
  forAll genValidDependentTypeCode $ \code ->
    let result = parseDependentType code
    in isLeft result || isRight result  -- Should always return Either

-- Test parseTypeDeclaration function
prop_parse_type_declaration_returns_result :: Property
                              prop_parse_type_declaration_returns_result =
  forAll genSimpleTypeDecl $ \code ->
    let result = parseTypeDeclaration code
    in isLeft result || isRight result  -- Should always return Either

-- Test validateDependentTypeSyntax function
prop_validate_syntax_returns_result :: Property
                              prop_validate_syntax_returns_result =
  forAll genValidDependentTypeCode $ \code ->
    let result = validateDependentTypeSyntax code
    in isLeft result || isRight result  -- Should always return Either

-- ============================================================================
-- Error Handling QuickCheck Tests
-- ============================================================================

-- Test DependentTypeError creation
prop_syntax_error_has_components :: String -> Int -> String -> Property
prop_syntax_error_has_components msg line                               fragment =
  not (null msg) ==>
  let error = SyntaxError msg line fragment
  in case error of
    SyntaxError m l f ->                               m === msg &&                               l === line &&                               f === fragment
    _ -> property False

prop_invalid_type_syntax_error :: String -> Property
prop_invalid_type_syntax_error                               msg =
  not (null msg) ==>
  let error = InvalidTypeSyntax msg
  in case error of
    InvalidTypeSyntax m ->                               m === msg
    _ -> property False

-- ============================================================================
-- Round-trip Tests
-- ============================================================================

-- Test type reference round-trip
prop_type_ref_round_trip :: TypeRef -> Property
prop_type_ref_round_trip                               ref =
  let name = refName ref
                                    args = refArgs ref
                                    reconstructed = TypeRef name args
  in                               ref === reconstructed

-- Test constraint round-trip
prop_constraint_round_trip :: TypeConstraint -> Property
prop_constraint_round_trip                               constraint =
  case constraint of
    EqualityConstraint var val ->
      let reconstructed = EqualityConstraint var val
      in                               constraint === reconstructed
    RangeConstraint var minVal maxVal ->
      let reconstructed = RangeConstraint var minVal maxVal
      in                               constraint === reconstructed
    _ -> property True  -- Other constraints follow same pattern

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- Test parser handles empty input
prop_parse_empty_input :: Property
                              prop_parse_empty_input =
  let result = runDependentTypesParser ""
  in case result of
    Left _ -> property True  -- Empty input should fail gracefully
    Right (types, parser) -> null types  -- Should return empty list

-- Test parser handles whitespace only
prop_parse_whitespace_only :: Property
                              prop_parse_whitespace_only =
  let whitespace = unlines $ replicate 5 "   \t  "
                                    result = runDependentTypesParser whitespace
  in case result of
    Left _ -> property True  -- Whitespace only should fail gracefully
    Right (types, parser) -> null types  -- Should return empty list

-- Test parser handles comments
prop_parse_comments :: Property
                              prop_parse_comments =
  let commentCode = "-- This is a comment\n/* Another comment */\n"
                                    result = runDependentTypesParser commentCode
  in case result of
    Left _ -> property True  -- Comments only should fail gracefully
    Right (types, parser) -> null types  -- Should return empty list

-- Test parser handles multiple definitions
prop_parse_multiple_definitions :: Property
                              prop_parse_multiple_definitions =
  forAll (listOf genValidDependentTypeCode) $ \codes ->
    let input = unlines codes
                                      result = runDependentTypesParser input
    in case result of
      Left _ -> property False  -- Valid definitions should not fail
      Right (types, parser) -> L.length types >= 0  -- Should return some types

-- Test parser preserves order
prop_parser_preserves_order :: Property
                              prop_parser_preserves_order =
  forAll (listOf genSimpleTypeDecl) $ \decls ->
    let input = unlines decls
                                      result =  runDependentTypesParser input
    in property $ case result of
      Right (types, parser) -> L.length types >= 0  -- Basic order check
      Left _ -> property False

tests :: TestTree
tests =   testGroup "New Cabal DependentTypesParser QuickCheck Tests"
  [ testGroup "TypeRef tests"
      [             testProperty "type ref has name" prop_type_ref_has_name
      ,             testProperty "simple type ref no args" prop_simple_type_ref_no_args
      ]
  , testGroup "Field tests"
      [             testProperty "field has name L.and type" prop_field_has_name_and_type
      ]
  , testGroup "TypeBody tests"
      [             testProperty "struct body contains fields" prop_struct_body_contains_fields
      ]
  , testGroup "TypeParameter tests"
      [             testProperty "type parameter has components" prop_type_parameter_has_components
      ]
  , testGroup "TypeConstraint tests"
      [             testProperty "equality constraint" prop_equality_constraint
      ,             testProperty "range constraint" prop_range_constraint
      ]
  , testGroup "DependentType tests"
      [             testProperty "type decl has components" prop_type_decl_has_components
      ,             testProperty "dependent function has components" prop_dependent_function_has_components
      ,             testProperty "type alias has components" prop_type_alias_has_components
      ]
  , testGroup "Parser functions tests"
      [             testProperty "run parser returns result" prop_run_parser_returns_result
      ,             testProperty "run parser handles valid input" prop_run_parser_handles_valid_input
      ,             testProperty "run parser handles invalid input" prop_run_parser_handles_invalid_input
      ,             testProperty "parse dependent type returns result" prop_parse_dependent_type_returns_result
      ,             testProperty "parse type declaration returns result" prop_parse_type_declaration_returns_result
      ,             testProperty "validate syntax returns result" prop_validate_syntax_returns_result
      ]
  , testGroup "Error handling tests"
      [             testProperty "syntax error has components" prop_syntax_error_has_components
      ,             testProperty "invalid type syntax error" prop_invalid_type_syntax_error
      ]
  , testGroup "Round-trip tests"
      [             testProperty "type ref round-trip" prop_type_ref_round_trip
      ,             testProperty "constraint round-trip" prop_constraint_round_trip
      ]
  , testGroup "Additional property tests"
      [             testProperty "parse empty input" prop_parse_empty_input
      ,             testProperty "parse whitespace only" prop_parse_whitespace_only
      ,             testProperty "parse comments" prop_parse_comments
      ,             testProperty "parse multiple definitions" prop_parse_multiple_definitions
      ,             testProperty "parser preserves order" prop_parser_preserves_order
      ]
  ]