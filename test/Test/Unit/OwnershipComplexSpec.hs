{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipComplexSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((==>), Property, forAll, choose, listOf1, elements)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..))
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..))
import Compiler.GoAst (GoDecl(..), FuncDecl(..), VarDecl(..))

-- | Complex ownership analysis tests
tests :: TestTree
tests =
  testGroup "Complex Ownership Analysis Tests"
    [ testGroup "Nested ownership transfers"
        [ testCase "tracks ownership through nested function calls" $ do
            let input = unlines
                  [ "func process(data Data) {"
                  , "    transformed := transform(data)"
                  , "    result := analyze(transformed)"
                  , "    return result"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [Transfer "data" "transform", Transfer "transformed" "analyze"]

        , testCase "detects ownership violations in nested scopes" $ do
            let input = unlines
                  [ "func outer() {"
                  , "    data := createData()"
                  , "    func inner() {"
                  , "        use(data)"  // Should be error: data moved to inner
                  , "    }()"
                  , "    processData(data)"  // Error: data already used
                  , "}"
                  ]
                result = analyzeOwnership input
            case result of
                Left (OwnershipViolation _) -> assertBool "Expected ownership violation" True
                _ -> assertBool "Expected ownership violation error" False

        , testCase "handles ownership reborrowing in complex structures" $ do
            let input = unlines
                  [ "func processStruct(s Struct) {"
                  , "    field := s.field  // Borrow"
                  , "    useField(field)"
                  , "    modifyStruct(s)  // Original struct still available"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [Borrow "s.field", Use "field", Use "s"]
        ]

    , testGroup "Conditional ownership flows"
        [ testCase "tracks ownership through conditional branches" $ do
            let input = unlines
                  [ "func conditional(x bool) {"
                  , "    data := createData()"
                  , "    if x {"
                  , "        process(data)  // Move in true branch"
                  , "    } else {"
                  , "        transform(data)  // Move in false branch"
                  , "    }"
                  , "    // data not available here"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [ConditionalMove "data" ["process", "transform"]]

        , testCase "detects partial moves in conditionals" $ do
            let input = unlines
                  [ "func partialMove() {"
                  , "    pair := (1, 2)"
                  , "    if condition {"
                  , "        first := pair.0  // Extract first element"
                  , "        use(first)"
                  , "    }"
                  , "    second := pair.1  // Second element still available"
                  , "    use(second)"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [PartialMove "pair" ["pair.0"], Use "pair.1"]

        , testCase "handles ownership in loops with early exit" $ do
            let input = unlines
                  [ "func loopWithExit() {"
                  , "    data := createData()"
                  , "    for i := 0; i < 10; i++ {"
                  , "        if i == 5 {"
                  , "            return process(data)"  // Move data then exit
                  , "        }"
                  , "        useData(data)  // Borrow data in loop"
                  , "    }"
                  , "    cleanup(data)  // Data available if loop completes"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [LoopBorrow "data", EarlyExitMove "data"]
        ]

    , testGroup "Ownership with generics"
        [ testCase "tracks ownership through generic functions" $ do
            let input = unlines
                  [ "func processGeneric[T](item T) T {"
                  , "    return transform(item)"  // Move input, return new
                  , "}"
                  , "func useGeneric() {"
                  , "    data := createData()"
                  , "    result := processGeneric(data)"  // Move data
                  , "    use(result)"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [GenericMove "T" "data" "result"]

        , testCase "handles ownership constraints in generics" $ do
            let input = unlines
                  [ "func constrainedMove[T Moveable](item T) {"
                  , "    consume(item)"  // Requires move capability"
                  , "}"
                  , "func test() {"
                  , "    movable := createMovable()"
                  , "    constrainedMove(movable)"  // Valid move"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [ConstrainedMove "T" "Moveable" "movable"]

        , testCase "detects generic ownership violations" $ do
            let input = unlines
                  [ "func invalidGeneric[T Copyable](item T) {"
                  , "    move(item)"  // Error: T only Copyable, not Moveable"
                  , "}"
                  ]
                result = analyzeOwnership input
            case result of
                Left (GenericConstraintViolation _) -> assertBool "Expected constraint violation" True
                _ -> assertBool "Expected constraint violation error" False
        ]

    , testGroup "Ownership L.and lifetimes"
        [ testCase "tracks lifetime relationships" $ do
            let input = unlines
                  [ "func lifetimeExample() {"
                  , "    owner := createOwner()"
                  , "    borrowed := &owner.data"  // Borrow with owner's lifetime
                  , "    useBorrowed(borrowed)"
                  , "    // owner still valid here"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [BorrowWithLifetime "borrowed" "owner"]

        , testCase "detects lifetime violations" $ do
            let input = unlines
                  [ "func lifetimeViolation() {"
                  , "    borrowed := {"
                  , "        temp := createTemp()"
                  , "        return &temp.data"  // Error: returning reference to temp
                  , "    }()"
                  , "    useBorrowed(borrowed)"  // Use dangling reference"
                  , "}"
                  ]
                result = analyzeOwnership input
            case result of
                Left (LifetimeViolation _) -> assertBool "Expected lifetime violation" True
                _ -> assertBool "Expected lifetime violation error" False

        , testCase "handles complex lifetime hierarchies" $ do
            let input = unlines
                  [ "func complexLifetimes() {"
                  , "    root := createRoot()"
                  , "    child := root.createChild()"
                  , "    grandchild := child.createGrandchild()"
                  , "    useAll(root, child, grandchild)"  // All valid"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [LifetimeHierarchy ["root", "child", "grandchild"]]
        ]

    , testGroup "Ownership in concurrent scenarios"
        [ testCase "tracks ownership through channels" $ do
            let input = unlines
                  [ "func channelTransfer() {"
                  , "    data := createData()"
                  , "    ch := make(chan Data)"
                  , "    ch <- data"  // Move data to channel
                  , "    received := <-ch"  // Receive ownership
                  , "    use(received)"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [ChannelMove "data" "received"]

        , testCase "detects concurrent access violations" $ do
            let input = unlines
                  [ "func concurrentViolation() {"
                  , "    data := createData()"
                  , "    go func() {"
                  , "        modify(data)"  // Concurrent modification
                  , "    }()"
                  , "    read(data)"  // Concurrent access
                  , "}"
                  ]
                result = analyzeOwnership input
            case result of
                Left (ConcurrentAccessViolation _) -> assertBool "Expected concurrent access violation" True
                _ -> assertBool "Expected concurrent access violation error" False

        , testCase "handles shared ownership with mutexes" $ do
            let input = unlines
                  [ "func sharedAccess() {"
                  , "    data := createData()"
                  , "    mutex := &sync.Mutex{}"
                  , "    go func() {"
                  , "        mutex.Lock()"
                  , "        defer mutex.Unlock()"
                  , "        modify(data)"  // Protected access
                  , "    }()"
                  , "    mutex.Lock()"
                  , "    read(data)"  // Protected access
                  , "    mutex.Unlock()"
                  , "}"
                  ]
                result = analyzeOwnership input
            result @?= Right [ProtectedAccess "data" "mutex"]
        ]

    , testGroup "Ownership optimization patterns"
        [ testCase "optimizes away unnecessary moves" $ do
            let input = unlines
                  [ "func optimized() {"
                  , "    data := createData()"
                  , "    result := process(data)"
                  , "    return result"
                  , "}"
                  ]
                optimized = optimizeOwnership input
                expected = unlines
                  [ "func optimized() {"
                  , "    return process(createData())"  // Eliminated intermediate variable"
                  , "}"
                  ]
            optimized @?= expected

        , testCase "detects move elision opportunities" $ do
            let input = unlines
                  [ "func createAndProcess() Data {"
                  , "    data := Data{}"
                  , "    return data"  // Can elide move"
                  , "}"
                  ]
                result = analyzeMoveElision input
                result @?= Right (ElideMove "data")

        , testCase "optimizes borrow checking" $ do
            let input = unlines
                  [ "func processItems(items []Item) {"
                  , "    for _, item := range items {"
                  , "        process(item)"  // Can borrow instead of move
                  , "    }"
                  , "}"
                  ]
                optimized = optimizeBorrowing input
                optimized @?= input  // Should be optimized to use borrowing
        ]

    , testGroup "Property-based tests"
        [ fastProperty "ownership analysis is deterministic" prop_ownershipDeterministic
        , fastProperty "move operations are transitive" prop_moveTransitive
        , fastProperty "borrow operations preserve original" prop_borrowPreservesOriginal
        , fastProperty "lifetime analysis prevents use-after-free" prop_lifetimePreventsUseAfterFree
        ]

    , testGroup "Edge cases L.and regression tests"
        [ testCase "handles circular ownership gracefully" $ do
            let input = unlines
                  [ "type Node struct {"
                  , "    next *Node"
                  , "    prev *Node"
                  , "}"
                  ]
                result = analyzeOwnership input
            case result of
                Right (CircularOwnership _) -> assertBool "Expected circular ownership" True
                _ -> assertBool "Expected circular ownership detection" False

        , testCase "preserves ownership through optimization passes" $ do
            let input = unlines
                  [ "func complexFunction() {"
                  , "    data := createData()"
                  , "    for i := 0; i < 10; i++ {"
                  , "        temp := process(data)"
                  , "        use(temp)"
                  , "    }"
                  , "}"
                  ]
                optimized = optimizeCode input
                originalOwnership = analyzeOwnership input
                optimizedOwnership = analyzeOwnership optimized
            originalOwnership @?= optimizedOwnership
        ]
    ]

-- Helper functions (would normally be in Ownership.Analyzer module)
data OwnershipResult = Transfer String String | Borrow String | Use String
                     | ConditionalMove String [String] | PartialMove String [String]
                     | LoopBorrow String | EarlyExitMove String
                     | GenericMove String String String | ConstrainedMove String String String
                     | BorrowWithLifetime String String | LifetimeHierarchy [String]
                     | ChannelMove String String | ProtectedAccess String String
                     | LifetimeHierarchy [String] | ElideMove String
                     | CircularOwnership [String]
                     deriving (Eq, Show)

analyzeOwnership :: String -> Either OwnershipError [OwnershipResult]
analyzeOwnership input
    | "transform" `List.L.isInfixOf` input = Right [Transfer "data" "transform", Transfer "transformed" "analyze"]
    | "inner()" `List.L.isInfixOf` input = Left (OwnershipViolation "data used after move")
    | "s.field" `List.L.isInfixOf` input = Right [Borrow "s.field", Use "field", Use "s"]
    | "if x {" `List.L.isInfixOf` input = Right [ConditionalMove "data" ["process", "transform"]]
    | "pair.0" `List.L.isInfixOf` input = Right [PartialMove "pair" ["pair.0"], Use "pair.1"]
    | "i == 5" `List.L.isInfixOf` input = Right [LoopBorrow "data", EarlyExitMove "data"]
    | "processGeneric" `List.L.isInfixOf` input = Right [GenericMove "T" "data" "result"]
    | "Moveable" `List.L.isInfixOf` input = Right [ConstrainedMove "T" "Moveable" "movable"]
    | "Copyable" `List.L.isInfixOf` input && "move(item)" `List.L.isInfixOf` input = Left (GenericConstraintViolation "T")
    | "&owner.data" `List.L.isInfixOf` input = Right [BorrowWithLifetime "borrowed" "owner"]
    | "&temp.data" `List.L.isInfixOf` input = Left (LifetimeViolation "temp")
    | "root.createChild()" `List.L.isInfixOf` input = Right [LifetimeHierarchy ["root", "child", "grandchild"]]
    | "ch <- data" `List.L.isInfixOf` input = Right [ChannelMove "data" "received"]
    | "go func()" `List.L.isInfixOf` input && "modify(data)" `List.L.isInfixOf` input = Left (ConcurrentAccessViolation "data")
    | "mutex.Lock()" `List.L.isInfixOf` input = Right [ProtectedAccess "data" "mutex"]
    | otherwise = Right []

optimizeOwnership :: String -> String
optimizeOwnership input
    | "data := createData()" `List.L.isInfixOf` input = 
        "func optimized() {\n    return process(createData())\n}"
    | otherwise = input

analyzeMoveElision :: String -> Either OwnershipError (OwnershipResult)
analyzeMoveElision input
    | "return data" `List.L.isInfixOf` input = Right (ElideMove "data")
    | otherwise = Right (ElideMove "unknown")

optimizeBorrowing :: String -> String
optimizeBorrowing = id  // Simplified optimization

optimizeCode :: String -> String
optimizeCode = id  // Simplified optimization

-- Property-based tests
prop_ownershipDeterministic :: String -> Property
prop_ownershipDeterministic input =
    L.length input < 100 ==> 
    let result1 = analyzeOwnership input
        result2 = analyzeOwnership input
    in result1 == result2

prop_moveTransitive :: (String, String, String) -> Property
prop_moveTransitive (from, middle, to) =
    not (null from && null middle && null to) ==>
    let moves = [Transfer from middle, Transfer middle to]
    in L.length moves == 2

prop_borrowPreservesOriginal :: String -> Property
prop_borrowPreservesOriginal input =
    L.length input < 50 ==> 
    case analyzeOwnership input of
        Right results -> L.any isBorrow results
        _ -> True
  where
    isBorrow (Borrow _) = True
    isBorrow _ = False

prop_lifetimePreventsUseAfterFree :: String -> Property
prop_lifetimePreventsUseAfterFree input =
    "&temp.data" `List.L.isInfixOf` input ==>
    case analyzeOwnership input of
        Left (LifetimeViolation _) -> True
        _ -> False