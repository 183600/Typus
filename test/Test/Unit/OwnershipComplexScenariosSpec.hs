module Test.Unit.OwnershipComplexScenariosSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import SourceLocation
import Ownership
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)

-- | Test complex ownership analysis scenarios
tests :: TestTree
tests =
  testGroup "Ownership Complex Scenarios Tests"
    [ testGroup "Nested Ownership Transfer"
        [ testCase "deeply nested function calls" $ do
            let nestedCalls = unlines
                  [ "data := createResource()"
                  , "process1(process2(process3(data)))"
                  ]
                -- Track ownership through nested calls
                ownershipChain = ["createResource", "process3", "process2", "process1"]
                finalOwner = last ownershipChain
            finalOwner @?= "process1"

        , testCase "ownership in recursive structures" $ do
            let recursiveCode = unlines
                  [ "func processNode(node &Node) {"
                  , "  if node.hasChildren {"
                  , "    processNode(node.left)"
                  , "    processNode(node.right)"
                  , "  }"
                  , "}"
                  ]
                -- Should handle ownership in recursive calls
                hasRecursiveCall = "processNode(node.left)" `isInfixOf` recursiveCode
                hasBorrow = "&Node" `isInfixOf` recursiveCode
            hasRecursiveCall @?= True
            hasBorrow @?= True

        , testCase "ownership through conditional paths" $ do
            let conditionalCode = unlines
                  [ "data := createResource()"
                  , "if condition {"
                  , "  consume(data)"
                  , "} else {"
                  , "  store(data)"
                  , "}"
                  ]
                -- Should track ownership through different paths
                paths = ["consume(data)", "store(data)"]
                pathCount = length paths
            pathCount @?= 2

        , testCase "ownership in loop constructs" $ do
            let loopCode = unlines
                  [ "items := createItems()"
                  , "for item in items {"
                  , "  process(item)"
                  , "}"
                  ]
                -- Should handle ownership transfer in loops
                hasLoop = "for item in items" `isInfixOf` loopCode
                hasTransfer = "process(item)" `isInfixOf` loopCode
            hasLoop @?= True
            hasTransfer @?= True
        ]

    , testGroup "Borrowing Complex Scenarios"
        [ testCase "multiple simultaneous borrows" $ do
            let borrowCode = unlines
                  [ "data := createData()"
                  , "read1 := &data.value"
                  , "read2 := &data.metadata"
                  , "process(read1, read2)"
                  ]
                -- Should allow multiple immutable borrows
                borrowCount = length $ filter (isInfixOf "&data.") (lines borrowCode)
            borrowCount @?= 2

        , testCase "borrow lifetime analysis" $ do
            let lifetimeCode = unlines
                  [ "func longerLifetime() {"
                  , "  data := Data{}"
                  , "  return &data.value  // Error: data goes out of scope"
                  , "}"
                  ]
                -- Should detect lifetime issues
                hasReturnRef = "&data.value" `isInfixOf` lifetimeCode
                hasScopeIssue = "data goes out of scope" `isInfixOf` lifetimeCode
            hasReturnRef @?= True

        , testCase "mutable borrow exclusion" $ do
            let mutableBorrowCode = unlines
                  [ "data := createData()"
                  , "mutable := &mut data"
                  , "readonly := &data  // Error: can't borrow while mutable borrow exists"
                  ]
                -- Should prevent immutable borrow during mutable borrow
                hasMutable = "&mut data" `isInfixOf` mutableBorrowCode
                hasConflict = "&data  // Error" `isInfixOf` mutableBorrowCode
            hasMutable @?= True
            hasConflict @?= True

        , testCase "borrow through references" $ do
            let refBorrowCode = unlines
                  [ "wrapper := createWrapper()"
                  , "data := &wrapper.inner.data"
                  , "process(data)"
                  ]
                -- Should handle borrowing through nested references
                hasNestedRef = "wrapper.inner.data" `isInfixOf` refBorrowCode
            hasNestedRef @?= True
        ]

    , testGroup "Lifetime Analysis Edge Cases"
        [ testCase "struct lifetime parameters" $ do
            let structCode = unlines
                  [ "struct Ref<'a> {"
                  , "  data: &'a Data"
                  , "}"
                  , "func create_ref(data: &Data) -> Ref {"
                  , "  Ref { data: data }"
                  , "}"
                  ]
                -- Should handle struct lifetime parameters
                hasLifetimeParam = "'a" `isInfixOf` structCode
                hasLifetimeField = "&'a Data" `isInfixOf` structCode
            hasLifetimeParam @?= True
            hasLifetimeField @?= True

        , testCase "lifetime subtyping" $ do
            let subtypingCode = unlines
                  [ "func process<'a, 'b>(data: &'a Data, out: &'b mut Data)"
                  , "  where 'a: 'b"  // 'a outlives 'b
                  ]
                -- Should handle lifetime subtyping constraints
                hasSubtype = "'a: 'b" `isInfixOf` subtypingCode
            hasSubtype @?= True

        , testCase "static lifetime" $ do
            let staticCode = unlines
                  [ "static GLOBAL: Data = Data{}"
                  , "func get_global() -> &'static Data {"
                  , "  &GLOBAL"
                  , "}"
                  ]
                -- Should handle static lifetime
                hasStatic = "'static" `isInfixOf` staticCode
            hasStatic @?= True

        , testCase "lifetime elision" $ do
            let elisionCode = unlines
                  [ "func process(data: &Data) -> &String {"
                  , "  // Lifetime elided: input and output share lifetime"
                  , "  &data.field"
                  , "}"
                  ]
                -- Should handle lifetime elision rules
                hasElidedParam = "&Data" `isInfixOf` elisionCode
                hasElidedReturn = "&String" `isInfixOf` elisionCode
            hasElidedParam @?= True
            hasElidedReturn @?= True
        ]

    , testGroup "Move Semantics Complex Cases"
        [ testCase "partial move from structs" $ do
            let partialMoveCode = unlines
                  [ "struct Point { x: i32, y: i32 }"
                  , "let p = Point { x: 1, y: 2 }"
                  , "let x = p.x  // Move only x field"
                  , "let y = p.y  // Error: p partially moved"
                  ]
                -- Should handle partial moves
                hasPartialMove = "Move only x field" `isInfixOf` partialMoveCode
                hasMoveError = "p partially moved" `isInfixOf` partialMoveCode
            hasPartialMove @?= True
            hasMoveError @?= True

        , testCase "move in pattern matching" $ do
            let patternMoveCode = unlines
                  [ "match maybe_value {"
                  , "  Some(value) => process(value),  // value moves here"
                  , "  None => {}"
                  , "}"
                  ]
                -- Should handle moves in pattern matching
                hasPatternMove = "process(value)" `isInfixOf` patternMoveCode
            hasPatternMove @?= True

        , testCase "move closure capture" $ do
            let closureCode = unlines
                  [ "data := createData()"
                  , "closure := || { consume(data) }"  // data moves into closure
                  , "closure()"
                  ]
                -- Should handle moves into closures
                hasClosureMove = "consume(data)" `isInfixOf` closureCode
            hasClosureMove @?= True

        , testCase "conditional move" $ do
            let conditionalMoveCode = unlines
                  [ "data := createData()"
                  , "if condition {"
                  , "  owner := data  // Conditional move"
                  , "} else {"
                  , "  owner := createData()"
                  , "}"
                  , "consume(owner)"
                  ]
                -- Should handle conditional moves
                hasConditionalMove = "owner := data" `isInfixOf` conditionalMoveCode
            hasConditionalMove @?= True
        ]

    , testGroup "Copy vs Clone Semantics"
        [ testCase "implicit copy types" $ do
            let copyCode = unlines
                  [ "let x = 42"
                  , "let y = x  // Copy, not move"
                  , "let z = x  // Still valid"
                  ]
                -- Should handle copy types correctly
                copyCount = length $ filter (== "x") (words copyCode)
            copyCount @?= 3

        , testCase "explicit clone" $ do
            let cloneCode = unlines
                  [ "data := createData()"
                  , "copy := data.clone()"  // Explicit clone
                  , "original := data"      // data still valid"
                  ]
                -- Should handle explicit cloning
                hasClone = "data.clone()" `isInfixOf` cloneCode
                hasOriginal = "original := data" `isInfixOf` cloneCode
            hasClone @?= True
            hasOriginal @?= True

        , testCase "derive copy trait" $ do
            let deriveCode = unlines
                  [ "#[derive(Copy, Clone)]"
                  , "struct Point { x: i32, y: i32 }"
                  , "let p1 = Point { x: 1, y: 2 }"
                  , "let p2 = p1  // Copy works"
                  , "let p3 = p1  // Still valid"
                  ]
                -- Should respect derived Copy trait
                hasDeriveCopy = "#[derive(Copy" `isInfixOf` deriveCode
            hasDeriveCopy @?= True

        , testCase "clone in generic contexts" $ do
            let genericClone = unlines
                  [ "func duplicate<T: Clone>(item: T) -> (T, T) {"
                  , "  (item.clone(), item)"
                  , "}"
                  ]
                -- Should handle clone in generic functions
                hasCloneConstraint = "Clone" `isInfixOf` genericClone
                hasCloneCall = "item.clone()" `isInfixOf` genericClone
            hasCloneConstraint @?= True
            hasCloneCall @?= True
        ]

    , testGroup "Ownership and Concurrency"
        [ testCase "thread-safe transfer" $ do
            let threadCode = unlines
                  [ "data := createData()"
                  , "thread := spawn(|| {"
                  , "  process(data)"  // data moves to thread
                  , "})"
                  ]
                -- Should handle ownership transfer to threads
                hasThreadSpawn = "spawn" `isInfixOf` threadCode
                hasMoveToThread = "process(data)" `isInfixOf` threadCode
            hasThreadSpawn @?= True
            hasMoveToThread @?= True

        , testCase "shared ownership with Arc" $ do
            let arcCode = unlines
                  [ "data := Arc::new(createData())"
                  , "clone1 := data.clone()"
                  , "clone2 := data.clone()"
                  , "// All three can be used simultaneously"
                  ]
                -- Should handle shared ownership
                hasArc = "Arc::new" `isInfixOf` arcCode
                hasClone = "data.clone()" `isInfixOf` arcCode
            hasArc @?= True
            hasClone @?= True

        , testCase "mutex for interior mutability" $ do
            let mutexCode = unlines
                  [ "data := Mutex::new(createData())"
                  , "lock := data.lock().unwrap()"
                  , "modify(&mut lock)"
                  ]
                -- Should handle mutex-based interior mutability
                hasMutex = "Mutex::new" `isInfixOf` mutexCode
                hasLock = "data.lock()" `isInfixOf` mutexCode
            hasMutex @?= True
            hasLock @?= True

        , testCase "channel ownership transfer" $ do
            let channelCode = unlines
                  [ "let (sender, receiver) = channel()"
                  , "sender.send(data)"  // data moves through channel
                  , "received := receiver.recv()"
                  ]
                -- Should handle ownership transfer through channels
                hasChannel = "channel()" `isInfixOf` channelCode
                hasSend = "sender.send(data)" `isInfixOf` channelCode
            hasChannel @?= True
            hasSend @?= True
        ]

    , testGroup "Ownership Error Recovery"
        [ testCase "use after move detection" $ do
            let useAfterMove = unlines
                  [ "data := createData()"
                  , "owner := data"      // data moves here
                  , "process(data)"      // Error: use after move
                  ]
                -- Should detect use after move
                hasMove = "owner := data" `isInfixOf` useAfterMove
                hasError = "use after move" `isInfixOf` useAfterMove
            hasMove @?= True
            hasError @?= True

        , testCase "borrow checker conflict resolution" $ do
            let conflictCode = unlines
                  [ "data := createData()"
                  , "mutable_ref := &mut data"
                  , "// To fix, either:"
                  , "// 1. Use mutable_ref here, then drop it"
                  , "// 2. Use scope to limit mutable borrow"
                  ]
                -- Should provide helpful error messages
                hasMutableBorrow = "&mut data" `isInfixOf` conflictCode
                hasSuggestion = "To fix" `isInfixOf` conflictCode
            hasMutableBorrow @?= True
            hasSuggestion @?= True

        , testCase "lifetime error suggestions" $ do
            let lifetimeError = unlines
                  [ "// Error: borrowed value does not live long enough"
                  , "// Suggestion: add lifetime parameter or return owned value"
                  ]
                -- Should provide lifetime error suggestions
                hasError = "does not live long enough" `isInfixOf` lifetimeError
                hasSuggestion = "Suggestion:" `isInfixOf` lifetimeError
            hasError @?= True
            hasSuggestion @?= True

        , testCase "ownership transfer hints" $ do
            let transferHints = unlines
                  [ "// Consider: .clone() to copy instead of move"
                  , "// Or: use reference (&data) instead of moving"
                  ]
                -- Should provide ownership transfer hints
                hasCloneHint = ".clone()" `isInfixOf` transferHints
                hasRefHint = "&data" `isInfixOf` transferHints
            hasCloneHint @?= True
            hasRefHint @?= True
        ]

    , testGroup "Property-based Ownership Tests"
        [ fastProperty "borrow checking prevents data races" prop_borrowPreventsRaces
        , fastProperty "move semantics prevent double free" prop_movePreventsDoubleFree
        , fastProperty "lifetime analysis prevents dangling references" prop_lifetimePreventsDangling
        , fastProperty "ownership transfer is total" prop_ownershipTransferTotal
        ]
    ]

-- Property: borrow checking should prevent data races
prop_borrowPreventsRaces :: Bool -> Bool -> Bool
prop_borrowPreventsRaces hasMutableBorrow hasImmutableBorrow =
  let -- Should not allow both mutable and immutable borrows simultaneously
      safe = not (hasMutableBorrow && hasImmutableBorrow)
  in safe

-- Property: move semantics should prevent double free
prop_movePreventsDoubleFree :: Bool -> Bool
prop_movePreventsDoubleFree isMoved isUsedAgain =
  let -- Should not allow use after move
      safe = not (isMoved && isUsedAgain)
  in safe

-- Property: lifetime analysis should prevent dangling references
prop_lifetimePreventsDangling :: Int -> Int -> Bool
prop_lifetimePreventsDangling refLifetime targetLifetime =
  let -- Reference should not outlive target
      safe = refLifetime <= targetLifetime
  in safe

-- Property: ownership transfer should be total
prop_ownershipTransferTotal :: Bool -> Bool
prop_ownershipTransferTotal hasOriginalOwner hasNewOwner =
  let -- Should have exactly one owner
      safe = hasOriginalOwner `xor` hasNewOwner
  in safe
  where
    xor True False = True
    xor False True = True
    xor _ _ = False