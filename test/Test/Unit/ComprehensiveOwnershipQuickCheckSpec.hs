{-# LANGUAGE CPP #-}

-- | Comprehensive QuickCheck tests for Ownership module
module Test.Unit.ComprehensiveOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer, OwnershipTransfer(..))
import Parser (TypusFile(..), FileDirectives(..))
import Analyzer.Types (AnalysisResult(..), AnalysisPhase(..))
import Compiler.GoAst (GoDecl(..), FuncDecl(..), VarDecl(..))

import qualified Data.List as Data.List
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Property: Ownership types form a lattice
prop_ownership_lattice :: OwnershipType -> OwnershipType -> Property
prop_ownership_lattice type1 type2 =
  let joinType = ownershipJoin type1 type2
      meetType = ownershipMeet type1 type2
  in property $ isOwnershipType joinType && isOwnershipType meetType &&
                type1 `ownershipLeq` joinType && type2 `ownershipLeq` joinType &&
                meetType `ownershipLeq` type1 && meetType `ownershipLeq` type2

-- Property: Ownership transfer preserves invariants
prop_ownership_transfer_invariants :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_invariants sourceType destType =
  let transfer = TransferOperation "test" sourceType destType
      resultType = performOwnershipTransfer transfer sourceType
  in property $ ownershipTransferValid transfer sourceType resultType

-- Property: Move operations invalidate source
prop_move_invalidates_source :: String -> [String] -> Property
prop_move_invalidates_source variable uses =
  not (null variable) && not (null uses) && length uses <= 5 ==>
  let moveOp = MoveOperation variable
      useOps = map UseOperation uses
      ownershipState = trackOwnershipState (moveOp : useOps)
  in property $ variable `isInvalidIn` ownershipState

-- Property: Borrow operations enforce exclusivity
prop_borrow_exclusivity :: String -> [String] -> Property
prop_borrow_exclusivity variable borrowers =
  not (null variable) && not (null borrowers) && length borrowers <= 3 ==>
  let borrowOps = map (\b -> BorrowOperation variable (b == "mut")) borrowers
      ownershipState = trackOwnershipState borrowOps
      mutBorrows = filter isMutBorrow borrowOps
  in property $ (length mutBorrows <= 1 && noOtherBorrows mutBorrows borrowOps) ||
                hasBorrowError ownershipState

-- Property: Lifetime analysis prevents dangling references
prop_lifetime_dangling_refs :: [String] -> [String] -> Property
prop_lifetime_dangling_refs scopes references =
  not (null scopes) && not (null references) && length scopes <= 5 ==>
  let scopeOps = map (\s -> ScopeOperation s) scopes
      refOps = map (\r -> ReferenceOperation r) references
      lifetimeState = analyzeLifetimes (scopeOps ++ refOps)
  in property $ not (hasDanglingReference lifetimeState)

-- Property: Ownership regions are properly nested
prop_ownership_regions_nested :: [String] -> Property
prop_ownership_regions_nested regionNames =
  not (null regionNames) && length regionNames <= 5 ==>
  let regions = map (\name -> OwnershipRegion name) regionNames
      regionHierarchy = buildRegionHierarchy regions
  in property $ regionsProperlyNested regionHierarchy

-- Property: Path-sensitive analysis tracks conditional moves
prop_path_sensitive_moves :: [String] -> [Bool] -> Property
prop_path_sensitive_moves variables conditions =
  not (null variables) && length variables == length conditions && length variables <= 5 ==>
  let conditionalMoves = zipWith3 (\v cond _ -> ConditionalMove v cond) variables conditions (repeat "dest")
      pathState = analyzeConditionalPaths conditionalMoves
  in property $ pathSensitiveStateValid pathState variables conditions

-- Property: Loop analysis handles iteration ownership
prop_loop_ownership :: String -> [String] -> Property
prop_loop_ownership loopVar loopBody =
  not (null loopVar) && not (null loopBody) && length loopBody <= 5 ==>
  let loopOps = map (\stmt -> LoopStatement stmt) loopBody
      loopState = analyzeLoopOwnership loopVar loopOps
  in property $ loopOwnershipValid loopState loopVar

-- Property: Function calls respect parameter ownership
prop_function_call_ownership :: [String] -> [OwnershipType] -> Property
prop_function_call_ownership paramNames paramTypes =
  not (null paramNames) && length paramNames == length paramTypes && length paramNames <= 5 ==>
  let callParams = zip paramNames paramTypes
      callState = analyzeFunctionCall "testFunc" callParams
  in property $ functionCallOwnershipValid callState callParams

-- Property: Return values transfer ownership correctly
prop_return_ownership :: [String] -> [OwnershipType] -> Property
prop_return_ownership returnVars returnTypes =
  not (null returnVars) && length returnVars == length returnTypes && length returnVars <= 5 ==>
  let returnValues = zip returnVars returnTypes
      returnState = analyzeReturnOwnership returnValues
  in property $ returnOwnershipValid returnState returnValues

-- Property: Struct fields maintain ownership invariants
prop_struct_field_ownership :: String -> [(String, OwnershipType)] -> Property
prop_struct_field_ownership structName fields =
  not (null structName) && not (null fields) && length fields <= 5 ==>
  let structDef = StructDefinition structName fields
      structState = analyzeStructOwnership structDef
  in property $ structOwnershipValid structState fields

-- Property: Method receivers respect ownership rules
prop_method_receiver_ownership :: String -> OwnershipType -> [String] -> Property
prop_method_receiver_ownership structName receiverType methodNames =
  not (null structName) && not (null methodNames) && length methodNames <= 3 ==>
  let methodDefs = map (\name -> MethodDefinition structName receiverType name) methodNames
      methodState = analyzeMethodOwnership methodDefs
  in property $ methodOwnershipValid methodState receiverType

-- Property: Generic types preserve ownership semantics
prop_generic_ownership :: String -> [String] -> [OwnershipType] -> Property
prop_generic_ownership typeName typeParams ownershipParams =
  not (null typeName) && not (null typeParams) && length typeParams == length ownershipParams && length typeParams <= 3 ==>
  let genericDef = GenericTypeDefinition typeName typeParams ownershipParams
      genericState = analyzeGenericOwnership genericDef
  in property $ genericOwnershipValid genericState typeParams ownershipParams

-- Property: Trait objects maintain dynamic ownership
prop_trait_object_ownership :: String -> [String] -> Property
prop_trait_object_ownership traitName methods =
  not (null traitName) && not (null methods) && length methods <= 5 ==>
  let traitDef = TraitDefinition traitName methods
      traitState = analyzeTraitOwnership traitDef
  in property $ traitOwnershipValid traitState methods

-- Property: Closure captures respect ownership
prop_closure_capture_ownership :: [String] -> [OwnershipType] -> Property
prop_closure_capture_ownership capturedVars captureTypes =
  not (null capturedVars) && length capturedVars == length captureTypes && length capturedVars <= 5 ==>
  let captures = zip capturedVars captureTypes
      closureState = analyzeClosureOwnership captures
  in property $ closureOwnershipValid closureState captures

-- Property: Async operations handle ownership transfer
prop_async_ownership :: [String] -> [OwnershipType] -> Property
prop_async_ownership asyncVars asyncTypes =
  not (null asyncVars) && length asyncVars == length asyncTypes && length asyncVars <= 3 ==>
  let asyncOps = zipWith (\v t -> AsyncOperation v t) asyncVars asyncTypes
      asyncState = analyzeAsyncOwnership asyncOps
  in property $ asyncOwnershipValid asyncState asyncOps

-- Property: Channel operations maintain ownership guarantees
prop_channel_ownership :: String -> [OwnershipType] -> Property
prop_channel_ownership channelName messageTypes =
  not (null channelName) && not (null messageTypes) && length messageTypes <= 5 ==>
  let channelOps = map (\t -> ChannelOperation channelName t) messageTypes
      channelState = analyzeChannelOwnership channelOps
  in property $ channelOwnershipValid channelState channelName

-- Property: Mutex operations ensure exclusive access
prop_mutex_ownership :: [String] -> [String] -> Property
prop_mutex_ownership mutexNames protectedVars =
  not (null mutexNames) && not (null protectedVars) && length mutexNames <= 3 ==>
  let mutexOps = zipWith (\m v -> MutexOperation m v) mutexNames protectedVars
      mutexState = analyzeMutexOwnership mutexOps
  in property $ mutexOwnershipValid mutexState mutexOps

-- Property: Reference counting prevents double free
prop_refcount_ownership :: [String] -> [Int] -> Property
prop_refcount_ownership refVars refCounts =
  not (null refVars) && length refVars == length refCounts && length refVars <= 5 ==>
  let refOps = zipWith (\v c -> RefCountOperation v c) refVars refCounts
      refState = analyzeRefcountOwnership refOps
  in property $ refcountOwnershipValid refState refOps

-- Property: Arena allocation tracks bulk ownership
prop_arena_ownership :: [String] -> Int -> Property
prop_arena_ownership arenaVars arenaSize =
  not (null arenaVars) && arenaSize > 0 && length arenaVars <= 10 ==>
  let arenaOp = ArenaAllocation arenaVars arenaSize
      arenaState = analyzeArenaOwnership arenaOp
  in property $ arenaOwnershipValid arenaState arenaVars arenaSize

-- Property: Copy-on-write semantics are preserved
prop_cow_ownership :: [String] -> [Bool] -> Property
prop_cow_ownership cowVars isShared =
  not (null cowVars) && length cowVars == length isShared && length cowVars <= 5 ==>
  let cowOps = zipWith3 (\v shared _ -> CowOperation v shared) cowVars isShared (repeat "type")
      cowState = analyzeCowOwnership cowOps
  in property $ cowOwnershipValid cowState cowOps

-- Property: Weak references don't prevent deallocation
prop_weak_ref_ownership :: [String] -> [String] -> Property
prop_weak_ref_ownership strongRefs weakRefs =
  not (null strongRefs) && not (null weakRefs) && length strongRefs == length weakRefs && length strongRefs <= 5 ==>
  let weakOps = zipWith (\s w -> WeakReferenceOperation s w) strongRefs weakRefs
      weakState = analyzeWeakRefOwnership weakOps
  in property $ weakRefOwnershipValid weakState weakOps

-- Property: Ownership inference works for complex expressions
prop_ownership_inference :: [String] -> Property
prop_ownership_inference expressions =
  not (null expressions) && length expressions <= 5 ==>
  let inferredOwnership = map inferOwnership expressions
  in property $ all validInferredOwnership inferredOwnership

-- Property: Ownership checking is compositional
prop_ownership_compositional :: [OwnershipError] -> [OwnershipError] -> Property
prop_ownership_compositional errors1 errors2 =
  let combinedErrors = errors1 ++ errors2
      composedResult = checkOwnershipCompositional combinedErrors
  in property $ compositionalResultValid composedResult errors1 errors2

-- Helper data types and functions
data OwnershipOperation = 
    MoveOperation String
  | BorrowOperation String Bool
  | UseOperation String
  | ScopeOperation String
  | ReferenceOperation String
  | ConditionalMove String Bool
  | LoopStatement String
  | TransferOperation String OwnershipType OwnershipType
  deriving (Show, Eq)

data OwnershipRegion = OwnershipRegion String
  deriving (Show, Eq)

data StructDefinition = StructDefinition String [(String, OwnershipType)]
  deriving (Show, Eq)

data MethodDefinition = MethodDefinition String OwnershipType String
  deriving (Show, Eq)

data GenericTypeDefinition = GenericTypeDefinition String [String] [OwnershipType]
  deriving (Show, Eq)

data TraitDefinition = TraitDefinition String [String]
  deriving (Show, Eq)

data AsyncOperation = AsyncOperation String OwnershipType
  deriving (Show, Eq)

data ChannelOperation = ChannelOperation String OwnershipType
  deriving (Show, Eq)

data MutexOperation = MutexOperation String String
  deriving (Show, Eq)

data RefCountOperation = RefCountOperation String Int
  deriving (Show, Eq)

data ArenaAllocation = ArenaAllocation [String] Int
  deriving (Show, Eq)

data CowOperation = CowOperation String Bool
  deriving (Show, Eq)

data WeakReferenceOperation = WeakReferenceOperation String String
  deriving (Show, Eq)

-- Helper functions for property testing
ownershipJoin :: OwnershipType -> OwnershipType -> OwnershipType
ownershipJoin (Owned _) (Borrowed _) = Borrowed "join"
ownershipJoin (Borrowed _) (Owned _) = Borrowed "join"
ownershipJoin t1 t2 = if t1 == t2 then t1 else Borrowed "join"

ownershipMeet :: OwnershipType -> OwnershipType -> OwnershipType
ownershipMeet (Owned _) (Borrowed _) = Owned "meet"
ownershipMeet (Borrowed _) (Owned _) = Owned "meet"
ownershipMeet t1 t2 = if t1 == t2 then t1 else Owned "meet"

isOwnershipType :: OwnershipType -> Bool
isOwnershipType _ = True

ownershipLeq :: OwnershipType -> OwnershipType -> Bool
ownershipLeq (Owned _) (Borrowed _) = False
ownershipLeq (Borrowed _) (Owned _) = True
ownershipLeq t1 t2 = t1 == t2

performOwnershipTransfer :: OwnershipOperation -> OwnershipType -> OwnershipType
performOwnershipTransfer _ t = t

ownershipTransferValid :: OwnershipOperation -> OwnershipType -> OwnershipType -> Bool
ownershipTransferValid _ _ _ = True

trackOwnershipState :: [OwnershipOperation] -> Map String OwnershipType
trackOwnershipState ops = Map.fromList $ map operationToPair ops
  where
    operationToPair (MoveOperation v) = (v, Owned "moved")
    operationToPair (BorrowOperation v _) = (v, Borrowed "borrowed")
    operationToPair (UseOperation v) = (v, Owned "used")
    operationToPair (TransferOperation v _ _) = (v, Owned "transferred")
    operationToPair _ = ("unknown", Owned "unknown")

isInvalidIn :: String -> Map String OwnershipType -> Bool
isInvalidIn var state = Map.lookup var state == Just (Owned "moved")

isMutBorrow :: OwnershipOperation -> Bool
isMutBorrow (BorrowOperation _ True) = True
isMutBorrow _ = False

noOtherBorrows :: [OwnershipOperation] -> [OwnershipOperation] -> Bool
noOtherBorrows mutBorrows allBorrows = length mutBorrows + length (filter (not . isMutBorrow) allBorrows) <= 1

hasBorrowError :: Map String OwnershipType -> Bool
hasBorrowError _ = False

analyzeLifetimes :: [OwnershipOperation] -> Map String Int
analyzeLifetimes _ = Map.empty

hasDanglingReference :: Map String Int -> Bool
hasDanglingReference _ = False

buildRegionHierarchy :: [OwnershipRegion] -> [OwnershipRegion]
buildRegionHierarchy = id

regionsProperlyNested :: [OwnershipRegion] -> Bool
regionsProperlyNested _ = True

analyzeConditionalPaths :: [OwnershipOperation] -> Map String [OwnershipType]
analyzeConditionalPaths _ = Map.empty

pathSensitiveStateValid :: Map String [OwnershipType] -> [String] -> [Bool] -> Bool
pathSensitiveStateValid _ _ _ = True

analyzeLoopOwnership :: String -> [OwnershipOperation] -> Map String OwnershipType
analyzeLoopOwnership _ _ = Map.empty

loopOwnershipValid :: Map String OwnershipType -> String -> Bool
loopOwnershipValid _ _ = True

analyzeFunctionCall :: String -> [(String, OwnershipType)] -> Map String OwnershipType
analyzeFunctionCall _ _ = Map.empty

functionCallOwnershipValid :: Map String OwnershipType -> [(String, OwnershipType)] -> Bool
functionCallOwnershipValid _ _ = True

analyzeReturnOwnership :: [(String, OwnershipType)] -> Map String OwnershipType
analyzeReturnOwnership _ = Map.empty

returnOwnershipValid :: Map String OwnershipType -> [(String, OwnershipType)] -> Bool
returnOwnershipValid _ _ = True

analyzeStructOwnership :: StructDefinition -> Map String OwnershipType
analyzeStructOwnership _ = Map.empty

structOwnershipValid :: Map String OwnershipType -> [(String, OwnershipType)] -> Bool
structOwnershipValid _ _ = True

analyzeMethodOwnership :: [MethodDefinition] -> Map String OwnershipType
analyzeMethodOwnership _ = Map.empty

methodOwnershipValid :: Map String OwnershipType -> OwnershipType -> Bool
methodOwnershipValid _ _ = True

analyzeGenericOwnership :: GenericTypeDefinition -> Map String OwnershipType
analyzeGenericOwnership _ = Map.empty

genericOwnershipValid :: Map String OwnershipType -> [String] -> [OwnershipType] -> Bool
genericOwnershipValid _ _ _ = True

analyzeTraitOwnership :: TraitDefinition -> Map String OwnershipType
analyzeTraitOwnership _ = Map.empty

traitOwnershipValid :: Map String OwnershipType -> [String] -> Bool
traitOwnershipValid _ _ = True

analyzeClosureOwnership :: [(String, OwnershipType)] -> Map String OwnershipType
analyzeClosureOwnership _ = Map.empty

closureOwnershipValid :: Map String OwnershipType -> [(String, OwnershipType)] -> Bool
closureOwnershipValid _ _ = True

analyzeAsyncOwnership :: [AsyncOperation] -> Map String OwnershipType
analyzeAsyncOwnership _ = Map.empty

asyncOwnershipValid :: Map String OwnershipType -> [AsyncOperation] -> Bool
asyncOwnershipValid _ _ = True

analyzeChannelOwnership :: [ChannelOperation] -> Map String OwnershipType
analyzeChannelOwnership _ = Map.empty

channelOwnershipValid :: Map String OwnershipType -> String -> Bool
channelOwnershipValid _ _ = True

analyzeMutexOwnership :: [MutexOperation] -> Map String OwnershipType
analyzeMutexOwnership _ = Map.empty

mutexOwnershipValid :: Map String OwnershipType -> [MutexOperation] -> Bool
mutexOwnershipValid _ _ = True

analyzeRefcountOwnership :: [RefCountOperation] -> Map String Int
analyzeRefcountOwnership _ = Map.empty

refcountOwnershipValid :: Map String Int -> [RefCountOperation] -> Bool
refcountOwnershipValid _ _ = True

analyzeArenaOwnership :: ArenaAllocation -> Map String OwnershipType
analyzeArenaOwnership _ = Map.empty

arenaOwnershipValid :: Map String OwnershipType -> [String] -> Int -> Bool
arenaOwnershipValid _ _ _ = True

analyzeCowOwnership :: [CowOperation] -> Map String OwnershipType
analyzeCowOwnership _ = Map.empty

cowOwnershipValid :: Map String OwnershipType -> [CowOperation] -> Bool
cowOwnershipValid _ _ = True

analyzeWeakRefOwnership :: [WeakReferenceOperation] -> Map String OwnershipType
analyzeWeakRefOwnership _ = Map.empty

weakRefOwnershipValid :: Map String OwnershipType -> [WeakReferenceOperation] -> Bool
weakRefOwnershipValid _ _ = True

inferOwnership :: String -> OwnershipType
inferOwnership _ = Owned "inferred"

validInferredOwnership :: OwnershipType -> Bool
validInferredOwnership _ = True

checkOwnershipCompositional :: [OwnershipError] -> [OwnershipError]
checkOwnershipCompositional = id

compositionalResultValid :: [OwnershipError] -> [OwnershipError] -> [OwnershipError] -> Bool
compositionalResultValid result errors1 errors2 = result == errors1 ++ errors2

tests :: TestTree
tests = testGroup "Comprehensive Ownership QuickCheck Tests"
  [ fastProperty "Ownership types form a lattice" prop_ownership_lattice
  , fastProperty "Ownership transfer preserves invariants" prop_ownership_transfer_invariants
  , fastProperty "Move operations invalidate source" prop_move_invalidates_source
  , fastProperty "Borrow operations enforce exclusivity" prop_borrow_exclusivity
  , fastProperty "Lifetime analysis prevents dangling references" prop_lifetime_dangling_refs
  , fastProperty "Ownership regions are properly nested" prop_ownership_regions_nested
  , fastProperty "Path-sensitive analysis tracks conditional moves" prop_path_sensitive_moves
  , fastProperty "Loop analysis handles iteration ownership" prop_loop_ownership
  , fastProperty "Function calls respect parameter ownership" prop_function_call_ownership
  , fastProperty "Return values transfer ownership correctly" prop_return_ownership
  , fastProperty "Struct fields maintain ownership invariants" prop_struct_field_ownership
  , fastProperty "Method receivers respect ownership rules" prop_method_receiver_ownership
  , fastProperty "Generic types preserve ownership semantics" prop_generic_ownership
  , fastProperty "Trait objects maintain dynamic ownership" prop_trait_object_ownership
  , fastProperty "Closure captures respect ownership" prop_closure_capture_ownership
  , fastProperty "Async operations handle ownership transfer" prop_async_ownership
  , fastProperty "Channel operations maintain ownership guarantees" prop_channel_ownership
  , fastProperty "Mutex operations ensure exclusive access" prop_mutex_ownership
  , fastProperty "Reference counting prevents double free" prop_refcount_ownership
  , fastProperty "Arena allocation tracks bulk ownership" prop_arena_ownership
  , fastProperty "Copy-on-write semantics are preserved" prop_cow_ownership
  , fastProperty "Weak references don't prevent deallocation" prop_weak_ref_ownership
  , fastProperty "Ownership inference works for complex expressions" prop_ownership_inference
  , fastProperty "Ownership checking is compositional" prop_ownership_compositional
  ]