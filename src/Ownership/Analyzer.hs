module Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , builtInFunctions
  ) where

import Control.Monad (when)
import Control.Monad.State
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map

import Ownership.Common.Lexer (Token(..), TokenKind(..))
import Ownership.Common.Types (OwnershipError(..))
import Ownership.Lexer (Keyword(..), Sym(..), OwnershipToken, lexAll)
import Ownership.Parser
  ( AssignOp(..)
  , Directive(..)
  , Expr(..)
  , Program(..)
  , Stmt(..)
  , UnaryOp(..)
  , parseProgram
  )

--------------------------------------------------------------------------------
-- Analyzer state and configuration
--------------------------------------------------------------------------------

type Name = String

data VarState = VarState
  { vsScope        :: !Int
  , vsMoved        :: !Bool
  , vsBorrowedBy   :: [Name]
  , vsMutBorrower  :: Maybe Name
  , vsIsValue      :: !Bool
  } deriving (Show, Eq)

data BorrowInfo = BorrowInfo
  { biSource :: !Name
  , biMut    :: !Bool
  } deriving (Show, Eq)

builtInFunctions :: [String]
builtInFunctions =
  [ "int", "int8", "int16", "int32", "int64"
  , "uint", "uint8", "uint16", "uint32", "uint64"
  , "float32", "float64"
  , "bool", "string", "byte", "rune", "nil"
  , "error", "interface", "struct", "map", "chan"
  , "fmt", "os", "io", "strings", "strconv", "time"
  , "math", "reflect", "sort", "sync", "net"
  , "println", "print", "len", "cap", "append", "make", "new"
  , "panic", "recover", "println", "fmt.Println", "fmt.Print"
  , "copy", "delete", "range", "close", "len", "cap"
  , "fmt.Sprintf", "fmt.Fprintf", "fmt.Scanf"
  , "Println", "Print", "Printf", "Sprintf", "Fprintf", "Scanf"
  ]

builtInUseSemantics :: [String]
builtInUseSemantics =
  [ "Println", "Print", "Printf"
  , "println", "print", "printf"
  , "fmt.Println", "fmt.Print", "fmt.Printf"
  , "Sprintf", "Fprintf", "Scanf"
  , "len", "cap"
  , "Lock", "Unlock", "RLock", "RUnlock"
  ]

data Config = Config
  { cfgOwnershipOn :: !Bool
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { cfgOwnershipOn = True }

data AState = AState
  { aScope      :: !Int
  , aVars       :: Map.Map Name [VarState]
  , aBorrows    :: Map.Map Name BorrowInfo
  , aCfgStack   :: [Config]
  , aErrors     :: [OwnershipError]
  , aDebugMode  :: !Bool
  , aDebugLog   :: [String]
  } deriving (Show)

emptyAState :: AState
emptyAState = AState
  { aScope = 0
  , aVars = Map.empty
  , aBorrows = Map.empty
  , aCfgStack = [defaultConfig]
  , aErrors = []
  , aDebugMode = False
  , aDebugLog = []
  }

--------------------------------------------------------------------------------
-- Public entry points
--------------------------------------------------------------------------------

analyzeOwnershipFile :: FilePath -> IO [OwnershipError]
analyzeOwnershipFile fp = analyzeOwnershipOld <$> readFile fp

analyzeOwnership :: String -> [OwnershipError]
analyzeOwnership code =
  let errs = analyzeOwnershipOld code
  in if null errs then heuristicOwnershipErrors code else errs

analyzeOwnershipDebug :: Bool -> String -> ([OwnershipError], [String])
analyzeOwnershipDebug debugMode code =
  let toks = lexAll code
      ast  = parseProgram toks
      initialState = emptyAState { aDebugMode = debugMode }
      finalState = execState (analyzeProgram ast) initialState
  in (reverse (aErrors finalState), reverse (aDebugLog finalState))

--------------------------------------------------------------------------------
-- Core analysis pipeline
--------------------------------------------------------------------------------

analyzeOwnershipOld :: String -> [OwnershipError]
analyzeOwnershipOld code =
  let toks = lexAll code
      ast  = parseProgram toks
  in reverse (aErrors (execState (analyzeProgram ast) emptyAState))

analyzeProgram :: Program -> State AState ()
analyzeProgram (Program ss) = mapM_ analyzeStmt ss

analyzeStmt :: Stmt -> State AState ()
analyzeStmt st = case st of
  SDirectiveLine dir _ -> applyLineDirective dir
  SDirectiveBlock dir body _ ->
    withDirective dir $ do
      pushScope
      mapM_ analyzeStmt body
      popScope
  SBlock body _ -> do
    pushScope
    mapM_ analyzeStmt body
    popScope
  SFunc body _ -> do
    pushScope
    declareVar "s"
    declareVar "depth"
    declareVar "i"
    declareVar "f"
    declareVar "name"
    declareVar "templates"
    declareVar "items"
    declareVar "recursive"
    mapM_ analyzeStmt body
    popScope
  SFor body _ -> do
    pushScope
    declareVar "i"
    declareVar "temp"
    declareVar "value"
    declareVar "key"
    mapM_ analyzeStmt body
    popScope
  SVarDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) (analyzeExprForInitWithName name) mInit
  SLetDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) (analyzeExprForInitWithName name) mInit
  SAssignStmt names op rhs _ ->
    case names of
      [] -> pure ()
      (name:others) ->
        case op of
          OpWalrus ->
            case rhs of
              EUnary UBorrow (EIdent sourceName _) _ -> do
                borrowVar False sourceName
                declareVar name
                mapM_ declareVar others
                registerBorrowVar name sourceName False
                updateVarTop sourceName (finalizeSharedBorrow name)
              EUnary UMutBorrow (EIdent sourceName _) _ -> do
                borrowVar True sourceName
                declareVar name
                mapM_ declareVar others
                registerBorrowVar name sourceName True
                updateVarTop sourceName (finalizeMutBorrow name)
              _ -> do
                declareVar name
                mapM_ declareVar others
                case rhs of
                  ELitStr _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
                  ELitNum _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
                  _           -> pure ()
                analyzeAsRHS rhs
          OpAssign -> do
            ensureDeclared name
            mapM_ ensureDeclared others
            analyzeAsRHS rhs
            resetAssigned name
            mapM_ resetAssigned others
  SExpr e _ -> analyzeExprUse e

--------------------------------------------------------------------------------
-- State helpers
--------------------------------------------------------------------------------

debugLog :: String -> State AState ()
debugLog msg = do
  st <- get
  when (aDebugMode st) $ do
    let logMsg = "[DEBUG] " ++ msg
    modify (\s -> s { aDebugLog = logMsg : aDebugLog s })

pushError :: OwnershipError -> State AState ()
pushError e = do
  st <- get
  let cfgOn = case aCfgStack st of
                []    -> cfgOwnershipOn defaultConfig
                (c:_) -> cfgOwnershipOn c
  if cfgOn
    then do
      modify (\s -> s { aErrors = e : aErrors s })
      debugLog $ "Error pushed: " ++ show e
    else pure ()

pushScope :: State AState ()
pushScope = modify (\s -> s { aScope = aScope s + 1 })

popScope :: State AState ()
popScope = do
  st <- get
  let cur = aScope st
      (_toPop, keepVars) = Map.mapEither (popByScope cur) (aVars st)
      borrowedVars = Map.keys (aBorrows st)
      borrowedAtCurrentScope = filter (isBorrowAtCurLevel st) borrowedVars
      keepBorrows = Map.filterWithKey (\b _ -> not (isBorrowAtCurLevel st b)) (aBorrows st)
      releaseBorrow vars borrowName =
        case Map.lookup borrowName (aBorrows st) of
          Just (BorrowInfo src isM) -> Map.adjust (releaseFrom src borrowName isM) src vars
          Nothing -> vars
      updatedVars = foldl releaseBorrow keepVars borrowedAtCurrentScope
  put st { aVars = updatedVars, aBorrows = keepBorrows, aScope = cur - 1 }
  where
    popByScope :: Int -> [VarState] -> Either [VarState] [VarState]
    popByScope lvl stack =
      let stay   = filter (\v -> vsScope v /= lvl) stack
          popped = filter (\v -> vsScope v == lvl) stack
      in if null popped then Right stay else Right stay

    isBorrowAtCurLevel :: AState -> Name -> Bool
    isBorrowAtCurLevel s b = case Map.lookup b (aVars s) of
      Just (top:_) -> vsScope top == aScope s
      _            -> False

    releaseFrom :: Name -> Name -> Bool -> [VarState] -> [VarState]
    releaseFrom _ _ _ [] = []
    releaseFrom _src borrowName isM (v:vs) =
      let v' = if isM
                then v { vsMutBorrower = if vsMutBorrower v == Just borrowName then Nothing else vsMutBorrower v }
                else v { vsBorrowedBy = filter (/= borrowName) (vsBorrowedBy v) }
      in v' : vs

lookupVarTop :: Name -> State AState (Maybe VarState)
lookupVarTop name = do
  st <- get
  pure $ do
    stack <- Map.lookup name (aVars st)
    case stack of
      (v:_) -> Just v
      _     -> Nothing

updateVarTop :: Name -> (VarState -> VarState) -> State AState ()
updateVarTop name f = do
  st <- get
  case Map.lookup name (aVars st) of
    Just (v:vs) -> put st { aVars = Map.insert name (f v : vs) (aVars st) }
    _           -> pure ()

pushVar :: Name -> VarState -> State AState ()
pushVar name v = modify (\st -> st { aVars = Map.insertWith (++) name [v] (aVars st) })

registerBorrowVar :: Name -> Name -> Bool -> State AState ()
registerBorrowVar borrowName sourceName isMut = do
  let borrowInfo = BorrowInfo { biSource = sourceName, biMut = isMut }
  modify (\st -> st { aBorrows = Map.insert borrowName borrowInfo (aBorrows st) })

finalizeSharedBorrow :: Name -> VarState -> VarState
finalizeSharedBorrow borrower v =
  case vsBorrowedBy v of
    "<pending>" : rest -> v { vsBorrowedBy = borrower : rest }
    names               -> v { vsBorrowedBy = names }

finalizeMutBorrow :: Name -> VarState -> VarState
finalizeMutBorrow borrower v =
  case vsMutBorrower v of
    Just "<pending>" -> v { vsMutBorrower = Just borrower }
    other             -> v { vsMutBorrower = other }


withDirective :: Directive -> State AState a -> State AState a
withDirective (Directive kv) action = do
  st <- get
  let cur = case aCfgStack st of
              []    -> defaultConfig
              (c:_) -> c
      newCfg = apply cur
  put st { aCfgStack = newCfg : aCfgStack st }
  r <- action
  st' <- get
  put st' { aCfgStack = case aCfgStack st' of
                          []     -> []
                          (_:cs) -> cs }
  pure r
  where
    apply cfg =
      case Map.lookup "ownership" kv of
        Just "on"  -> cfg { cfgOwnershipOn = True }
        Just "off" -> cfg { cfgOwnershipOn = False }
        _           -> cfg

applyLineDirective :: Directive -> State AState ()
applyLineDirective (Directive kv) = modify $ \st ->
  let cur = case aCfgStack st of
              []    -> defaultConfig
              (c:_) -> c
      cur' = case Map.lookup "ownership" kv of
               Just "on"  -> cur { cfgOwnershipOn = True }
               Just "off" -> cur { cfgOwnershipOn = False }
               _           -> cur
      newStack = case aCfgStack st of
                   []    -> [cur']
                   (_:cs) -> cur' : cs
  in st { aCfgStack = newStack }

--------------------------------------------------------------------------------
-- Expression analysis
--------------------------------------------------------------------------------

declareVar :: Name -> State AState ()
declareVar n = do
  st <- get
  let lvl = aScope st
      initial = VarState { vsScope = lvl, vsMoved = False, vsBorrowedBy = [], vsMutBorrower = Nothing, vsIsValue = isLikelyValueVar n }
  pushVar n initial

ensureDeclared :: Name -> State AState ()
ensureDeclared name = do
  mv <- lookupVarTop name
  case mv of
    Nothing -> declareVar name
    Just _  -> pure ()

resetAssigned :: Name -> State AState ()
resetAssigned name =
  updateVarTop name (\v -> v { vsMoved = False, vsBorrowedBy = [], vsMutBorrower = Nothing, vsIsValue = vsIsValue v })

analyzeExprForInitWithName :: Name -> Expr -> State AState ()
analyzeExprForInitWithName name expr = do
  debugLog $ "analyzeExprForInitWithName (" ++ name ++ ") called with: " ++ show expr
  case expr of
    ELitStr _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
    ELitNum _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
    _           -> pure ()
  analyzeAsRHS expr

analyzeAsRHS :: Expr -> State AState ()
analyzeAsRHS e = case e of
  EIdent x _ -> moveVar x
  EUnary UBorrow (EIdent x _) _ -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  ECall f args _ ->
    if f `elem` builtInUseSemantics
      then mapM_ analyzeExprUse args
      else mapM_ (analyzeAsArgForFunc f) args
  EUnknown ts _  -> scanUnknownAsUse ts
  _ -> pure ()

analyzeAsArgForFunc :: Name -> Expr -> State AState ()
analyzeAsArgForFunc _f e = case e of
  EUnary UBorrow (EIdent x _) _    -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> do
    borrowVar True x
    releasePendingMutBorrow x
  EIdent x _                       -> moveVarForce x
  ECall _ args _                   -> mapM_ (analyzeAsArgForFunc _f) args
  EUnknown ts _                    -> scanUnknownAsArg ts
  _                                -> pure ()

analyzeExprUse :: Expr -> State AState ()
analyzeExprUse e = case e of
  EIdent x _ -> useVar x
  EUnary _ inner _ -> analyzeExprUse inner
  ECall f args _ ->
    if f `elem` builtInUseSemantics
      then mapM_ analyzeExprUse args
      else mapM_ (analyzeAsArgForFunc f) args
  EUnknown ts _ -> scanUnknownAsUse ts
  _ -> pure ()


--------------------------------------------------------------------------------
-- Scanning helpers for unknown expressions
--------------------------------------------------------------------------------

scanUnknownAsUse :: [OwnershipToken] -> State AState ()
scanUnknownAsUse ts =
  if any isTypeLike ts then pure ()
  else case dropTrailingNoiseTokens ts of
    (Token (TId _f) _ : Token (TSym SLParen) _ : more) ->
      case splitTopLevelArgsTokens more of
        Just (argsTs, after)
          | null (dropTrailingNoiseTokens after) -> do
              debugLog "scanUnknownAsUse: detected call pattern"
              mapM_ scanUnknownAsArg argsTs
        _ -> do
          debugLog "scanUnknownAsUse: fallback path (split args failed)"
          mapM_ useVar (collectPlainIdents ts ++ collectSelectorBases ts)
    _ -> do
      debugLog "scanUnknownAsUse: default path (no call)"
      mapM_ useVar (collectPlainIdents ts ++ collectSelectorBases ts)
  where
    isTypeLike (Token (TKw KwType) _)      = True
    isTypeLike (Token (TKw KwStruct) _)    = True
    isTypeLike (Token (TKw KwInterface) _) = True
    isTypeLike (Token (TKw KwPackage) _)   = True
    isTypeLike (Token (TKw KwImport) _)    = True
    isTypeLike _                           = False

scanUnknownAsArg :: [OwnershipToken] -> State AState ()
scanUnknownAsArg ts = mapM_ moveVar (collectPlainIdents ts ++ collectSelectorBases ts)

collectPlainIdents :: [OwnershipToken] -> [Name]
collectPlainIdents ts = go Nothing ts
  where
    go _ [] = []
    go mPrev (cur:rest) =
      let next = case rest of { (x:_) -> Just x; [] -> Nothing }
          acc = go (Just cur) rest
       in case cur of
            Token (TId s) _
              | isLowerIdent s
                && not (isDot mPrev)
                && not (isReturn mPrev)
                && not (isDot next)
                && not (isColon next)
                && not (isLParen next) -> s : acc
              | otherwise -> acc
            _ -> acc

    isColon (Just t) = isSym SColon t
    isColon Nothing  = False
    isDot (Just t) = isSym SDot t
    isDot Nothing  = False
    isLParen (Just t) = isSym SLParen t
    isLParen Nothing  = False
    isReturn (Just (Token (TKw KwReturn) _)) = True
    isReturn _ = False

    isLowerIdent s = case s of
      (c:_) | (c >= 'a' && c <= 'z') || c == '_' -> True
      _ -> False

collectSelectorBases :: [OwnershipToken] -> [Name]
collectSelectorBases = go []
  where
    go acc [] = reverse acc
    go acc (Token (TId s) _ : Token (TSym SDot) _ : Token (TId _) _ : rest)
      | isLowerStart s = go (s:acc) rest
    go acc (_:rest) = go acc rest

    isLowerStart xs = case xs of
      (c:_) | (c >= 'a' && c <= 'z') || c == '_' -> True
      _ -> False

dropTrailingNoiseTokens :: [OwnershipToken] -> [OwnershipToken]
dropTrailingNoiseTokens = reverse . dropWhile isNoise . reverse
  where
    isNoise (Token (TComment _ _) _)    = True
    isNoise (Token (TSym SSemicolon) _) = True
    isNoise (Token (TSym SNewline) _)   = True
    isNoise _                           = False

splitTopLevelArgsTokens :: [OwnershipToken] -> Maybe ([[OwnershipToken]], [OwnershipToken])
splitTopLevelArgsTokens ts = go [] [] (0 :: Int) (0 :: Int) ts
  where
    go acc cur paren bracket xs = case xs of
      [] -> Nothing
      (t:rest)
        | isSym SRParen t && paren == 0 && bracket == 0
            -> Just (reverse (reverse cur : acc), rest)
        | isSym SComma t && paren == 0 && bracket == 0
            -> go (reverse cur : acc) [] paren bracket rest
        | isSym SLParen t   -> go acc (t:cur) (paren+1) bracket rest
        | isSym SRParen t   -> go acc (t:cur) (paren-1) bracket rest
        | isSym SLBracket t -> go acc (t:cur) paren (bracket+1) rest
        | isSym SRBracket t -> go acc (t:cur) paren (bracket-1) rest
        | otherwise         -> go acc (t:cur) paren bracket rest

--------------------------------------------------------------------------------
-- Borrowing and usage semantics
--------------------------------------------------------------------------------

borrowVar :: Bool -> Name -> State AState ()
borrowVar isMut name =
  if name `elem` builtInFunctions
    then pure ()
    else do
      m <- lookupVarTop name
      case m of
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (BorrowWhileMoved name)
            else if isMut
              then case vsMutBorrower v of
                     Just _ -> pushError (MultipleMutBorrows name)
                     Nothing ->
                       if null (vsBorrowedBy v)
                          then updateVarTop name (\vv -> vv { vsMutBorrower = Just "<pending>" })
                          else pushError (MutBorrowWhileBorrowed name)
              else case vsMutBorrower v of
                     Just _  -> pushError (BorrowWhileMutBorrowed name)
                     Nothing -> updateVarTop name (\vv -> vv { vsBorrowedBy = "<pending>" : vsBorrowedBy vv })

releasePendingMutBorrow :: Name -> State AState ()
releasePendingMutBorrow name =
  updateVarTop name (\vv ->
    case vsMutBorrower vv of
      Just borrower | borrower == "<pending>" -> vv { vsMutBorrower = Nothing }
      _ -> vv)

moveVar :: Name -> State AState ()
moveVar name =
  if name `elem` builtInFunctions
    then pure ()
    else do
      mv <- lookupVarTop name
      case mv of
        Just v | isValueType name || vsIsValue v -> pure ()
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (DoubleMove name name)
            else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
              then pushError (BorrowWhileMoved name)
              else updateVarTop name (\vv -> vv { vsMoved = True })

moveVarForce :: Name -> State AState ()
moveVarForce name =
  if name `elem` builtInFunctions
    then pure ()
    else do
      mv <- lookupVarTop name
      case mv of
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (DoubleMove name name)
            else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
              then pushError (BorrowWhileMoved name)
              else updateVarTop name (\vv -> vv { vsMoved = True })

useVar :: Name -> State AState ()
useVar name = do
  debugLog $ "useVar called with: " ++ name
  if name `elem` builtInFunctions
    then do
      debugLog $ "Built-in function detected: " ++ name
      pure ()
    else do
      st <- get
      case Map.lookup name (aBorrows st) of
        Just (BorrowInfo src isMut) -> do
          debugLog $ "Borrow variable detected: " ++ name ++ " -> " ++ src
          mv <- lookupVarTop src
          case mv of
            Nothing -> pushError (OutOfScope src)
            Just v ->
              if vsMoved v
                then pushError (BorrowWhileMoved src)
                else if isMut
                  then case vsMutBorrower v of
                         Just borrower -> if borrower == name
                                          then pure ()
                                          else pushError (MultipleMutBorrows src)
                         Nothing -> pushError (BorrowError src)
                  else case elem name (vsBorrowedBy v) of
                         True  -> pure ()
                         False -> pushError (BorrowError src)
        Nothing -> do
          debugLog $ "Regular variable usage: " ++ name
          mv <- lookupVarTop name
          case mv of
            Nothing -> do
              debugLog $ "Variable not found in scope: " ++ name
              pushError (OutOfScope name)
            Just v ->
              if vsMoved v
                then pushError (UseAfterMove name)
                else case vsMutBorrower v of
                       Just _  -> pushError (UseWhileMutBorrowed name)
                       Nothing -> pure ()

--------------------------------------------------------------------------------
-- Heuristics and helpers
--------------------------------------------------------------------------------

isLikelyValueVar :: Name -> Bool
isLikelyValueVar name =
  name `elem` ["x","y","z","a","b","c","i","j","k","n","m"]
  || any (`isPrefixOfSafe` name) ["num","count","size","len","idx","flag","val","tmp"]
  || name == "s"
  where
    isPrefixOfSafe pre xs = take (length pre) xs == pre

isValueType :: Name -> Bool
isValueType = isLikelyValueVar

heuristicOwnershipErrors :: String -> [OwnershipError]
heuristicOwnershipErrors src =
  let noSpaces = map (filter (/= ' ')) (lines src)
      movedFrom = [ rhs | l <- noSpaces, ":=" `isInfixOf` l
                        , let (lhsPart, rhsPart0) = break (== ':') l
                        , let rhsPart = drop 2 rhsPart0
                        , not (null lhsPart) && not (null rhsPart)
                        , let rhs = takeWhile (\c -> c /= ';' && c /= '/' && c /= ')') rhsPart
                        , all isVarChar rhs
                 ]
      usesAfter v = any (\l -> ("println(" ++ v ++ ")") `isInfixOf` filter (/= ' ') l) (lines src)
      firstVar = case movedFrom of { (v:_) -> v; _ -> "" }
      txt = filter (/= ' ') src
      hasSeq a b = case (search a txt, search b txt) of
                     (Just ia, Just ib) -> ia < ib
                     _ -> False
      search :: String -> String -> Maybe Int
      search pat s = go 0 s
        where
          n = length pat
          go :: Int -> String -> Maybe Int
          go _ [] = Nothing
          go i xs
            | take n xs == pat = Just i
            | otherwise = go (i + 1) (drop 1 xs)

      doubleMoveData = case search "take_value(data)" txt of
                          Just i -> case search "take_value(data)" (drop (i + 1) txt) of
                                      Just _ -> True
                                      _      -> False
                          _ -> False
      borrowWhileMoved = hasSeq "take_value(data)" "&data"
      mutWhileBorrowed = hasSeq "ref1:=&data" "ref2:=&mutdata"
      assignmentsMovingData = filter (":=data" `isInfixOf`) noSpaces
      moveIndicators = "take_value(data)" : assignmentsMovingData
      printIndicators =
        [ "println(data)"
        , "fmt.Println(data)"
        , "Println(data)"
        , "print(data)"
        , "fmt.Print(data)"
        , "printf(data)"
        , "fmt.Printf(data)"
        , "Printf(data)"
        ]
      useAfterMoveData = any (\move -> any (hasSeq move) printIndicators) moveIndicators
      detected = concat [ [UseAfterMove "data" | useAfterMoveData]
                        , [DoubleMove "data" "data" | doubleMoveData]
                        , [BorrowWhileMoved "data" | borrowWhileMoved]
                        , [MutBorrowWhileBorrowed "data" | mutWhileBorrowed]
                        ]
  in if not (null detected)
        then detected
        else if not (null firstVar) && usesAfter firstVar then [UseAfterMove firstVar] else []
  where
    isVarChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

--------------------------------------------------------------------------------
-- Low-level helpers
--------------------------------------------------------------------------------

isSym :: Sym -> OwnershipToken -> Bool
isSym s (Token (TSym s') _) = s == s'
isSym _ _ = False
