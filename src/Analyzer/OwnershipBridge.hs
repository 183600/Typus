module Analyzer.OwnershipBridge (
    runOwnershipAnalysis,
    processOwnershipErrors
) where

import Analyzer.State
import Analyzer.SymbolTable (isReservedName, trim)
import Analyzer.Types
import qualified Ownership as Own

import Control.Monad.State
import Data.Char (isAlphaNum, isLetter, isSpace, isUpper)
import Data.List (find, findIndex, isInfixOf, isPrefixOf, stripPrefix, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (foldl')

runOwnershipAnalysis :: String -> IntegratedAnalyzer [(ErrorSeverity, Own.OwnershipError)]
runOwnershipAnalysis code = do
    let ownershipErrs = Own.analyzeOwnership code
    processOwnershipErrors code ownershipErrs

processOwnershipErrors :: String -> [Own.OwnershipError] -> IntegratedAnalyzer [(ErrorSeverity, Own.OwnershipError)]
processOwnershipErrors code ownershipErrs = do
    symbols <- gets symbolTable
    let significantErrors = filterSignificantOwnershipErrors code ownershipErrs symbols
        labeledErrors = map (\res -> (Error, res)) significantErrors
    updateSymbolTableWithOwnership significantErrors
    mapM_ (uncurry addOwnershipError) labeledErrors
    pure labeledErrors

updateSymbolTableWithOwnership :: [Own.OwnershipError] -> IntegratedAnalyzer ()
updateSymbolTableWithOwnership ownershipErrs =
    modify $ \s -> s { symbolTable = updateOwnershipSymbols (symbolTable s) ownershipErrs }
  where
    updateOwnershipSymbols :: Map.Map String SymbolInfo -> [Own.OwnershipError] -> Map.Map String SymbolInfo
    updateOwnershipSymbols symbols errors = foldl' updateSymbolForOwnership symbols errors

    updateSymbolForOwnership :: Map.Map String SymbolInfo -> Own.OwnershipError -> Map.Map String SymbolInfo
    updateSymbolForOwnership symbols (Own.UseAfterMove varName) =
        Map.adjust (\sym -> sym { isMoved = True }) varName symbols
    updateSymbolForOwnership symbols (Own.DoubleMove varName _) =
        Map.adjust (\sym -> sym { isMoved = True }) varName symbols
    updateSymbolForOwnership symbols (Own.BorrowWhileMoved varName) =
        Map.adjust (\sym -> sym { isMoved = True, isBorrowed = True }) varName symbols
    updateSymbolForOwnership symbols _ = symbols

filterSignificantOwnershipErrors
    :: String
    -> [Own.OwnershipError]
    -> Map.Map String SymbolInfo
    -> [Own.OwnershipError]
filterSignificantOwnershipErrors source errors symbols =
    let parameterNames = collectFunctionParameters source
        declaredNames = Map.keysSet symbols
        localDeclarations = collectLocalVariables source
        knownNames = declaredNames `Set.union` localDeclarations
        augmentedErrors = ensureUseAfterMove source knownNames errors
    in filter (isSignificant parameterNames knownNames) augmentedErrors
  where
    isSignificant params declared (Own.UseAfterMove varName) =
        not (isReservedName varName)
            && (Set.member varName params || Set.member varName declared)
    isSignificant _ _ (Own.UseWhileMutBorrowed _) = False
    isSignificant params _ (Own.OutOfScope varName)
        | Set.member varName params = False
        | isIgnoredIdentifier varName = False
        | otherwise = True
    isSignificant _ _ _ = True

    isIgnoredIdentifier name =
        null name
            || Set.member name ignoredTokens
            || isLikelyType name
            || not (startsWithIdentChar name)

    ignoredTokens =
        Set.fromList
            [ "owned"
            , "mut"
            , "borrow"
            , "borrowed"
            , "println"
            , "print"
            , "return"
            ]

    isLikelyType (c:_) = isUpper c
    isLikelyType _ = False

    startsWithIdentChar xs =
        case dropWhile (`elem` "&*") xs of
            (c:_) -> isLetter c || c == '_'
            _ -> False

ensureUseAfterMove :: String -> Set.Set String -> [Own.OwnershipError] -> [Own.OwnershipError]
ensureUseAfterMove source declared errs
    | any isUseAfterMoveErr errs = errs
    | otherwise =
        case detectUseAfterMove source declared of
            Just name -> Own.UseAfterMove name : errs
            Nothing -> errs
  where
    isUseAfterMoveErr Own.UseAfterMove{} = True
    isUseAfterMoveErr _ = False

detectUseAfterMove :: String -> Set.Set String -> Maybe String
detectUseAfterMove source declared =
    find (hasUseAfterPattern normalizedSource) (Set.toList declared)
  where
    normalizedSource = map normalizeLine (lines source)

    normalizeLine :: String -> String
    normalizeLine = filter (not . isSpace)

    hasUseAfterPattern :: [String] -> String -> Bool
    hasUseAfterPattern normalized name =
        case findIndex (isPotentialMoveLine name) normalized of
            Nothing -> False
            Just moveIdx ->
                any (isSuspiciousUsage name) (drop (moveIdx + 1) normalized)

    isPotentialMoveLine :: String -> String -> Bool
    isPotentialMoveLine name line =
        not (isNoise line)
            && containsNameArgument name line
            && not (isFunctionDeclaration line)

    isSuspiciousUsage :: String -> String -> Bool
    isSuspiciousUsage name line =
        not (isNoise line)
            && not (isFunctionDeclaration line)
            && ( containsNameArgument name line
               || rhsUsage name line
               || returnsName name line
               )

    containsNameArgument :: String -> String -> Bool
    containsNameArgument name line = ("(" ++ name ++ ")") `isInfixOf` line

    rhsUsage :: String -> String -> Bool
    rhsUsage name line =
        any (isValidRhsMatch line) (findPatternIndices pattern line)
      where
        pattern = "=" ++ name
        patternLength = length pattern
        isValidRhsMatch txt idx =
            let preceding = if idx == 0 then Nothing else Just (txt !! (idx - 1))
                following = drop (idx + patternLength) txt
            in preceding /= Just '=' && not (maybe False isIdentifierChar (listToMaybe following))

    returnsName :: String -> String -> Bool
    returnsName name line =
        case stripPrefix "return" line of
            Just rest -> startsWithNameToken name rest
            Nothing -> False

    startsWithNameToken :: String -> String -> Bool
    startsWithNameToken name str =
        case stripPrefix name str of
            Just [] -> True
            Just (c:_) -> not (isIdentifierChar c)
            Nothing -> False

    isIdentifierChar :: Char -> Bool
    isIdentifierChar c = isAlphaNum c || c == '_'

    isFunctionDeclaration :: String -> Bool
    isFunctionDeclaration line = "func" `isPrefixOf` line

    isNoise :: String -> Bool
    isNoise line = null line || isComment line

    isComment :: String -> Bool
    isComment line = "//" `isPrefixOf` line

    findPatternIndices :: String -> String -> [Int]
    findPatternIndices pattern text =
        [ idx | (idx, suffix) <- zip [0..] (tails text), pattern `isPrefixOf` suffix ]

collectLocalVariables :: String -> Set.Set String
collectLocalVariables source =
    Set.fromList . mapMaybe extractVar $ lines source
  where
    extractVar line =
        let trimmedLine = trim line
        in case words trimmedLine of
            ("var":name:_) -> normalize name
            _ ->
                case break (== ':') trimmedLine of
                    (lhs, ':':'=':_) -> normalize (trim lhs)
                    _ -> Nothing

    normalize raw =
        let cleaned = takeWhile isLocalIdentChar (dropWhile (`elem` "&*") raw)
        in if isValidName cleaned then Just cleaned else Nothing

    isLocalIdentChar c = isAlphaNum c || c == '_'

    isValidName name =
        not (null name)
            && name /= "_"
            && not (isReservedName name)
            && startsWithLocalIdentChar name

    startsWithLocalIdentChar xs =
        case dropWhile (`elem` "&*") xs of
            (c:_) -> isLetter c || c == '_'
            _ -> False

collectFunctionParameters :: String -> Set.Set String
collectFunctionParameters source =
    Set.fromList . concatMap extractParams $ lines source
  where
    extractParams line =
        let trimmedLine = trim line
        in if "func " `isPrefixOf` trimmedLine
               then case dropWhile (/= '(') trimmedLine of
                       '(' : rest ->
                           let (paramChunk, _) = span (/= ')') rest
                           in mapMaybe extractParamName (splitParams paramChunk)
                       _ -> []
               else []

    splitParams "" = []
    splitParams params =
        let (segment, rest) = break (== ',') params
            trimmedSegment = trim segment
        in trimmedSegment : case rest of
                [] -> []
                (_:xs) -> splitParams xs

    extractParamName raw =
        let cleaned = dropWhile isSpace raw
            name = takeWhile isIdentChar (dropWhile (`elem` "&*") cleaned)
        in if null name || name == "_" || name `elem` ["mut","owned"]
              then Nothing
              else Just name

    isIdentChar c = isAlphaNum c || c == '_'
