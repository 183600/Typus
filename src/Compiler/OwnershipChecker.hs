module Compiler.OwnershipChecker (
    checkOwnership,
    checkOwnershipWithValueInfo,
    extractOwnershipContent
) where

import qualified Data.Text as T

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Ownership (analyzeOwnership, OwnershipError(..))
import Compiler.ValueAnalysis (ValueInfo(..), ValueKind(..))
import Compiler.Errors
    ( CompilerError
    , CompilerResult
    , CompilationPhase(..)
    , ErrorCategory(..)
    , ErrorSeverity(..)
    , mkCompilerError
    )
import SourceLocation (Located(..), locatedValue)

import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf)

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

checkOwnership :: TypusFile -> CompilerResult ()
checkOwnership typusFile = runOwnershipAnalysis typusFile extractValueCopyVarsLegacy

checkOwnershipWithValueInfo :: TypusFile -> [ValueInfo] -> CompilerResult ()
checkOwnershipWithValueInfo typusFile valueInfos =
    let semanticValueCopies = [viName info | info <- valueInfos, viKind info == ValueCopy]
        determineValueCopies content =
            semanticValueCopies ++
                [ legacyVar
                | legacyVar <- extractValueCopyVarsLegacy content
                , legacyVar `notElem` semanticValueCopies
                ]
    in runOwnershipAnalysis typusFile determineValueCopies

runOwnershipAnalysis :: TypusFile -> (String -> [String]) -> CompilerResult ()
runOwnershipAnalysis typusFile determineValueCopies =
    let directives = tfDirectives typusFile
        blocks = tfBlocks typusFile
        fileEnabled = directiveEnabled (fdOwnership directives)
        blockEnabled = any (directiveEnabled . bdOwnership . cbDirectives) blocks
        shouldCheck = fileEnabled || blockEnabled
        fullContent = intercalate "\n" $ map cbContent blocks
        contentToCheck = if fileEnabled then fullContent else extractOwnershipContent typusFile
    in if shouldCheck
           then case contentToCheck of
               "" -> Right ()
               content ->
                   let errors0 = analyzeOwnership content
                       valueCopyVars = determineValueCopies content
                       filtered = filter (not . isIgnorableOwnershipError valueCopyVars) errors0
                   in if null filtered
                          then Right ()
                          else Left (map ownershipErrorToCompilerError filtered)
           else Right ()

ownershipErrorToCompilerError :: OwnershipError -> CompilerError
ownershipErrorToCompilerError err = case err of
    UseAfterMove var -> mk "OWN0001" ("Value '" ++ var ++ "' used after move")
        [ "Use a reference (&value) if you need to reuse it"
        , "Consider cloning the value before moving"
        ]
    DoubleMove var ctx -> mk "OWN0002"
        ("Value '" ++ var ++ "' moved multiple times" ++ contextSuffix ctx)
        [ "Track move sites and convert one of them to a borrow"
        , "Clone the value if multiple owners are required"
        ]
    BorrowWhileMoved var -> mk "OWN0003"
        ("Value '" ++ var ++ "' borrowed after being moved")
        [ "Borrow the value before moving it"
        , "Ensure the original binding remains valid while borrowed"
        ]
    MutBorrowWhileBorrowed var -> mk "OWN0004"
        ("Mutable borrow issued while '" ++ var ++ "' is immutably borrowed")
        [ "Release immutable borrows before taking a mutable borrow"
        , "Limit the lifetime of earlier borrows"
        ]
    BorrowWhileMutBorrowed var -> mk "OWN0005"
        ("Immutable borrow issued while '" ++ var ++ "' has an active mutable borrow")
        [ "End the mutable borrow before sharing the value"
        , "Restrict mutable borrow scopes"
        ]
    MultipleMutBorrows var -> mk "OWN0006"
        ("Multiple mutable borrows detected for '" ++ var ++ "'")
        [ "Allow only one mutable borrow at a time"
        , "Convert extra borrows to shared references"
        ]
    UseWhileMutBorrowed var -> mk "OWN0007"
        ("Value '" ++ var ++ "' used while a mutable borrow is active")
        [ "Wait for the mutable borrow to end before using the value"
        , "Shorten the mutable borrow scope"
        ]
    OutOfScope var -> mk "OWN0008"
        ("Value '" ++ var ++ "' moved out of scope")
        [ "Clone the value when leaving the scope"
        , "Return the value instead of moving it into inner scope"
        ]
    BorrowError msg -> mk "OWN0009"
        ("Borrow error: " ++ msg)
        [ "Verify borrow lifetimes"
        , "Ensure references do not outlive their owners"
        ]
    ParseError msg -> mk "OWN0010"
        ("Ownership parser error: " ++ msg)
        [ "Validate the ownership directives and syntax"
        , "Reduce complex expressions and re-run the analysis"
        ]
    CrossFunctionMove var fn -> mk "OWN0011"
        ("Value '" ++ var ++ "' moved across function boundary '" ++ fn ++ "'")
        [ "Return the value explicitly instead of moving implicitly"
        , "Borrow the value when passing to functions"
        ]
    ParameterMoveMismatch param -> mk "OWN0012"
        ("Parameter move mismatch for '" ++ param ++ "'")
        [ "Ensure parameter usage matches ownership expectations"
        , "Consider borrowing parameters that should not transfer ownership"
        ]
    ControlFlowError msg -> mk "OWN0013"
        ("Control-flow specific ownership error: " ++ msg)
        [ "Review ownership along all control-flow branches"
        , "Ensure moves and borrows are balanced in loops and conditionals"
        ]
    PathSensitiveError msg -> mk "OWN0014"
        ("Path-sensitive ownership error: " ++ msg)
        [ "Align ownership behaviour across different execution paths"
        , "Normalise move/borrow operations before branching"
        ]
    LoopOwnershipError msg -> mk "OWN0015"
        ("Loop ownership error: " ++ msg)
        [ "Avoid moving loop variables inside the loop body"
        , "Borrow values when they need to be reused on each iteration"
        ]
  where
    mk errId message suggestions =
        mkCompilerError
            errId
            (T.pack message)
            OwnershipAnalysisPhase
            Ownership
            Error
            Nothing
            Nothing
            (map T.pack suggestions)
            []
            Nothing

    contextSuffix s
        | null s    = ""
        | otherwise = " (" ++ s ++ ")"

extractOwnershipContent :: TypusFile -> String
extractOwnershipContent typusFile =
    let ownershipBlocks = filter (directiveEnabled . bdOwnership . cbDirectives) (tfBlocks typusFile)
    in concatMap cbContent ownershipBlocks

extractValueCopyVarsLegacy :: String -> [String]
extractValueCopyVarsLegacy src =
    let ls = lines src
        isValueInit t = any (`isInfixOf` t) ["\"", " true", " false", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        pickName t = trim $ takeWhile (/= ':') t
    in [ pickName (trim l)
       | l <- ls
       , let t = trim l
       , ":=" `isInfixOf` t
       , isValueInit t
       , not ("&" `isInfixOf` t)
       ]

isIgnorableOwnershipError :: [String] -> OwnershipError -> Bool
isIgnorableOwnershipError valueCopyVars err = case err of
    UseAfterMove v   -> v `elem` valueCopyVars
    DoubleMove v _   -> v `elem` valueCopyVars
    BorrowWhileMoved v -> v `elem` valueCopyVars
    OutOfScope v     -> v `elem` ownershipKeywords
    _ -> False
  where
    ownershipKeywords = ["owned", "mut", "borrow", "borrowed"]

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
