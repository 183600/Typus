module Compiler.OwnershipChecker (
    checkOwnership,
    checkOwnershipWithValueInfo,
    extractOwnershipContent
) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Ownership (analyzeOwnership, formatOwnershipErrors, OwnershipError(..))
import Compiler.Error
import Compiler.ValueAnalysis (ValueInfo(..), ValueKind(..))
import SourceLocation (Located(..), locatedValue)

import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf)

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

checkOwnership :: TypusFile -> Either CompilationError ()
checkOwnership typusFile =
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
                       valueCopyVars = extractValueCopyVarsLegacy content
                       errors = filter (not . isIgnorableOwnershipError valueCopyVars) errors0
                   in if null errors
                          then Right ()
                          else Left $ mkCompilationError OwnershipErrorKind ("Ownership errors: " ++ formatOwnershipErrors errors) []
           else Right ()

checkOwnershipWithValueInfo :: TypusFile -> [ValueInfo] -> Either CompilationError ()
checkOwnershipWithValueInfo typusFile valueInfos =
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
                       valueCopyVars = [viName info | info <- valueInfos, viKind info == ValueCopy]
                       errors = filter (not . isIgnorableOwnershipError valueCopyVars) errors0
                   in if null errors
                          then Right ()
                          else Left $ mkCompilationError OwnershipErrorKind ("Ownership errors: " ++ formatOwnershipErrors errors) []
           else Right ()

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
    UseAfterMove v -> v `elem` valueCopyVars
    _ -> False

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
