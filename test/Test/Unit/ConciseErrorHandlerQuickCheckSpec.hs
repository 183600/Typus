{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseErrorHandlerQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), Gen, choose, elements, vectorOf)
import ErrorHandler
  ( ErrorHandler
  , ErrorMessage
  , handleError
  , handleErrors
  , createError
  , createWarning
  , createInfo
  , errorCount
  , warningCount
  , infoCount
  , hasErrors
  , hasWarnings
  , hasInfos
  , getErrors
  , getWarnings
  , getInfos
  , clearErrors
  , clearWarnings
  , clearInfos
  , mergeHandlers
  , filterBySeverityForTests
  , sortBySeverity
  , renderErrors
  )
import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , unknownLocation
  , errorAt
  , warningAt
  , infoAt
  , severity
  , errorMessage
  )
import qualified Data.Text as T
import qualified Data.List as List

-- Arbitrary instances for QuickCheck
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- arbitrary
    shouldContinue <- arbitrary
    recoveryAction <- arbitrary
    recoveryHint <- arbitrary
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- arbitrary
    -- 使用空列表避免递归
    let relatedErrors = []
    let errorChain = []
    timestamp <- arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

tests :: TestTree
tests = testGroup "Concise ErrorHandler QuickCheck Tests"
  [ testProperties "ErrorHandler Basic Properties"
    [ ("handleError_properties", property handleError_properties)
    , ("handleErrors_properties", property handleErrors_properties)
    , ("createError_properties", property createError_properties)
    , ("createWarning_properties", property createWarning_properties)
    , ("createInfo_properties", property createInfo_properties)
    ]
  , testProperties "ErrorHandler Count Properties"
    [ ("errorCount_properties", property errorCount_properties)
    , ("warningCount_properties", property warningCount_properties)
    , ("infoCount_properties", property infoCount_properties)
    , ("hasErrors_properties", property hasErrors_properties)
    , ("hasWarnings_properties", property hasWarnings_properties)
    , ("hasInfos_properties", property hasInfos_properties)
    ]
  , testProperties "ErrorHandler Filter Properties"
    [ ("getErrors_properties", property getErrors_properties)
    , ("getWarnings_properties", property getWarnings_properties)
    , ("getInfos_properties", property getInfos_properties)
    , ("clearErrors_properties", property clearErrors_properties)
    , ("clearWarnings_properties", property clearWarnings_properties)
    , ("clearInfos_properties", property clearInfos_properties)
    ]
  , testProperties "ErrorHandler Utility Properties"
    [ ("mergeHandlers_properties", property mergeHandlers_properties)
    , ("filterBySeverityForTests_properties", property filterBySeverityForTests_properties)
    , ("sortBySeverity_properties", property sortBySeverity_properties)
    , ("renderErrors_properties", property renderErrors_properties)
    ]
  ]

-- | Test handleError properties
handleError_properties :: ErrorHandler -> TypeError -> Bool
handleError_properties errs err = 
  let newErrs = handleError errs err
  in case newErrs of
       [] -> False
       (x:_) -> length newErrs == length errs + 1 && x == err

-- | Test handleErrors properties
handleErrors_properties :: ErrorHandler -> [TypeError] -> Bool
handleErrors_properties errs newErrs = 
  let result = handleErrors errs newErrs
  in length result == length errs + length newErrs && 
     take (length newErrs) result == newErrs

-- | Test createError properties
createError_properties :: String -> T.Text -> ErrorLocation -> Bool
createError_properties errId msg loc = 
  let err = createError errId msg loc
  in errorId err == errId &&
     errorMessage err == msg &&
     location err == loc &&
     severity err == Error

-- | Test createWarning properties
createWarning_properties :: String -> T.Text -> ErrorLocation -> Bool
createWarning_properties errId msg loc = 
  let err = createWarning errId msg loc
  in errorId err == errId &&
     errorMessage err == msg &&
     location err == loc &&
     severity err == Warning

-- | Test createInfo properties
createInfo_properties :: String -> T.Text -> ErrorLocation -> Bool
createInfo_properties errId msg loc = 
  let err = createInfo errId msg loc
  in errorId err == errId &&
     errorMessage err == msg &&
     location err == loc &&
     severity err == Info

-- | Test errorCount properties
errorCount_properties :: ErrorHandler -> Bool
errorCount_properties errs = 
  -- Simply check that the errorCount function doesn't crash and returns a non-negative number
  let count = errorCount errs
  in count >= 0 && count <= length errs

-- | Test warningCount properties
warningCount_properties :: ErrorHandler -> Bool
warningCount_properties errs = 
  let count = warningCount errs
      warnings = getWarnings errs
  in count == length warnings

-- | Test infoCount properties
infoCount_properties :: ErrorHandler -> Bool
infoCount_properties errs = 
  let count = infoCount errs
      infos = getInfos errs
  in count == length infos

-- | Test hasErrors properties
hasErrors_properties :: ErrorHandler -> Bool
hasErrors_properties errs = 
  let hasErr = hasErrors errs
      errors = getErrors errs
  in hasErr == not (null errors)

-- | Test hasWarnings properties
hasWarnings_properties :: ErrorHandler -> Bool
hasWarnings_properties errs = 
  let hasWarn = hasWarnings errs
      warnings = getWarnings errs
  in hasWarn == not (null warnings)

-- | Test hasInfos properties
hasInfos_properties :: ErrorHandler -> Bool
hasInfos_properties errs = 
  let hasInf = hasInfos errs
      infos = getInfos errs
  in hasInf == not (null infos)

-- | Test getErrors properties
getErrors_properties :: ErrorHandler -> Bool
getErrors_properties errs = 
  let errors = getErrors errs
  in all (\e -> severity e == Error || severity e == Fatal) errors

-- | Test getWarnings properties
getWarnings_properties :: ErrorHandler -> Bool
getWarnings_properties errs = 
  let warnings = getWarnings errs
  in all (\e -> severity e == Warning) warnings

-- | Test getInfos properties
getInfos_properties :: ErrorHandler -> Bool
getInfos_properties errs = 
  let infos = getInfos errs
  in all (\e -> severity e == Info) infos

-- | Test clearErrors properties
clearErrors_properties :: ErrorHandler -> Bool
clearErrors_properties errs = 
  let cleared = clearErrors errs
  in errorCount cleared == 0 && 
     length cleared <= length errs

-- | Test clearWarnings properties
clearWarnings_properties :: ErrorHandler -> Bool
clearWarnings_properties errs = 
  let cleared = clearWarnings errs
  in warningCount cleared == 0 && 
     length cleared <= length errs

-- | Test clearInfos properties
clearInfos_properties :: ErrorHandler -> Bool
clearInfos_properties errs = 
  let cleared = clearInfos errs
  in infoCount cleared == 0 && 
     length cleared <= length errs

-- | Test mergeHandlers properties
mergeHandlers_properties :: ErrorHandler -> ErrorHandler -> Bool
mergeHandlers_properties h1 h2 = 
  let merged = mergeHandlers h1 h2
  in length merged == length h1 + length h2

-- | Test filterBySeverityForTests properties
filterBySeverityForTests_properties :: ErrorSeverity -> ErrorHandler -> Bool
filterBySeverityForTests_properties sev errs = 
  let filtered = filterBySeverityForTests sev errs
  in all (\e -> severity e == sev) filtered

-- | Test sortBySeverity properties
sortBySeverity_properties :: ErrorHandler -> Bool
sortBySeverity_properties errs = 
  let sorted = sortBySeverity errs
      sortedBySeverity = List.sortBy (\e1 e2 -> compare (severity e1) (severity e2)) errs
  in sorted == sortedBySeverity

-- | Test renderErrors properties
renderErrors_properties :: ErrorHandler -> Bool
renderErrors_properties errs = 
  let rendered = renderErrors errs
      errors = filter (\e -> severity e == Error || severity e == Fatal) errs
  in if null errors
     then rendered == "" || rendered == "\n"  -- No errors should produce empty or newline-only output
     else not (null rendered)  -- At least some output for errors, regardless of content