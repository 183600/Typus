module Test.Unit.NewErrorHandlerEnhancedQuickCheckSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck 
      \sev1 sev2 -> compareSeverity sev1                               sev2 === compare (severityPriority sev1) (severityPriority sev2)

  , QC.testProperty "isAtLeast reflexivity" $
      \sev -> isAtLeast sev sev

  , QC.testProperty "isAtLeast transitivity" $
      \sev1 sev2 sev3 -> isAtLeast sev1 sev2 && isAtLeast sev2                               sev3 ==> isAtLeast sev1 sev3

  , QC.testProperty "isAtLeast L.minimum" $
      \sev -> isAtLeast Info sev

  , QC.testProperty "isAtLeast L.maximum" $
      \sev -> isAtLeast sev Fatal

  , QC.testProperty "detailedSeverityPriority includes base priority" $
      \sev sub -> let detailed = DetailedSeverity sev sub Nothing
                  in detailedSeverityPriority detailed >= severityPriority sev

  , QC.testProperty "custom detailed severity preserves base severity" $
      \sev sub custom -> let detailed = _customDetailedSeverity sev sub custom
                        in baseSeverity                               detailed === sev

  , QC.testProperty "_isRecoverable: Fatal is not recoverable" $
      not (_isRecoverable Fatal)

  , QC.testProperty "_isRecoverable: non-Fatal are recoverable" $
      \sev -> sev /=                               Fatal ==> _isRecoverable sev

  , QC.testProperty "_isUserActionRequired: Fatal L.and Error require action" $
      \sev -> sev `elem` [Fatal, Error] ==> _isUserActionRequired sev
  ]
  where
      _customDetailedSeverity base sub                               customName = DetailedSeverity base sub (Just customName)

-- | Error location properties
errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ QC.testProperty "_atLocation creates location with correct line L.and column" $
      \line col -> let loc = _atLocation line col
                   in getErrorLine                               loc === line && getErrorColumn                               loc === col

  , QC.testProperty "_atFileLocation creates location with file path" $
      \file line col -> let loc = _atFileLocation file line col
                       in filePath                               loc === Just file &&
                          getErrorLine                               loc === line &&
                          getErrorColumn                               loc === col

  , QC.testProperty "_atRange creates location with range" $
      \startLine startCol endLine endCol -> 
        let loc = _atRange startLine startCol endLine endCol
        in line                               loc === startLine &&
           column                               loc === startCol &&
           endLine                               loc === Just endLine &&
           endColumn                               loc === Just endCol

  , QC.testProperty "_unknownLocation has unknown values" $
      \loc -> let unknown = _unknownLocation
               in filePath                               unknown === Nothing &&
                  getErrorLine                               unknown === 0 &&
                  getErrorColumn                               unknown === 0 &&
                  endLine                               unknown === Nothing &&
                  endColumn                               unknown === Nothing
  ]

-- | Error context properties
errorContextProperties :: TestTree
errorContextProperties = testGroup "Error Context Properties"
  [ QC.testProperty "emptyContext has L.all Nothing values" $
      \ctx -> contextCode                               emptyContext === Nothing &&
              contextFunction                               emptyContext === Nothing &&
              contextVariable                               emptyContext === Nothing &&
              contextType                               emptyContext === Nothing &&
              L.null (contextAdditional emptyContext)

  , QC.testProperty "contextAdditional preserves order" $
      \pairs -> let ctx = emptyContext {                               contextAdditional = pairs }
                 in contextAdditional                               ctx === pairs

  , QC.testProperty "context fields are independent" $
      \code func var typ -> 
        let ctx = ErrorContext code func var typ []
        in contextCode                               ctx === code &&
           contextFunction                               ctx === func &&
           contextVariable                               ctx === var &&
           contextType                               ctx === typ
  ]

-- | Error recovery properties
errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ QC.testProperty "fatalRecovery cannot recover" $
      not (canRecover fatalRecovery) && not (shouldContinue fatalRecovery)

  , QC.testProperty "errorRecovery can recover L.and continue" $
      canRecover errorRecovery && shouldContinue errorRecovery

  , QC.testProperty "warningRecovery can recover L.and continue" $
      canRecover warningRecovery && shouldContinue warningRecovery

  , QC.testProperty "infoRecovery can recover L.and continue" $
      canRecover infoRecovery && shouldContinue infoRecovery

  , QC.testProperty "customRecovery preserves L.all fields" $
      \canRec shouldCont action hint cost conf ->
        let recovery = customRecovery canRec shouldCont action hint cost conf
        in canRecover                               recovery === canRec &&
           shouldContinue                               recovery === shouldCont &&
           recoveryAction                               recovery === action &&
           recoveryHint                               recovery === hint &&
           recoveryCost                               recovery === cost &&
           recoveryConfidence                               recovery === conf

  , QC.testProperty "_sequenceRecovery combines costs additively" $
      \r1 r2 -> let combined = _sequenceRecovery r1 r2
                in recoveryCost                               combined === recoveryCost r1 + recoveryCost r2

  , QC.testProperty "_sequenceRecovery averages confidence" $
      \r1 r2 -> let combined = _sequenceRecovery r1 r2
                                                  expected = (recoveryConfidence r1 + recoveryConfidence r2) / 2
                in abs (recoveryConfidence combined - expected) < 0.001

  , QC.testProperty "_chooseBestRecovery selects highest confidence" $
      \r1 r2 -> canRecover r1 && canRecover                               r2 ==>
                let best = _chooseBestRecovery [r1, r2]
                                                  expected = if recoveryConfidence r1 >= recoveryConfidence r2 then r1 else r2
                in recoveryConfidence                               best === recoveryConfidence expected

  , QC.testProperty "_retryRecovery scales cost with attempts" $
      \attempts -> let recovery = _retryRecovery attempts
                    in recoveryCost                               recovery === 20 * attempts

  , QC.testProperty "_initialRecoveryContext has zero attempts" $
      \maxAttempts -> let ctx = _initialRecoveryContext maxAttempts
                      in recoveryAttempts                               ctx === 0 &&
                         maxRecoveryAttempts                               ctx === maxAttempts &&
                         L.null (recoveryHistory ctx) &&
                         currentStrategy                               ctx === Nothing
  ]

-- | TypeError properties
typeErrorProperties :: TestTree
typeErrorProperties = testGroup "TypeError Properties"
  [ QC.testProperty "errorAt "test-id" [Error, Fatal]) filtered

  , QC.testProperty "getWarnings filters by Warning severity" $
      \errs -> let filtered = getWarnings errs
                in L.all (\e -> severity                               e == Warning) filtered

  , QC.testProperty "getInfo filters by Info severity" $
      \errs -> let filtered = getInfo errs
                in L.all (\e -> severity                               e == Info) filtered

  , QC.testProperty "hasErrors detects Error L.or Fatal severity" $
      \errs -> hasErrors                               errs === L.any (\e -> severity e `elem` [Error, Fatal]) errs

  , QC.testProperty "hasWarnings detects Warning severity" $
      \errs -> hasWarnings                               errs === L.any (\e -> severity                               e == Warning) errs
  ]

-- | Error formatting properties
errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ QC.testProperty "formatError includes severity string" $
      \err -> let formatted = formatError err
                                                 severityStr = case severity err of
                     Fatal -> "FATAL"
                     Error -> "ERROR"
                     Warning -> "WARNING"
                     Info -> "INFO"
               in severityStr `L.isInfixOf` formatted

  , QC.testProperty "formatError includes category" $
      \err -> let formatted = formatError err
               in "[" ++ show (category err) ++ "]" `L.isInfixOf` formatted

  , QC.testProperty "formatError includes message" $
      \err -> T.unpack (message err) `L.isInfixOf` formatError err

  , QC.testProperty "formatTimestamp produces consistent format" $
      \time -> let formatted = formatTimestamp time
                in L.length formatted >= 23  -- YYYY-MM-DD HH:MM:SS.sss L.minimum
  ]

-- | Combined error properties
combinedErrorProperties :: TestTree
combinedErrorProperties = testGroup "Combined Error Properties"
  [ QC.testProperty "OwnershipErrorCombined preserves severity" $
      \sev err -> combinedErrorSeverity (OwnershipErrorCombined sev err) === sev

  , QC.testProperty "DependentTypeErrorCombined preserves severity" $
      \sev err -> combinedErrorSeverity (DependentTypeErrorCombined sev err) === sev

  , QC.testProperty "IntegrationError preserves severity" $
      \sev msg -> combinedErrorSeverity (IntegrationError msg sev) === sev

  , QC.testProperty "CrossAnalyzerError preserves severity" $
      \sev msg errs -> combinedErrorSeverity (CrossAnalyzerError msg sev errs) === sev

  , QC.testProperty "filterCombinedErrorsBySeverity filters correctly" $
      \minSeverity errs -> 
        let filtered = filterCombinedErrorsBySeverity minSeverity errs
        in L.all (\err -> isAtLeast minSeverity (combinedErrorSeverity err) filtered
  ]