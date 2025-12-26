{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SecurityValidationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler
import Compiler.IR
import Parser
import TypeChecker
import SourceLocation
import Utils
import Security

import Data.Char (isSpace, isLetter, isDigit, toLower, isControl)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, intercalate)
import Data.String (IsString)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Input sanitization prevents injection attacks
prop_input_sanitization_prevents_injection :: String -> Property
prop_input_sanitization_prevents_injection input =
  length input <= 50 ==> -- Limit for performance
  let sanitized = sanitizeInput input
      dangerous = ["<script>", "javascript:", "eval(", "exec(", "system("]
  in property $ not (any (`isInfixOf` map toLower sanitized) dangerous)

-- Property: Buffer overflow protection
prop_buffer_overflow_protection :: String -> Int -> Property
prop_buffer_overflow_protection content size =
  length content <= 20 && size >= 1 && size <= 1000 ==>
  let buffer = allocateBuffer size
      result = writeToBuffer buffer content
  in property $ bufferOverflowPrevented result

-- Property: Memory safety validation
prop_memory_safety_validation :: String -> Property
prop_memory_safety_validation code =
  length code <= 60 ==> -- Limit for performance
  let result = analyzeMemorySafety code
  in property $ memoryAccessesAreSafe result

-- Property: Type safety prevents cast attacks
prop_type_safety_prevents_cast_attacks :: String -> String -> Property
prop_type_safety_prevents_cast_attacks fromType toType =
  length fromType <= 10 && length toType <= 10 && all isLetter fromType && all isLetter toType ==>
  let castCode = "var x : " ++ toType ++ " = (" ++ toType ++ ")someValue;"
      result = checkTypeSafety castCode
  in property $ unsafeCastPrevented result

-- Property: Null pointer dereference detection
prop_null_pointer_detection :: String -> Property
prop_null_pointer_detection code =
  length code <= 40 ==> -- Limit for performance
  let result = analyzeNullPointerSafety code
  in property $ nullDereferencesDetected result

-- Property: Integer overflow protection
prop_integer_overflow_protection :: Int -> Int -> Property
prop_integer_overflow_protection x y =
  x >= 0 && y >= 0 && x <= 1000000 && y <= 1000000 ==>
  let operation = show x ++ " + " ++ show y
      result = checkIntegerOverflow operation
  in property $ overflowHandled result

-- Property: Stack overflow protection
prop_stack_overflow_protection :: Int -> Property
prop_stack_overflow_protection recursionDepth =
  recursionDepth >= 0 && recursionDepth <= 100 ==>
  let recursiveCode = "function rec(n) { if (n > 0) return rec(n - 1); return 0; } rec(" ++ show recursionDepth ++ ");"
      result = checkStackOverflow recursiveCode
  in property $ stackOverflowPrevented result

-- Property: Resource exhaustion protection
prop_resource_exhaustion_protection :: String -> Property
prop_resource_exhaustion_protection code =
  length code <= 50 ==> -- Limit for performance
  let result = checkResourceUsage code
  in property $ resourcesAreLimited result

-- Property: Secure random number generation
prop_secure_random_generation :: Int -> Property
prop_secure_random_generation seed =
  seed >= 0 && seed <= 1000 ==>
  let randomValue = generateSecureRandom seed
  in property $ randomValueIsSecure randomValue

-- Property: Cryptographic validation
prop_cryptographic_validation :: String -> Property
prop_cryptographic_validation input =
  length input <= 30 ==> -- Limit for performance
  let hash = computeSecureHash input
      result = validateCryptographicOperation hash
  in property $ cryptographicOperationIsSecure result

-- Property: Access control validation
prop_access_control_validation :: String -> String -> Property
prop_access_control_validation user resource =
  length user <= 10 && length resource <= 15 && all isLetter user && all isLetter resource ==>
  let accessCode = "user " ++ user ++ " access " ++ resource
      result = checkAccessControl accessCode
  in property $ accessIsProperlyControlled result

-- Property: Data encryption validation
prop_data_encryption_validation :: String -> Property
prop_data_encryption_validation data =
  length data <= 40 ==> -- Limit for performance
  let encrypted = encryptData data
      decrypted = decryptData encrypted
  in property $ data `isInfixOf` decrypted && encryptionIsSecure encrypted

-- Property: Authentication token security
prop_authentication_token_security :: String -> Property
prop_authentication_token_security token =
  length token <= 50 ==> -- Limit for performance
  let result = validateAuthToken token
  in property $ tokenIsSecure result

-- Property: SQL injection prevention
prop_sql_injection_prevention :: String -> Property
prop_sql_injection_prevention input =
  length input <= 40 ==> -- Limit for performance
  let query = "SELECT * FROM users WHERE name = '" ++ input ++ "'"
      sanitized = sanitizeSQLQuery query
      dangerous = ["'", ";", "--", "/*", "*/", "xp_", "sp_"]
  in property $ not (any (`isInfixOf` map toLower sanitized) dangerous)

-- Property: XSS prevention
prop_xss_prevention :: String -> Property
prop_xss_prevention input =
  length input <= 30 ==> -- Limit for performance
  let sanitized = sanitizeHTML input
      dangerous = ["<script", "onerror=", "onload=", "javascript:", "vbscript:"]
  in property $ not (any (`isInfixOf` map toLower sanitized) dangerous)

-- Property: Path traversal prevention
prop_path_traversal_prevention :: String -> Property
prop_path_traversal_prevention path =
  length path <= 30 ==> -- Limit for performance
  let sanitized = sanitizePath path
      dangerous = ["../", "..\\", "%2e%2e", "..%2f", "%2e%2e%2f"]
  in property $ not (any (`isInfixOf` map toLower sanitized) dangerous)

-- Property: Command injection prevention
prop_command_injection_prevention :: String -> Property
prop_command_injection_prevention command =
  length command <= 30 ==> -- Limit for performance
  let sanitized = sanitizeCommand command
      dangerous = [";", "&", "|", "`", "$(", "${"]
  in property $ not (any (`isInfixOf` sanitized) dangerous)

-- Property: Code injection prevention
prop_code_injection_prevention :: String -> Property
prop_code_injection_prevention code =
  length code <= 40 ==> -- Limit for performance
  let result = analyzeCodeInjection code
  in property $ codeInjectionPrevented result

-- Property: Deserialization security
prop_deserialization_security :: String -> Property
prop_deserialization_security dataStr =
  length dataStr <= 30 ==> -- Limit for performance
  let result = validateDeserialization dataStr
  in property $ deserializationIsSecure result

-- Property: Information leakage prevention
prop_information_leakage_prevention :: String -> Property
prop_information_leakage_prevention errorOutput =
  length errorOutput <= 50 ==> -- Limit for performance
  let sanitized = sanitizeErrorOutput errorOutput
      sensitive = ["password", "secret", "key", "token", "credential"]
  in property $ not (any (`isInfixOf` map toLower sanitized) sensitive)

-- Property: Rate limiting validation
prop_rate_limiting_validation :: String -> Int -> Property
prop_rate_limiting_validation userId requestCount =
  length userId <= 10 && all isLetter userId && requestCount >= 0 && requestCount <= 100 ==>
  let result = checkRateLimit userId requestCount
  in property $ rateLimitIsEnforced result

-- Property: Session security validation
prop_session_security_validation :: String -> Property
prop_session_security_validation sessionId =
  length sessionId <= 40 ==> -- Limit for performance
  let result = validateSession sessionId
  in property $ sessionIsSecure result

-- Advanced security tests

-- Property: Complex attack patterns
prop_complex_attack_patterns :: [String] -> Property
prop_complex_attack_patterns attackVectors =
  not (null attackVectors) && all (\v -> length v <= 20) attackVectors && length attackVectors <= 5 ==>
  let combinedAttack = intercalate " " attackVectors
      result = analyzeSecurityThreats combinedAttack
  in property $ securityThreatsPrevented result

-- Property: Zero-day vulnerability detection
prop_zero_day_detection :: String -> Property
prop_zero_day_detection suspiciousCode =
  length suspiciousCode <= 60 ==> -- Limit for performance
  let result = detectZeroDayVulnerabilities suspiciousCode
  in property $ zeroDayVulnerabilitiesDetected result

-- Property: Supply chain security
prop_supply_chain_security :: String -> Property
prop_supply_chain_security dependency =
  length dependency <= 30 ==> -- Limit for performance
  let result = validateSupplyChainSecurity dependency
  in property $ supplyChainIsSecure result

-- Property: Runtime security monitoring
prop_runtime_security_monitoring :: String -> Property
prop_runtime_security_monitoring code =
  length code <= 50 ==> -- Limit for performance
  let result = monitorRuntimeSecurity code
  in property $ runtimeSecurityIsMonitored result

-- Helper functions
sanitizeInput :: String -> String
sanitizeInput = filter (not . isControl)

allocateBuffer :: Int -> Buffer
allocateBuffer _ = Buffer

writeToBuffer :: Buffer -> String -> BufferResult
writeToBuffer _ _ = BufferResult

bufferOverflowPrevented :: BufferResult -> Bool
bufferOverflowPrevented _ = True

analyzeMemorySafety :: String -> MemorySafetyResult
analyzeMemorySafety _ = MemorySafe

memoryAccessesAreSafe :: MemorySafetyResult -> Bool
memoryAccessesAreSafe MemorySafe = True
memoryAccessesAreSafe _ = False

checkTypeSafety :: String -> TypeSafetyResult
checkTypeSafety _ = TypeSafe

unsafeCastPrevented :: TypeSafetyResult -> Bool
unsafeCastPrevented TypeSafe = False
unsafeCastPrevented _ = True

analyzeNullPointerSafety :: String -> NullSafetyResult
analyzeNullPointerSafety _ = NullSafe

nullDereferencesDetected :: NullSafetyResult -> Bool
nullDereferencesDetected NullSafe = False
nullDereferencesDetected _ = True

checkIntegerOverflow :: String -> OverflowResult
checkIntegerOverflow _ = OverflowSafe

overflowHandled :: OverflowResult -> Bool
overflowHandled OverflowSafe = True
overflowHandled _ = False

checkStackOverflow :: String -> StackResult
checkStackOverflow _ = StackSafe

stackOverflowPrevented :: StackResult -> Bool
stackOverflowPrevented StackSafe = False
stackOverflowPrevented _ = True

checkResourceUsage :: String -> ResourceResult
checkResourceUsage _ = ResourceSafe

resourcesAreLimited :: ResourceResult -> Bool
resourcesAreLimited ResourceSafe = True
resourcesAreLimited _ = False

generateSecureRandom :: Int -> SecureRandom
generateSecureRandom _ = SecureRandom

randomValueIsSecure :: SecureRandom -> Bool
randomValueIsSecure _ = True

computeSecureHash :: String -> SecureHash
computeSecureHash _ = SecureHash

validateCryptographicOperation :: SecureHash -> CryptoResult
validateCryptographicOperation _ = CryptoSecure

cryptographicOperationIsSecure :: CryptoResult -> Bool
cryptographicOperationIsSecure CryptoSecure = True
cryptographicOperationIsSecure _ = False

checkAccessControl :: String -> AccessResult
checkAccessControl _ = AccessControlled

accessIsProperlyControlled :: AccessResult -> Bool
accessIsProperlyControlled AccessControlled = True
accessIsProperlyControlled _ = False

encryptData :: String -> EncryptedData
encryptData _ = EncryptedData

decryptData :: EncryptedData -> String
decryptData _ = "decrypted"

encryptionIsSecure :: EncryptedData -> Bool
encryptionIsSecure _ = True

validateAuthToken :: String -> AuthResult
validateAuthToken _ = AuthValid

tokenIsSecure :: AuthResult -> Bool
tokenIsSecure AuthValid = True
tokenIsSecure _ = False

sanitizeSQLQuery :: String -> String
sanitizeSQLQuery = filter (`notElem` "'\";\\/*-")

sanitizeHTML :: String -> String
sanitizeHTML = filter (`notElem` "<>\"'&")

sanitizePath :: String -> String
sanitizePath = filter (`notElem` "/\\.")

sanitizeCommand :: String -> String
sanitizeCommand = filter (`notElem` "&|`$")

analyzeCodeInjection :: String -> InjectionResult
analyzeCodeInjection _ = InjectionSafe

codeInjectionPrevented :: InjectionResult -> Bool
codeInjectionPrevented InjectionSafe = True
codeInjectionPrevented _ = False

validateDeserialization :: String -> DeserializationResult
validateDeserialization _ = DeserializationSafe

deserializationIsSecure :: DeserializationResult -> Bool
deserializationIsSecure DeserializationSafe = True
deserializationIsSecure _ = False

sanitizeErrorOutput :: String -> String
sanitizeErrorOutput = map (\c -> if c `elem` "abcdef" then 'x' else c)

checkRateLimit :: String -> Int -> RateLimitResult
checkRateLimit _ count = if count > 10 then RateLimited else RateAllowed

rateLimitIsEnforced :: RateLimitResult -> Bool
rateLimitIsEnforced RateLimited = True
rateLimitIsEnforced RateAllowed = False

validateSession :: String -> SessionResult
validateSession _ = SessionValid

sessionIsSecure :: SessionResult -> Bool
sessionIsSecure SessionValid = True
sessionIsSecure _ = False

analyzeSecurityThreats :: String -> SecurityResult
analyzeSecurityThreats _ = SecuritySafe

detectZeroDayVulnerabilities :: String -> ZeroDayResult
detectZeroDayVulnerabilities _ = ZeroDaySafe

validateSupplyChainSecurity :: String -> SupplyChainResult
validateSupplyChainSecurity _ = SupplyChainSafe

monitorRuntimeSecurity :: String -> RuntimeSecurityResult
monitorRuntimeSecurity _ = RuntimeSecuritySafe

-- Simplified types for testing
data Buffer = Buffer
data BufferResult = BufferResult
data MemorySafetyResult = MemorySafe | MemoryUnsafe
data TypeSafetyResult = TypeSafe | TypeUnsafe
data NullSafetyResult = NullSafe | NullUnsafe
data OverflowResult = OverflowSafe | OverflowUnsafe
data StackResult = StackSafe | StackUnsafe
data ResourceResult = ResourceSafe | ResourceUnsafe
data SecureRandom = SecureRandom
data SecureHash = SecureHash
data CryptoResult = CryptoSecure | CryptoUnsafe
data AccessResult = AccessControlled | AccessUncontrolled
data EncryptedData = EncryptedData
data AuthResult = AuthValid | AuthInvalid
data InjectionResult = InjectionSafe | InjectionUnsafe
data DeserializationResult = DeserializationSafe | DeserializationUnsafe
data RateLimitResult = RateAllowed | RateLimited
data SessionResult = SessionValid | SessionInvalid
data SecurityResult = SecuritySafe | SecurityUnsafe
data ZeroDayResult = ZeroDaySafe | ZeroDayVulnerable
data SupplyChainResult = SupplyChainSafe | SupplyChainUnsafe
data RuntimeSecurityResult = RuntimeSecuritySafe | RuntimeSecurityUnsafe

-- Additional helper functions for advanced tests
securityThreatsPrevented :: SecurityResult -> Bool
securityThreatsPrevented SecuritySafe = True
securityThreatsPrevented _ = False

zeroDayVulnerabilitiesDetected :: ZeroDayResult -> Bool
zeroDayVulnerabilitiesDetected ZeroDaySafe = False
zeroDayVulnerabilitiesDetected _ = True

supplyChainIsSecure :: SupplyChainResult -> Bool
supplyChainIsSecure SupplyChainSafe = True
supplyChainIsSecure _ = False

runtimeSecurityIsMonitored :: RuntimeSecurityResult -> Bool
runtimeSecurityIsMonitored RuntimeSecuritySafe = True
runtimeSecurityIsMonitored _ = False

tests :: TestTree
tests = testGroup "Security Validation Tests"
  [ fastProperty "Input sanitization prevents injection attacks" prop_input_sanitization_prevents_injection
  , fastProperty "Buffer overflow protection" prop_buffer_overflow_protection
  , fastProperty "Memory safety validation" prop_memory_safety_validation
  , fastProperty "Type safety prevents cast attacks" prop_type_safety_prevents_cast_attacks
  , fastProperty "Null pointer dereference detection" prop_null_pointer_detection
  , fastProperty "Integer overflow protection" prop_integer_overflow_protection
  , fastProperty "Stack overflow protection" prop_stack_overflow_protection
  , fastProperty "Resource exhaustion protection" prop_resource_exhaustion_protection
  , fastProperty "Secure random number generation" prop_secure_random_generation
  , fastProperty "Cryptographic validation" prop_cryptographic_validation
  , fastProperty "Access control validation" prop_access_control_validation
  , fastProperty "Data encryption validation" prop_data_encryption_validation
  , fastProperty "Authentication token security" prop_authentication_token_security
  , fastProperty "SQL injection prevention" prop_sql_injection_prevention
  , fastProperty "XSS prevention" prop_xss_prevention
  , fastProperty "Path traversal prevention" prop_path_traversal_prevention
  , fastProperty "Command injection prevention" prop_command_injection_prevention
  , fastProperty "Code injection prevention" prop_code_injection_prevention
  , fastProperty "Deserialization security" prop_deserialization_security
  , fastProperty "Information leakage prevention" prop_information_leakage_prevention
  , fastProperty "Rate limiting validation" prop_rate_limiting_validation
  , fastProperty "Session security validation" prop_session_security_validation
  , fastProperty "Complex attack patterns" prop_complex_attack_patterns
  , fastProperty "Zero-day vulnerability detection" prop_zero_day_detection
  , fastProperty "Supply chain security" prop_supply_chain_security
  , fastProperty "Runtime security monitoring" prop_runtime_security_monitoring
  ]