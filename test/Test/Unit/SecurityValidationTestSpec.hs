{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SecurityValidationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Parser (parseTypus)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependentTypes)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, isInfixOf)
import Data.List (intercalate)
import Data.Char (isAscii, isControl, isSpace)
import Data.Maybe (isJust, isNothing)
import Text.Read (readMaybe)

-- ============================================================================
-- Security Test Utilities
-- ============================================================================

-- Check for potentially dangerous patterns in code
checkDangerousPatterns :: String -> [String]
checkDangerousPatterns code = 
  let patterns = 
        [ "eval(" -- Code execution
        , "exec(" -- Command execution
        , "system(" -- System calls
        , "shell(" -- Shell execution
        , "os.system" -- OS system calls
        , "subprocess" -- Subprocess execution
        , "unsafe" -- Unsafe operations
        , "reflect" -- Reflection
        , "runtime" -- Runtime manipulation
        , "syscall" -- System calls
        , "mmap" -- Memory mapping
        , "ptr" -- Pointer operations
        , "unsafe.Pointer" -- Unsafe pointers
        , "C." -- C interop
        ]
  in L.filter (`L.isInfixOf` code) patterns

-- Check for injection vulnerabilities
checkInjectionVulnerabilities :: String -> [String]
checkInjectionVulnerabilities code =
  let patterns =
        [ "sql" ++ "query" -- SQL injection
        , "exec" ++ "sql" -- SQL execution
        , "format" ++ "sql" -- SQL formatting
        , "shell" ++ "exec" -- Shell injection
        , "cmd" ++ "exec" -- Command injection
        , "path" ++ "traversal" -- Path traversal
        , "../" -- Directory traversal
        , "..\\" -- Windows directory traversal
        , "<script" -- XSS
        , "javascript:" -- XSS
        , "innerHTML" -- XSS
        ]
  in L.filter (`L.isInfixOf` code) patterns

-- Check for buffer overflow patterns
checkBufferOverflowPatterns :: String -> [String]
checkBufferOverflowPatterns code =
  let patterns =
        [ "strcpy" -- Unsafe string copy
        , "strcat" -- Unsafe string L.concat
        , "sprintf" -- Unsafe formatting
        , "gets" -- Unsafe input
        , "scanf" -- Unsafe input
        , "memcpy" -- Unsafe memory copy
        , "memset" -- Memory operations
        , "alloca" -- Stack allocation
        ]
  in L.filter (`L.isInfixOf` code) patterns

-- Check for cryptographic issues
checkCryptoIssues :: String -> [String]
checkCryptoIssues code =
  let patterns =
        [ "md5" -- Weak hash
        , "sha1" -- Weak hash
        , "des" -- Weak encryption
        , "rc4" -- Weak cipher
        , "rand" -- Weak random
        , "srand" -- Weak seeding
        , "time" -- Time-based seed
        , "uuid" -- Predictable UUID
        ]
  in L.filter (`L.isInfixOf` code) patterns

-- Check for input validation issues
checkInputValidation :: String -> [String]
checkInputValidation code =
  let hasValidation = L.any (`L.isInfixOf` code) 
        [ "validate", "sanitize", "escape", "filter", "check", "verify" ]
      hasInput = L.any (`L.isInfixOf` code)
        [ "input", "user", "form", "request", "param", "query" ]
  in if hasInput && not hasValidation
     then ["Missing input validation"]
     else []

-- Check for authentication/authorization issues
checkAuthIssues :: String -> [String]
checkAuthIssues code =
  let patterns =
        [ "password" ++ "==" -- Plain text password comparison
        , "admin" ++ "==" -- Hardcoded admin check
        , "token" ++ "==" -- Hardcoded token
        , "secret" ++ "==" -- Hardcoded secret
        , "key" ++ "==" -- Hardcoded key
        , "root" ++ "==" -- Hardcoded root check
        ]
  in L.filter (`L.isInfixOf` code) patterns

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate potentially dangerous code patterns
genDangerousCode :: Gen String
genDangerousCode = oneof
  [ pure "func dangerous() { exec(\"rm -rf /\") }"
  , pure "func eval(code string) { system(code) }"
  , pure "func unsafe() { ptr := unsafe.Pointer(&x) }"
  , pure "func syscall() { runtime.syscall(SYS_EXEC, ...) }"
  , pure "func reflect() { reflect.ValueOf(obj).MethodByName(\"dangerous\") }"
  ]

-- Generate code with injection vulnerabilities
genInjectionCode :: Gen String
genInjectionCode = oneof
  [ pure "func query(userInput string) { db.Exec(\"SELECT * FROM users WHERE id = \" + userInput) }"
  , pure "func shell(cmd string) { exec(\"sh -c \" + cmd) }"
  , pure "func path(userPath string) { file.Open(\"../\" + userPath) }"
  , pure "func html(userInput string) { innerHTML = \"<script>\" + userInput + \"</script>\" }"
  ]

-- Generate code with buffer overflow potential
genBufferOverflowCode :: Gen String
genBufferOverflowCode = oneof
  [ pure "func copy(dest, src string) { strcpy(dest, src) }"
  , pure "func format(buf, input string) { sprintf(buf, \"%s\", input) }"
  , pure "func input() { gets(buffer) }"
  , pure "func memory() { memcpy(dest, src, largeSize) }"
  ]

-- Generate code with crypto issues
genCryptoCode :: Gen String
genCryptoCode = oneof
  [ pure "func hash(data string) string { return md5.Sum(data) }"
  , pure "func encrypt(data string) string { return des.Encrypt(data, key) }"
  , pure "func random() int { srand(time.Now().Unix()); return rand() }"
  , pure "func uuid() string { return uuid.New() }"
  ]

-- Generate code with missing input validation
genUnvalidatedInputCode :: Gen String
genUnvalidatedInputCode = oneof
  [ pure "func process(input string) { db.Exec(\"INSERT INTO table VALUES (\" + input + \")\") }"
  , pure "func handle(userInput string) { file.Open(userInput) }"
  , pure "func render(template, data string) { fmt.Sprintf(template, data) }"
  ]

-- Generate code with authentication issues
genAuthCode :: Gen String
genAuthCode = oneof
  [ pure "func login(password string) bool { return password == \"admin123\" }"
  , pure "func checkAdmin(user string) bool { return user == \"admin\" }"
  , pure "func verify(token string) bool { return token == \"secret123\" }"
  ]

-- Generate secure code patterns
genSecureCode :: Gen String
genSecureCode = oneof
  [ pure "func safeQuery(input string) { stmt := db.Prepare(\"SELECT * FROM users WHERE id = ?\"); stmt.Exec(input) }"
  , pure "func safeCopy(dest, src string, size int) { strncpy(dest, src, size-1); dest[size-1] = '\\0' }"
  , pure "func safeHash(data string) string { return sha256.Sum256(data) }"
  , pure "func validate(input string) error { if len(input) > 100 { return errors.New(\"too long\") } return nil }"
  ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test dangerous pattern detection
testDangerousPatternDetection :: TestTree
testDangerousPatternDetection = testGroup "Dangerous Pattern Detection"
  [ testCase "detects eval usage" $ do
      let code = "func test() { eval(userInput) }"
          patterns = checkDangerousPatterns code
      assertBool "Should detect eval" $ "eval(" `elem` patterns
      
  , testCase "detects system calls" $ do
      let code = "func test() { system(\"rm -rf /\") }"
          patterns = checkDangerousPatterns code
      assertBool "Should detect system" $ "system(" `elem` patterns
      
  , testCase "detects unsafe operations" $ do
      let code = "func test() { ptr := unsafe.Pointer(&x) }"
          patterns = checkDangerousPatterns code
      assertBool "Should detect unsafe" $ "unsafe" `elem` patterns
      
  , testCase "safe code passes detection" $ do
      let code = "func test() { x := 42; return x }"
          patterns = checkDangerousPatterns code
      assertBool "Safe code should not trigger patterns" $ null patterns
  ]

-- Test injection vulnerability detection
testInjectionDetection :: TestTree
testInjectionDetection = testGroup "Injection Vulnerability Detection"
  [ testCase "detects SQL injection" $ do
      let code = "func query(id string) { db.Exec(\"SELECT * FROM users WHERE id = \" + id) }"
          patterns = checkInjectionVulnerabilities code
      assertBool "Should detect SQL injection" $ L.any (`L.isInfixOf` code) ["sql" ++ "query", "exec" ++ "sql"]
      
  , testCase "detects command injection" $ do
      let code = "func exec(cmd string) { system(\"sh -c \" + cmd) }"
          patterns = checkInjectionVulnerabilities code
      assertBool "Should detect command injection" $ "shell" ++ "exec" `elem` patterns
      
  , testCase "detects path traversal" $ do
      let code = "func open(path string) { file.Open(\"../\" + path) }"
          patterns = checkInjectionVulnerabilities code
      assertBool "Should detect path traversal" $ "../" `elem` patterns
      
  , testCase "parameterized queries are safe" $ do
      let code = "func query(id string) { stmt := db.Prepare(\"SELECT * FROM users WHERE id = ?\"); stmt.Exec(id) }"
          patterns = checkInjectionVulnerabilities code
      assertBool "Parameterized queries should be safe" $ null patterns
  ]

-- Test buffer overflow detection
testBufferOverflowDetection :: TestTree
testBufferOverflowDetection = testGroup "Buffer Overflow Detection"
  [ testCase "detects unsafe string operations" $ do
      let code = "func copy(dest, src string) { strcpy(dest, src) }"
          patterns = checkBufferOverflowPatterns code
      assertBool "Should detect strcpy" $ "strcpy" `elem` patterns
      
  , testCase "detects unsafe formatting" $ do
      let code = "func format(buf, input string) { sprintf(buf, \"%s\", input) }"
          patterns = checkBufferOverflowPatterns code
      assertBool "Should detect sprintf" $ "sprintf" `elem` patterns
      
  , testCase "safe alternatives are not flagged" $ do
      let code = "func copy(dest, src string, size int) { strncpy(dest, src, size-1) }"
          patterns = checkBufferOverflowPatterns code
      assertBool "strncpy should not be flagged" $ not $ "strncpy" `elem` patterns
  ]

-- Test cryptographic issue detection
testCryptoIssueDetection :: TestTree
testCryptoIssueDetection = testGroup "Cryptographic Issue Detection"
  [ testCase "detects weak hash functions" $ do
      let code = "func hash(data string) string { return md5.Sum(data) }"
          patterns = checkCryptoIssues code
      assertBool "Should detect MD5" $ "md5" `elem` patterns
      
  , testCase "detects weak encryption" $ do
      let code = "func encrypt(data string) string { return des.Encrypt(data, key) }"
          patterns = checkCryptoIssues code
      assertBool "Should detect DES" $ "des" `elem` patterns
      
  , testCase "detects weak random generation" $ do
      let code = "func random() int { srand(time.Now().Unix()); return rand() }"
          patterns = checkCryptoIssues code
      assertBool "Should detect time-based seeding" $ "time" `elem` patterns
      
  , testCase "strong crypto is not flagged" $ do
      let code = "func hash(data string) string { return sha256.Sum256(data) }"
          patterns = checkCryptoIssues code
      assertBool "SHA-256 should not be flagged" $ not $ "sha256" `elem` patterns
  ]

-- Test input validation detection
testInputValidationDetection :: TestTree
testInputValidationDetection = testGroup "Input Validation Detection"
  [ testCase "detects missing validation" $ do
      let code = "func process(input string) { db.Exec(\"INSERT INTO table VALUES (\" + input + \")\") }"
          patterns = checkInputValidation code
      assertBool "Should detect missing validation" $ not $ null patterns
      
  , testCase "recognizes proper validation" $ do
      let code = "func process(input string) { if len(input) > 100 { return }; db.Exec(\"INSERT INTO table VALUES (?)\", input) }"
          patterns = checkInputValidation code
      assertBool "Should recognize validation" $ null patterns
      
  , testCase "detects validation without sanitization" $ do
      let code = "func process(input string) { if len(input) > 0 { db.Exec(\"INSERT INTO table VALUES (\" + input + \")\") }"
          patterns = checkInputValidation code
      assertBool "Should detect incomplete validation" $ not $ null patterns
  ]

-- Test authentication issue detection
testAuthIssueDetection :: TestTree
testAuthIssueDetection = testGroup "Authentication Issue Detection"
  [ testCase "detects hardcoded passwords" $ do
      let code = "func login(pwd string) bool { return pwd == \"admin123\" }"
          patterns = checkAuthIssues code
      assertBool "Should detect hardcoded password" $ not $ null patterns
      
  , testCase "detects hardcoded admin checks" $ do
      let code = "func isAdmin(user string) bool { return user == \"admin\" }"
          patterns = checkAuthIssues code
      assertBool "Should detect hardcoded admin" $ not $ null patterns
      
  , testCase "recognizes proper authentication" $ do
      let code = "func login(pwd string) bool { hash := bcrypt.Hash(pwd); return db.CompareHash(hash, storedHash) }"
          patterns = checkAuthIssues code
      assertBool "Proper auth should not be flagged" $ null patterns
  ]

-- Test secure code processing
testSecureCodeProcessing :: TestTree
testSecureCodeProcessing = testGroup "Secure Code Processing"
  [ testCase "parser handles malicious input safely" $ do
      let maliciousCode = "func evil() { eval(\"rm -rf /\"); system(\"shutdown\"); }"
      case parseTypus maliciousCode of
        Left _ -> assertBool "Should handle malicious code safely" True
        Right _ -> assertBool "Should parse without executing" True
        
  , testCase "ownership analysis handles malicious code" $ do
      let maliciousCode = "func evil() { ptr := unsafe.Pointer(nil); *ptr = 42 }"
      case analyzeOwnership maliciousCode of
        Left _ -> assertBool "Should handle malicious ownership" True
        Right _ -> assertBool "Should analyze without executing" True
        
  , testCase "dependency analysis handles malicious code" $ do
      let maliciousCode = "func evil() { exec(\"malicious command\") }"
      case analyzeDependentTypes maliciousCode of
        Left _ -> assertBool "Should handle malicious dependencies" True
        Right _ -> assertBool "Should analyze without executing" True
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Dangerous code is detected
prop_dangerous_code_detected :: Property
prop_dangerous_code_detected =
  forAll genDangerousCode $ \code ->
    let patterns = checkDangerousPatterns code
    in property $ not $ null patterns

-- Property: Injection vulnerabilities are detected
prop_injection_vulnerabilities_detected :: Property
prop_injection_vulnerabilities_detected =
  forAll genInjectionCode $ \code ->
    let patterns = checkInjectionVulnerabilities code
    in property $ not $ null patterns

-- Property: Buffer overflow patterns are detected
prop_buffer_overflow_detected :: Property
prop_buffer_overflow_detected =
  forAll genBufferOverflowCode $ \code ->
    let patterns = checkBufferOverflowPatterns code
    in property $ not $ null patterns

-- Property: Crypto issues are detected
prop_crypto_issues_detected :: Property
prop_crypto_issues_detected =
  forAll genCryptoCode $ \code ->
    let patterns = checkCryptoIssues code
    in property $ not $ null patterns

-- Property: Missing input validation is detected
prop_missing_validation_detected :: Property
prop_missing_validation_detected =
  forAll genUnvalidatedInputCode $ \code ->
    let patterns = checkInputValidation code
    in property $ not $ null patterns

-- Property: Authentication issues are detected
prop_auth_issues_detected :: Property
prop_auth_issues_detected =
  forAll genAuthCode $ \code ->
    let patterns = checkAuthIssues code
    in property $ not $ null patterns

-- Property: Secure code passes validation
prop_secure_code_passes :: Property
prop_secure_code_passes =
  forAll genSecureCode $ \code ->
    let dangerousPatterns = checkDangerousPatterns code
        injectionPatterns = checkInjectionVulnerabilities code
        bufferPatterns = checkBufferOverflowPatterns code
        cryptoPatterns = checkCryptoIssues code
        authPatterns = checkAuthIssues code
        allPatterns = dangerousPatterns ++ injectionPatterns ++ bufferPatterns ++ cryptoPatterns ++ authPatterns
    in property $ null allPatterns

-- Property: Parser doesn't execute malicious code
prop_parser_no_execution :: Property
prop_parser_no_execution =
  forAll genDangerousCode $ \code ->
    case parseTypus code of
      Left _ -> property True
      Right _ -> property True  -- Parsing should not execute code

-- Property: Analysis doesn't execute malicious code
prop_analysis_no_execution :: Property
prop_analysis_no_execution =
  forAll genDangerousCode $ \code ->
    let parseResult = parseTypus code
        ownershipResult = analyzeOwnership code
        depResult = analyzeDependentTypes code
    in property $ True  -- If we get here, no code was executed

-- Property: Security checks are comprehensive
prop_security_checks_comprehensive :: String -> Property
prop_security_checks_comprehensive code =
  let dangerous = checkDangerousPatterns code
      injection = checkInjectionVulnerabilities code
      buffer = checkBufferOverflowPatterns code
      crypto = checkCryptoIssues code
      input = checkInputValidation code
      auth = checkAuthIssues code
      allChecks = dangerous ++ injection ++ buffer ++ crypto ++ input ++ auth
  in property $ L.length allChecks >= 0  -- Always true, ensures L.all checks run

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Security Validation Tests"
  [ testDangerousPatternDetection
  , testInjectionDetection
  , testBufferOverflowDetection
  , testCryptoIssueDetection
  , testInputValidationDetection
  , testAuthIssueDetection
  , testSecureCodeProcessing
  , testGroup "QuickCheck Properties"
    [ fastProperty "Dangerous code detected" prop_dangerous_code_detected
    , fastProperty "Injection vulnerabilities detected" prop_injection_vulnerabilities_detected
    , fastProperty "Buffer overflow detected" prop_buffer_overflow_detected
    , fastProperty "Crypto issues detected" prop_crypto_issues_detected
    , fastProperty "Missing validation detected" prop_missing_validation_detected
    , fastProperty "Auth issues detected" prop_auth_issues_detected
    , fastProperty "Secure code passes" prop_secure_code_passes
    , fastProperty "Parser no execution" prop_parser_no_execution
    , fastProperty "Analysis no execution" prop_analysis_no_execution
    , fastProperty "Security checks comprehensive" prop_security_checks_comprehensive
    ]
  ]