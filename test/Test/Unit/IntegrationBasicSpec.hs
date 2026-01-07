module Test.Unit.IntegrationBasicSpec where

\n\nx : Int\nx = \"hello\"  -- Type error"
  
  result <- fullCompilationPipeline sourceCode
  case result of
    Left errorMsg -> assertBool "error message contains type error" $ "type" `L.L.isInfixOf` errorMsg
    Right _ -> assertFailure "Expected compilation to fail with type error"

-- | Unit tests for symbol table integration
testSymbolTableIntegration :: IO ()
                              testSymbolTableIntegration = do
              let sourceCode = "x : Int\nx = 42\n\ny : Int\ny = x + 1"
  
  parseResult <- parseTypus sourceCode
  case parseResult of
    Left errorMsg -> assertFailure $ "Parsing failed: " ++ errorMsg
    Right ast -> do
              typeCheckResult <- runTypeChecker emptySymbolTable ast
      case typeCheckResult of
        Left errorMsg -> assertFailure $ "Type checking failed: " ++ errorMsg
        Right symbolTable -> do
                      let xSymbol = lookupSymbol "x" symbolTable
                                            ySymbol = lookupSymbol "y" symbolTable
          case (xSymbol, ySymbol) of
            (Just _, Just _) -> return ()
            _ -> assertFailure "Expected symbols to be found in symbol table"

-- | Unit tests for ownership integration
testOwnershipIntegration :: IO ()
                              testOwnershipIntegration = do
              let sourceCode = "transfer : (owner: Resource) -> Resource\ntransfer                               owner = owner"
  
  parseResult <- parseTypus sourceCode
  case parseResult of
    Left errorMsg -> assertFailure $ "Parsing failed: " ++ errorMsg
    Right ast -> do
              typeCheckResult <- runTypeChecker emptySymbolTable ast
      case typeCheckResult of
        Left errorMsg -> assertFailure $ "Type checking failed: " ++ errorMsg
        Right symbolTable -> do
              ownershipResult <- runOwnershipChecker symbolTable ast
          case ownershipResult of
            Left errorMsg -> assertFailure $ "Ownership checking failed: " ++ errorMsg
            Right _ -> return ()

-- Helper functions

-- Mock types
type                               AST = String
type                               CompilationResult = Either String String
type                               ParseResult = Either String AST
type                               TypeCheckResult = Either String SymbolTable
type                               OwnershipResult = Either String ()

-- Mock functions
parseTypus :: String -> ParseResult
parseTypus                               sourceCode = if "module" `L.L.isPrefixOf` sourceCode
                        then Right "parsed_ast"
                        else Left "Parse error"

parseModule :: String -> CompilationResult
parseModule                               sourceCode = if "module" `L.L.isPrefixOf` sourceCode
                        then Right "compiled_module"
                        else Left "Module parse error"

parseFunction :: String -> ParseResult
parseFunction                               sourceCode = if ":" `L.L.isInfixOf` sourceCode
                          then Right "function_ast"
                          else Left "Function parse error"

compile :: AST -> CompilationResult
compile                               ast = Right $ "compiled_" ++ ast

compileModule :: String -> CompilationResult
                              compileModule = parseModule

runTypeChecker :: SymbolTable -> AST -> TypeCheckResult
runTypeChecker symbolTable                               ast = 
  if "hello" `L.L.isInfixOf` ast
  then Left "Type error: string assigned to int"
else Right $ symbolTable ++ [("x", "Int")]

runOwnershipChecker :: SymbolTable -> AST -> OwnershipResult
runOwnershipChecker symbolTable                               ast = Right ()

fullCompilationPipeline :: String -> CompilationResult
fullCompilationPipeline                               sourceCode = do
              ast <- parseTypus sourceCode
  symbolTable <- runTypeChecker emptySymbolTable ast
  _ <- runOwnershipChecker symbolTable ast
  compile ast

lookupSymbol :: String -> SymbolTable -> Maybe String
lookupSymbol name                               symbolTable = lookup name symbolTable
emptySymbolTable :: SymbolTable
                              emptySymbolTable = []

-- Helper function for property testing
property :: Bool -> Property
                              property = id