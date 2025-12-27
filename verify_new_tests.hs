{-# LANGUAGE PackageImports #-}

import qualified Test.Unit.NewParserPropertiesSpec
import qualified Test.Unit.NewSourceLocationMathSpec
import qualified Test.Unit.NewErrorHandlerCoreSpec
import qualified Test.Unit.NewUtilsStringPropertiesSpec
import qualified Test.Unit.NewOwnershipTransferPropertiesSpec
import qualified Test.Unit.NewDependenciesCorePropertiesSpec
import qualified Test.Unit.NewSyntaxValidatorBoundarySpec

main :: IO ()
main = do
  putStrLn "✅ All new test modules imported successfully!"
  putStrLn "New test modules created:"
  putStrLn "1. Test.Unit.NewParserPropertiesSpec"
  putStrLn "2. Test.Unit.NewSourceLocationMathSpec"
  putStrLn "3. Test.Unit.NewErrorHandlerCoreSpec"
  putStrLn "4. Test.Unit.NewUtilsStringPropertiesSpec"
  putStrLn "5. Test.Unit.NewOwnershipTransferPropertiesSpec"
  putStrLn "6. Test.Unit.NewDependenciesCorePropertiesSpec"
  putStrLn "7. Test.Unit.NewSyntaxValidatorBoundarySpec"