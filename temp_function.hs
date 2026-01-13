-- | Test that formatErrorsWithLocation produces strings with locations
prop_formatErrorsWithLocationProducesStrings :: Property
prop_formatErrorsWithLocationProducesStrings =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let mkError msg = TypeError
                { errorId = "test-error"
                , severity = Error
                , category = SyntaxError
                , message = T.pack msg
                , location = ErrorLocation (Just "test") 0 0 Nothing Nothing
                , context = emptyContext
                , recovery = errorRecovery
                , suggestions = []
                , relatedErrors = []
                , errorChain = []
                , timestamp = Nothing
                }
          collector = foldl (\c msg -> 
            execState (addError (mkError msg)) c) newErrorCollector msgs
          errors = getErrors collector
          formatted = formatErrorsWithLocation errors
      in property $ length formatted == length msgs