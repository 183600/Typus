{-# LANGUAGE OverloadedStrings #-}

module Compiler.EnhancedErrors
    ( mkCompilerError
    , defaultSpan
    , defaultLocation
    ) where

import Data.Text (Text)

import EnhancedErrorHandler (CompilerError(..), CompilationPhase(..))
import ErrorHandler
    ( TypeError(..)
    , ErrorSeverity(..)
    , ErrorCategory(..)
    , ErrorLocation(..)
    , ErrorRecovery
    , errorWithCategory
    , fatalRecovery
    , errorRecovery
    , warningRecovery
    , infoRecovery
    )
import SourceLocation
    ( SourceSpan
    , SourcePos
    , startPos
    , spanFrom
    , toErrorLocationWithSpan
    )

-- | Default span used when no specific source information is available.
defaultSpan :: SourceSpan
defaultSpan = spanFrom defaultPos

-- | Default source position (line/column start at 1 in Typus sources).
defaultPos :: SourcePos
defaultPos = startPos

-- | Default error location when no span information is available.
defaultLocation :: ErrorLocation
defaultLocation = ErrorLocation
    { filePath = Nothing
    , line = 0
    , column = 0
    , endLine = Nothing
    , endColumn = Nothing
    }

-- | Helper to adjust severity and recovery on a type error.
applySeverity :: ErrorSeverity -> TypeError -> TypeError
applySeverity severity = case severity of
    Fatal   -> \err -> err { severity = Fatal, recovery = fatalRecovery }
    Error   -> \err -> err { severity = Error, recovery = errorRecovery }
    Warning -> \err -> err { severity = Warning, recovery = warningRecovery }
    Info    -> \err -> err { severity = Info, recovery = infoRecovery }

-- | Construct a compiler error using the enhanced error handling infrastructure.
mkCompilerError
    :: String               -- ^ Error identifier
    -> Text                 -- ^ Message presented to the user
    -> CompilationPhase     -- ^ Pipeline phase in which the error originated
    -> ErrorCategory        -- ^ High level error category
    -> ErrorSeverity        -- ^ Severity of the error (fatal, error, warning, info)
    -> Maybe SourceSpan     -- ^ Optional source span for rich location reporting
    -> Maybe String         -- ^ Optional code/context snippet
    -> [Text]               -- ^ Suggestions presented to the user
    -> [String]             -- ^ Optional stack trace indicating call chain
    -> Maybe ErrorRecovery  -- ^ Optional recovery information override
    -> CompilerError
mkCompilerError errId msg phase category severity mSpan mContext hintTexts stackTrace mRecovery =
    let loc = maybe defaultLocation toErrorLocationWithSpan mSpan
        base = errorWithCategory errId category msg loc
        baseWithSeverity = applySeverity severity base
        baseWithRecovery = maybe baseWithSeverity (\rec -> baseWithSeverity { recovery = rec }) mRecovery
        context0 = context baseWithRecovery
        context1 = context0 { contextCode = mContext }
        finalTypeError = baseWithRecovery
            { context = context1
            , suggestions = hintTexts ++ suggestions baseWithRecovery
            }
    in CompilerError
        { ceError = finalTypeError
        , ceSourceContext = mContext
        , ceStackTrace = stackTrace
        , cePhase = phase
        }
