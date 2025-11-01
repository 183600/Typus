module Compiler.Error (
    ErrorKind(..),
    SourceLocation(..),
    CompilationError(..),
    mkCompilationError,
    sourceLocation,
    renderCompilationError
) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)

-- | Classification for compiler errors so that callers can react based on the
-- failing pipeline stage.
data ErrorKind
    = SyntaxErrorKind
    | DependentTypeErrorKind
    | TypeErrorKind
    | OwnershipErrorKind
    | GoGenerationErrorKind
    | InternalErrorKind
    deriving (Eq, Show)

-- | Optional source information for diagnostics that can pinpoint an exact
-- location.
data SourceLocation = SourceLocation
    { locationFile :: Maybe FilePath
    , locationLine :: Maybe Int
    , locationColumn :: Maybe Int
    } deriving (Eq, Show)

-- | Structured error returned by the compiler. The error message is intended
-- for human consumption while the structured fields allow IDEs/CLIs to provide
-- richer feedback.
data CompilationError = CompilationError
    { errorKind :: ErrorKind
    , errorMessage :: String
    , errorLocations :: [SourceLocation]
    } deriving (Eq, Show)

mkCompilationError :: ErrorKind -> String -> [SourceLocation] -> CompilationError
mkCompilationError kind message locations = CompilationError
    { errorKind = kind
    , errorMessage = message
    , errorLocations = locations
    }

sourceLocation :: Maybe FilePath -> Maybe Int -> Maybe Int -> SourceLocation
sourceLocation file line column = SourceLocation
    { locationFile = file
    , locationLine = line
    , locationColumn = column
    }

renderCompilationError :: CompilationError -> String
renderCompilationError err =
    let prefix = "[" ++ show (errorKind err) ++ "] " ++ errorMessage err
        locationText = renderLocations (errorLocations err)
    in prefix ++ locationText
  where
    renderLocations [] = ""
    renderLocations locs =
        let rendered = catMaybes (map renderLocation locs)
        in if null rendered
              then ""
              else " (" ++ intercalate "; " rendered ++ ")"

    renderLocation loc =
        let parts = catMaybes
                [ fmap ("file " ++) (locationFile loc)
                , fmap (\ln -> "line " ++ show ln) (locationLine loc)
                , fmap (\col -> "column " ++ show col) (locationColumn loc)
                ]
        in if null parts then Nothing else Just (intercalate ", " parts)
