-- | Verbosity levels for debug output.
module Sentinel.Verbosity
  ( Verbosity (..),
  )
where

import Data.Char qualified as Char
import Options.Applicative (maybeReader)
import Options.Generic (ParseField (..), ParseFields (..), ParseRecord (..))
import Pre
import Text.Read (readMaybe)
import Prelude qualified

-- | Debug verbosity levels.
--
-- The 'Ord' instance allows @when (level >= Basic) ...@ style checks.
data Verbosity
  = -- | No debug output (default)
    Silent
  | -- | Blocked vars, tools available
    Basic
  | -- | Guard evaluation, solver steps
    Detailed
  | -- | All internal state
    Verbose
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Parse a verbosity level from a string (case-insensitive).
parseVerbosity :: Prelude.String -> Maybe Verbosity
parseVerbosity s = readMaybe (capitalize s)
  where
    capitalize [] = []
    capitalize (c : cs) = Char.toUpper c : fmap Char.toLower cs

instance ParseField Verbosity where
  readField = maybeReader parseVerbosity

instance ParseFields Verbosity

instance ParseRecord Verbosity
