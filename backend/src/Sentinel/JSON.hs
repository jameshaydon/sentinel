-- | JSON extraction utilities for tool arguments and results.
--
-- This module provides helpers for extracting typed values from JSON objects,
-- used throughout the codebase for extracting tool arguments in tool execute
-- functions.
--
-- All functions handle missing fields gracefully by returning 'Maybe'.
module Sentinel.JSON
  ( -- * Simple Field Extraction
    extractString,

    -- * Tool Argument Extraction
    extractToolArg,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Pre
import Sentinel.Solver.Types (Scalar, scalarFromJSON)

--------------------------------------------------------------------------------
-- Simple Field Extraction
--------------------------------------------------------------------------------

-- | Extract a string field from a JSON object.
--
-- Returns 'Nothing' if the field is missing, not a string, or the input is not an object.
--
-- @
-- extractString "name" (object ["name" .= "Alice"]) == Just "Alice"
-- extractString "name" (object ["age" .= 30])       == Nothing
-- @
extractString :: Text -> Aeson.Value -> Maybe Text
extractString fieldName = \case
  Aeson.Object obj -> case KM.lookup (Key.fromText fieldName) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- Tool Argument Extraction
--------------------------------------------------------------------------------

-- | Extract a scalar value from JSON tool arguments.
--
-- Looks up a field in a JSON object and attempts to parse it as a 'Scalar'.
-- Returns 'Nothing' if the field is missing or cannot be converted to a scalar.
--
-- Scalars include: strings, numbers, and booleans.
--
-- @
-- extractToolArg "name" (object ["name" .= "Alice"]) == Just (ScStr "Alice")
-- extractToolArg "age" (object ["age" .= 30]) == Just (ScNum 30.0)
-- @
extractToolArg :: Text -> Aeson.Value -> Maybe Scalar
extractToolArg key (Aeson.Object obj) =
  case KM.lookup (Key.fromText key) obj of
    Just v -> scalarFromJSON v
    Nothing -> Nothing
extractToolArg _ _ = Nothing
