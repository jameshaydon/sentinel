-- | JSON extraction utilities for tool arguments and results.
--
-- This module provides helpers for extracting typed values from JSON objects,
-- used throughout the codebase for:
--
-- - Extracting tool arguments in tool execute functions
-- - Extracting facts from tool results in ToolBindings
--
-- All functions handle missing fields gracefully by returning 'Maybe' or empty lists.
module Sentinel.JSON
  ( -- * Simple Field Extraction
    extractString,
    extractNumber,
    extractBool,

    -- * Nested and Array Extraction
    extractNestedString,
    extractArrayField,

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

-- | Extract a number field from a JSON object.
--
-- Returns 'Nothing' if the field is missing, not a number, or the input is not an object.
--
-- @
-- extractNumber "age" (object ["age" .= 30]) == Just 30.0
-- extractNumber "age" (object ["name" .= "Alice"]) == Nothing
-- @
extractNumber :: Text -> Aeson.Value -> Maybe Double
extractNumber fieldName = \case
  Aeson.Object obj -> case KM.lookup (Key.fromText fieldName) obj of
    Just (Aeson.Number n) -> Just (realToFrac n)
    _ -> Nothing
  _ -> Nothing

-- | Extract a boolean field from a JSON object.
--
-- Returns 'Nothing' if the field is missing, not a boolean, or the input is not an object.
--
-- @
-- extractBool "active" (object ["active" .= True]) == Just True
-- extractBool "active" (object ["name" .= "Alice"]) == Nothing
-- @
extractBool :: Text -> Aeson.Value -> Maybe Bool
extractBool fieldName = \case
  Aeson.Object obj -> case KM.lookup (Key.fromText fieldName) obj of
    Just (Aeson.Bool b) -> Just b
    _ -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- Nested and Array Extraction
--------------------------------------------------------------------------------

-- | Extract a nested string field from a JSON object.
--
-- Traverses a path of field names and returns the string at the end.
-- Returns 'Nothing' if any field is missing or the final value is not a string.
--
-- @
-- let json = object ["user" .= object ["profile" .= object ["name" .= "Alice"]]]
-- extractNestedString ["user", "profile", "name"] json == Just "Alice"
-- extractNestedString ["user", "missing"] json == Nothing
-- @
extractNestedString :: [Text] -> Aeson.Value -> Maybe Text
extractNestedString path value = go path value
  where
    go [] (Aeson.String s) = Just s
    go (p : ps) (Aeson.Object obj) =
      case KM.lookup (Key.fromText p) obj of
        Just v -> go ps v
        Nothing -> Nothing
    go _ _ = Nothing

-- | Extract a field from each element of a JSON array.
--
-- Used for tools that return arrays (like SearchBookingsByName).
-- If the input is a single object (not an array), tries to extract from it.
--
-- @
-- let json = Array [object ["id" .= "a"], object ["id" .= "b"]]
-- extractArrayField "id" json == ["a", "b"]
-- @
extractArrayField :: Text -> Aeson.Value -> [Text]
extractArrayField fieldName = \case
  Aeson.Array arr -> mapMaybe (extractString fieldName) (toList arr)
  -- If it's a single object (not wrapped in array), try extracting from it
  obj@(Aeson.Object _) -> maybeToList (extractString fieldName obj)
  _ -> []

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
