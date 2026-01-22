-- | JSON field extraction utilities for tool arguments.
module Sentinel.JSON
  ( extractString,
    extractToolArg,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Pre
import Sentinel.Solver.Types (Scalar, scalarFromJSON)

extractString :: Text -> Aeson.Value -> Maybe Text
extractString fieldName = \case
  Aeson.Object obj -> case KM.lookup (Key.fromText fieldName) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

extractToolArg :: Text -> Aeson.Value -> Maybe Scalar
extractToolArg key (Aeson.Object obj) =
  KM.lookup (Key.fromText key) obj >>= scalarFromJSON
extractToolArg _ _ = Nothing
