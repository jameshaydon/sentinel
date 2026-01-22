-- | Helpers for constructing JSON Schema objects for tool parameters.
module Sentinel.Schema
  ( objectSchema,
    emptyObjectSchema,
    stringProp,
    numberProp,
    boolProp,
    enumProp,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Pre hiding ((.=))

-- | Build an object schema with properties and required fields.
--
-- Example:
--
-- > objectSchema
-- >   [("bookingRef", stringProp "The 6-character booking reference")]
-- >   ["bookingRef"]
objectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
objectSchema props required =
  Aeson.object
    [ "type" .= ("object" :: Text),
      "properties" .= Aeson.Object (KeyMap.fromList [(Key.fromText k, v) | (k, v) <- props]),
      "required" .= required,
      "additionalProperties" .= False
    ]

-- | A string property with a description.
stringProp :: Text -> Aeson.Value
stringProp desc =
  Aeson.object
    [ "type" .= ("string" :: Text),
      "description" .= desc
    ]

-- | Build an empty object schema (no properties).
emptyObjectSchema :: Aeson.Value
emptyObjectSchema = objectSchema [] []

-- | A number property with a description.
numberProp :: Text -> Aeson.Value
numberProp desc =
  Aeson.object
    [ "type" .= ("number" :: Text),
      "description" .= desc
    ]

-- | A boolean property with a description.
boolProp :: Text -> Aeson.Value
boolProp desc =
  Aeson.object
    [ "type" .= ("boolean" :: Text),
      "description" .= desc
    ]

-- | An enum property with allowed values and a description.
enumProp :: [Text] -> Text -> Aeson.Value
enumProp values desc =
  Aeson.object
    [ "type" .= ("string" :: Text),
      "enum" .= values,
      "description" .= desc
    ]
