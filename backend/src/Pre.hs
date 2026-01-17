{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pre
  ( module Prelude,
    module Data.Default,
    module Data.Functor,
    module Data.Foldable,
    module Data.Traversable,
    module Control.Applicative,
    module Control.Category,
    module Control.Monad,
    module Control.Lens,
    module Control.Monad.Except,
    module Data.Maybe,
    Generic,
    Generic1,
    Generically (..),
    Generically1 (..),
    Text,
    ShortText,
    Map,
    Set,
    Scientific,
    FromJSON,
    FromJSON1,
    ToJSON,
    ToJSON1,
    Show1,
    Eq1,
    module Data.Functor.Compose,
    module Control.Monad.Reader,
    headMay,
    module Data.Proxy,
    putDispLn,
    putDocLn,
    renderDoc,
    renderDocPlain,
    Disp (..),
    Ann (..),
    IdKind (..),
    Severity (..),
    annId,
    annEntityType,
    annField,
    module Prettyprinter,
    pPrint,
    pTraceShow,
    Void,
    absurd,
    upcast,

    -- * NonEmpty
    NonEmpty (..),

    -- * List
    collectDuplicatesOn,
    discard,

    -- * Set
    setMapMaybe,

    -- * Errors
    (??:),
    (??%),

    -- * Strings
    IsString (..),
    lazyEncodeUtf8,

    -- * Functor utils
    unzipWith,
    second,

    -- * Comonad
    extract,

    -- * Default
    withDefault,

    -- * Safe functions
    safeMinimum,
    safeMaximum,

    -- * Applicative
    zipWithT,

    -- * Folding
    foldMapM,

    -- * Constraints
    Unconstrained,

    -- * Doc helpers
    quoted,
  )
where

import Control.Applicative
import Control.Category
import Control.Comonad (extract)
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, FromJSON1, ToJSON, ToJSON1)
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as LazyBS
import Data.Default
import Data.Foldable
import Data.Foldable1 qualified as Foldable1
import Data.Functor
import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Compose
import Data.Generics.Labels ()
import Data.Generics.Product (upcast)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid (Ap (Ap, getAp))
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText
import Data.Text.Short (ShortText)
import Data.Traversable
import Data.Void
import Debug.Pretty.Simple (pTraceShow)
import GHC.Generics (Generic, Generic1)
import Generic.Data (Generically (..), Generically1 (..))
import Safe (headMay)
import Text.Pretty.Simple (pPrint)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as Term
import Prettyprinter.Render.Text (renderStrict)
import Prelude hiding (id, unzip, (.))

-- | Semantic annotation type for pretty-printing validation messages.
-- These annotations enable styled output (colors, bold, etc.) in terminals.
data Ann
  = -- | An identifier value with its kind
    AnnId IdKind
  | -- | An entity type mention (e.g., "Logical Buffer", "Abstract Task")
    AnnEntityType
  | -- | A field name mention (e.g., "inputBuffers")
    AnnField
  | -- | A severity marker for validation messages
    AnnSeverity Severity
  deriving stock (Eq, Show)

-- | Severity level for validation issues
-- | 🔰Undefined: Unable to provide further validation (unresolvable references)
-- | ⛔Error: Violation disables the scenario from running properly
-- | ⚠️Warning: Violation may cause issues but not prohibited
data Severity = Undefined | Error | Warning
  deriving stock (Eq, Show)

-- | The kind of identifier, for differentiated styling
data IdKind
  = IdLogicalBuffer
  | IdAbstractTask
  | IdLogicalResourcePool
  | IdPhysicalResourcePool
  | IdAttribute
  | IdResourceType
  deriving stock (Eq, Show)

-- | Annotate an identifier with its kind
annId :: IdKind -> Doc Ann -> Doc Ann
annId = annotate . AnnId

-- | Annotate an entity type name
annEntityType :: Doc Ann -> Doc Ann
annEntityType = annotate AnnEntityType

-- | Annotate a field name
annField :: Doc Ann -> Doc Ann
annField = annotate AnnField

-- | Convert semantic annotations to ANSI terminal styles
annToStyle :: Ann -> AnsiStyle
annToStyle = \case
  AnnId idKind -> case idKind of
    IdLogicalBuffer -> Term.color Term.Cyan
    IdAbstractTask -> Term.color Term.Yellow
    IdLogicalResourcePool -> Term.color Term.Green
    IdPhysicalResourcePool -> Term.color Term.Green
    IdAttribute -> Term.color Term.Magenta
    IdResourceType -> Term.color Term.Blue
  AnnEntityType -> Term.bold
  AnnField -> Term.italicized
  AnnSeverity sev -> case sev of
    Undefined -> Term.color Term.Cyan <> Term.bold
    Error -> Term.color Term.Red <> Term.bold
    Warning -> Term.color Term.Yellow <> Term.bold

-- | Render a Doc with ANSI styling to Text
renderDoc :: Doc Ann -> Text
renderDoc = Term.renderStrict . reAnnotateS annToStyle . layoutPretty defaultLayoutOptions

-- | Render a Doc to plain Text without any styling
renderDocPlain :: Doc ann -> Text
renderDocPlain = renderStrict . layoutPretty defaultLayoutOptions

-- | Print a Doc with ANSI styling to stdout, followed by a newline
putDocLn :: Doc Ann -> IO ()
putDocLn doc = Term.putDoc (reAnnotate annToStyle doc) >> putStrLn ""

putDispLn :: (Disp a) => a -> IO ()
putDispLn = putDocLn . disp

-- | A class for things that have a canonical representation as a sequence of
-- characters.
class Disp a where
  disp :: a -> Doc Ann

instance Disp Severity where
  disp sev = annotate (AnnSeverity sev) $ case sev of
    Undefined -> "[UNDEFINED]"
    Error -> "[ERROR]"
    Warning -> "[WARNING]"

infixr 0 ??:

-- | Lift a 'Maybe' value into a 'MonadError' monad.
(??:) :: (MonadError e m) => Maybe a -> e -> m a
(??:) x_ e = maybe (throwError e) pure x_

infixr 0 ??%

-- | Lift an Either value into a 'MonadError' monad.
(??%) :: (MonadError e' m) => Either e a -> (e -> e') -> m a
(??%) x f = case x of
  Left e -> throwError (f e)
  Right y -> pure y

lazyEncodeUtf8 :: Text.StrictText -> LazyBS.ByteString
lazyEncodeUtf8 = LazyText.encodeUtf8 . LazyText.fromStrict

unzipWith :: (Functor f) => (t -> (a, b)) -> f t -> (f a, f b)
unzipWith f xs = Data.Functor.unzip $ f <$> xs

collectDuplicatesOn :: (Ord b) => (a -> b) -> [a] -> [a]
collectDuplicatesOn k =
  concat
    . filter moreThanOne
    . groupOn k
    . sortOn k
  where
    moreThanOne (_ : _ : _) = True
    moreThanOne _ = False

discard :: (a -> Bool) -> [a] -> [a]
discard p = filter (not . p)

withDefault :: (Default a) => Maybe a -> a
withDefault = fromMaybe def

safeMinimum, safeMaximum :: (Ord a) => a -> [a] -> a
safeMinimum m xs = maybe m Foldable1.minimum (NE.nonEmpty xs)
safeMaximum m xs = maybe m Foldable1.maximum (NE.nonEmpty xs)

zipWithT :: (Applicative f, Traversable t) => (t a -> b) -> t (f a) -> f b
zipWithT f xs = f <$> sequenceA xs

-- | Monadic version of 'foldMap'
foldMapM :: (Applicative m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = getAp <$> foldMap (Ap . f)

setMapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = Set.fromList . mapMaybe p . toList

class Unconstrained a

instance Unconstrained a

-- | Wrap a Doc in single quotes for display.
quoted :: Doc Ann -> Doc Ann
quoted d = "'" <> d <> "'"
