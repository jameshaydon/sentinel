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
    module Sentinel.Pretty,
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
import Prettyprinter
import Safe (headMay)
import Sentinel.Pretty
import Text.Pretty.Simple (pPrint)
import Prelude hiding (id, unzip, (.))

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
