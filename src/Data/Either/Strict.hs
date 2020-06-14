{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- | Copyright :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module re-exports everything from the 'Data.Strict.Either' module as
-- well as the following utilities and instances:
--
-----------------------------------------------------------------------------

module Data.Either.Strict (
    module Data.Strict.Either
  , _Left
  , _Right
) where

import           Data.Strict.Either
import           Prelude             hiding (Either (..), either)
import qualified Prelude             as L

import           Control.Lens.Iso    (Strict (..), Swapped (..), iso)
import           Control.Lens.Prism  (Prism, prism)
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Test.QuickCheck     (Arbitrary (..))


-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (L.Either a b) (Either a b) where
  strict = iso toStrict toLazy

instance Swapped Either where
  swapped = either Right Left `iso` either Right Left

-- | Analogous to 'Control.Lens.Prism._Left' in "Control.Lens.Prism".
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either L.Right (L.Left . Right)

-- | Analogous to 'Control.Lens.Prism._Right' in "Control.Lens.Prism".
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (L.Left . Left) L.Right
