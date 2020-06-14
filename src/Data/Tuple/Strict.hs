{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
-- This module re-exports everything from the 'Data.Strict.Tuple' module as
-- well as the following utilities and instances:
--
-----------------------------------------------------------------------------

module Data.Tuple.Strict (
    module Data.Strict.Tuple
) where

import           Data.Strict.Tuple

#if MIN_VERSION_lens(4,0,0)
import           Control.Lens.At     (Index)
import           Control.Lens.Each   (Each(..))
#else
import           Control.Lens.Each   (Index, Each(..))
#endif

import           Control.Lens.Iso    (Strict (..), Swapped (..), iso)
import           Control.Lens.Indexed (indexed)
import           Control.Lens.Operators ((<&>))
import           Control.Lens.Tuple  (Field1 (..), Field2 (..))
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Test.QuickCheck     (Arbitrary (..))

#if __HADDOCK__
import Data.Tuple ()
#endif

-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Pair a b) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (a, b) (Pair a b) where
  strict = iso toStrict toLazy

instance Field1 (Pair a b) (Pair a' b) a a' where
  _1 k (a :!: b) = indexed k (0 :: Int) a <&> \a' -> (a' :!: b)

instance Field2 (Pair a b) (Pair a b') b b' where
  _2 k (a :!: b) = indexed k (1 :: Int) b <&> \b' -> (a :!: b')

instance Swapped Pair where
  swapped = iso swap swap

type instance Index (Pair a b) = Int

#if MIN_VERSION_lens(4,0,0)
instance (a~a', b~b') => Each (Pair a a') (Pair b b') a b where
  each f ~(a :!: b) = (:!:) <$> f a <*> f b
  {-# INLINE each #-}
#else
instance (Applicative f, a~a', b~b') => Each f (Pair a a') (Pair b b') a b where
  each f (a :!: b) = (:!:) <$> indexed f (0::Int) a <*> indexed f (1::Int) b
  {-# INLINE each #-}
#endif
