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
-- This module re-exports everything from the 'Data.Strict.Maybe' module as
-- well as the following utilities and instances:
--
-----------------------------------------------------------------------------

module Data.Maybe.Strict (
     module Data.Strict.Maybe
   , _Just
   , _Nothing
) where

import           Data.Strict.Maybe
import           Prelude             hiding (Maybe (..), maybe)
import qualified Prelude             as L

import           Control.Lens.Iso    (Strict (..), iso)
import           Control.Lens.Prism  (Prism, Prism', prism, prism')
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Test.QuickCheck     (Arbitrary (..))


-- aeson
instance ToJSON a => ToJSON (Maybe a) where
  toJSON = toJSON . toLazy

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (L.Maybe a) (Maybe a) where
  strict = iso toStrict toLazy

-- | Analogous to 'Control.Lens.Prism._Just' in "Control.Lens.Prism"
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

-- | Analogous to 'Control.Lens.Prism._Nothing' in "Control.Lens.Prism"
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (L.Just ()) (const L.Nothing)
