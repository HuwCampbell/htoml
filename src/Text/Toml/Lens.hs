{-# LANGUAGE RankNTypes #-}
module Text.Toml.Lens (
    _VTable
  , _VTArray
  , _VString
  , _VInteger
  , _VFloat
  , _VBoolean
  , _VDatetime
  , _VArray
  ) where


import           Control.Applicative
import           Control.Monad
import           Data.Int            (Int64)
import           Data.Profunctor
import           Data.Text
import           Data.Time.Clock     (UTCTime)
import           Prelude

import           Text.Toml.Types

_VTable :: Prism' Node Table
_VTable =
  prism  VTable $ \n -> case n of
    VTable v -> pure v
    _ -> Left n

_VTArray :: Prism' Node [Table]
_VTArray =
  prism  VTArray $ \n -> case n of
    VTArray v -> pure v
    _ -> Left n

_VString :: Prism' Node Text
_VString =
  prism  VString $ \n -> case n of
    VString v -> pure v
    _ -> Left n

_VInteger :: Prism' Node Int64
_VInteger =
  prism VInteger $ \n -> case n of
    VInteger v -> pure v
    _ -> Left n

_VFloat :: Prism' Node Double
_VFloat =
  prism VFloat $ \n -> case n of
    VFloat v -> pure v
    _ -> Left n

_VBoolean :: Prism' Node Bool
_VBoolean =
  prism VBoolean $ \n -> case n of
    VBoolean v -> pure v
    _ -> Left n

_VDatetime :: Prism' Node UTCTime
_VDatetime =
  prism VDatetime $ \n -> case n of
    VDatetime v -> pure v
    _ -> Left n

_VArray :: Prism' Node [Node]
_VArray =
  prism VArray $ \n -> case n of
    VArray v -> pure v
    _ -> Left n



-- | Definititions of Prism and helper functions.
--   Defining these ourselves allows us to not depend on Control.Lens
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

prism :: (Choice p, Applicative f) => (b -> t) -> (s -> Either t a) -> p a (f b) -> p s (f t)
prism bt seta = dimap seta (either pure (fmap bt)) . right'
