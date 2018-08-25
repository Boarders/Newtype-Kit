{-# LANGUAGE TypeFamilies #-}

module Newtype where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Profunctor ()
import Data.Monoid (Sum(..), Product(..))
import Data.Functor.Compose (Compose(

class Newtype n where
  type Old n
  pack   :: Old n -> n
  unpack :: n     -> Old n

instance Newtype (Const a b) where
  type Old (Const a b) = a
  pack   = Const
  unpack = getConst

instance Newtype (Identity a) where
  type Old (Identity a) = a
  pack   = Identity
  unpack = runIdentity

instance Newtype (Sum a) where
  type Old (Sum a) = a
  pack   = Sum
  unpack = getSum

instance Newtype (Product a) where
  type Old (Product a) = a
  pack = Product
  unpack = getProduct

newly ::
     (Newtype n, Newtype n')
  => (Old n -> Old n')
  -> n -> n'
newly f = pack . f . unpack

oldly ::
     (Newtype n, Newtype n')
  => (n -> n')
  -> Old n -> Old n'
oldly f = unpack . f . pack

ala ::
     (Newtype n, Newtype n', Functor f, Functor g)
  => (f n -> g n') -> (Old n -> n)
  -> (f (Old n) -> g (Old n'))
ala hof _ = (fmap unpack) . hof . (fmap pack)

foldMapNew ::
    (Monoid m, Traversable t)
 => (a -> m)
 -> t a -> m
foldMapNew = traverse `ala` Const

fmapNew ::
    (Traversable f)
 => (a -> b)
 -> f a -> f b
fmapNew = traverse `ala` Identity

sumNew ::
    (Traversable t, Num n)
 => t n -> n
sumNew = traverse `ala` Const `ala` Sum $ id

productNew ::
    (Traversable t, Num n)
  => t n -> n
productNew = traverse `ala` Const `ala` Product $ id
