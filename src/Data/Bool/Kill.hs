{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module        : Data.Bool.Kill
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : not portable
--
-- Strongly-typed booleans
--
-- Example:
--
-- > type TT = TBool "missing" "present"
-- >
-- > isPresent :: TT
-- > isPresent = mkIs $ Proxy @"present"
-- >
-- > is (Proxy @"missing") isPresent == False
module Data.Bool.Kill
  ( TBool (..),
    is,
    isNot,
    mkIs,
    mkTBool,
    ShouldBe,
    ShouldBeVal,

    -- * Reexport
    Proxy (..),
  )
where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

-- | Strongly-typed 'Bool'
data TBool (false :: Symbol) (true :: Symbol)
  = IsFalse
  | IsTrue

instance (KnownSymbol f, KnownSymbol t) => Show (TBool f t) where
  show =
    \case
      IsTrue -> "True (" <> symbolVal (Proxy @t) <> ")"
      IsFalse -> "False (" <> symbolVal (Proxy @f) <> ")"

-- | Check "if 'True'"
is :: forall b f t. ShouldBeVal (ShouldBe (TBool f t) b) => Proxy b -> TBool f t -> Bool
is _ = is' (Proxy @(ShouldBe (TBool f t) b))

-- | Check "if 'False'"
isNot :: forall b f t. ShouldBeVal (ShouldBe (TBool f t) b) => Proxy b -> TBool f t -> Bool
isNot _ = not . is' (Proxy @(ShouldBe (TBool f t) b))

-- | Create 'True'/'False' from its name
mkIs :: forall b f t. ShouldBeVal (ShouldBe (TBool f t) b) => Proxy b -> TBool f t
mkIs _ = mkIs' $ Proxy @(ShouldBe (TBool f t) b)

-- | Create 'TBool' from 'Bool'
mkTBool :: forall f t. Bool -> TBool f t
mkTBool p = if p then IsTrue else IsFalse

data TTrue = TTrue deriving (Show)

data TFalse = TFalse deriving (Show)

type family ShouldBe (tbool :: Type) (a :: Symbol) :: Type where
  ShouldBe (TBool f t) t = TTrue
  ShouldBe (TBool f t) f = TFalse
  ShouldBe (TBool f t) o = TypeError ('Text "Cannot find " ':$$: 'ShowType o ':$$: 'Text " valid choices are " ':$$: 'ShowType f ':$$: 'Text " and " ':$$: 'ShowType t)

class ShouldBeVal a where
  is' :: Proxy a -> TBool t f -> Bool
  mkIs' :: Proxy a -> TBool t f

instance ShouldBeVal TTrue where
  is' _ =
    \case
      IsTrue -> True
      IsFalse -> False
  mkIs' _ = IsTrue

instance ShouldBeVal TFalse where
  is' _ =
    \case
      IsTrue -> False
      IsFalse -> True
  mkIs' _ = IsFalse
