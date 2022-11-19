{-# language AllowAmbiguousTypes #-}

module Types where

import "base" Data.Monoid (Sum)

class Cost a where
  cost :: Sum Integer

data BusinessOutput = forall b. BusinessOutput b

instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"
