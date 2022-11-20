{-# language AllowAmbiguousTypes #-}

module Scenario3_EmissionsTracking.Model where

import "base" Data.Monoid (Sum)
import "text" Data.Text (Text)
import "data-default" Data.Default (Default)
import Types

-- Scope 1 - Direct emissions.
-- Scope 2 - Indirect, powering your operation: electricity.
-- Scope 3 - Emissions of your dependencies


-- TODO:
--    - How to model this?


-- class Emissions a where
--   scope1 :: Sum Integer
--   scope2 :: Sum Integer
--   scope3 :: Sum Integer


data World = World
  { resources  :: Sum Integer
  , businesses :: [SomeBusiness]
  , outputs    :: [BusinessOutput]
  }
  deriving Show


data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> b)
     )
  => SomeBusiness Text (a -> b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"
