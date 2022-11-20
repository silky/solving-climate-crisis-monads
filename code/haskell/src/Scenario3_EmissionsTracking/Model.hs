module Scenario3_EmissionsTracking.Model where

import "base" Data.Monoid (Sum)
import "text" Data.Text (Text)
import "data-default" Data.Default (Default)
import Types

-- Scope 1 - Direct emissions.
-- Scope 2 - Indirect, powering your operation: electricity.
-- Scope 3 - Emissions of your dependencies

data Resource = Resource
data Waste    = Waste

instance Cost (Resource -> a) where
  cost = 1


data World = World
  { resources  :: [Resource]
  , waste      :: [Waste]
  , businesses :: [SomeBusiness]
  }
  -- deriving Show


data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> b)
     )
  => SomeBusiness Text (a -> b)
