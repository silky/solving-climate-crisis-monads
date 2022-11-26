{-# language AllowAmbiguousTypes #-}

module Scenario3_EmissionsTracking.Model where

import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
import "base" Data.Monoid (Sum)
import "text" Data.Text (Text)
import Simulation
import Types


-- Scope 1 - Direct emissions.
-- Scope 2 - Indirect, powering your operation: electricity.
-- Scope 3 - Emissions of your dependencies


-- TODO:
--    - How to model this?
--    - Idea: Inside the WorldState, we could have a `Business` monad, and
--      then capture the production details as we go about production.  This
--      could then be bunled up into the cost of producing that thing.


-- gardenCenter:
--    - florist -> scope 1+2+3 => scope 3
--    - cafe    -> scope 1+2+3 => scope 3
--    - + it's own outputs


class Emissions a where
  scope1 :: Sum Integer
  scope2 :: Sum Integer
  scope3 :: Sum Integer


-- | A Monad
-- type ProductionState = MaybeT (State Emissions)


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


type WorldState = MaybeT (State World)


mkWorldState :: (World -> (Maybe a, World)) -> WorldState a
mkWorldState = MaybeT . state


runWorldState :: WorldState a -> World -> (Maybe a, World)
runWorldState = runState . runMaybeT


execWorldState :: WorldState a -> World -> World
execWorldState m = snd . runWorldState m

data SomeBusiness
  = forall a b
   . ( Default a
     , Emissions (a -> b)
     )
  => SomeBusiness Text (a -> b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


-- florist :: Plants -> WorldState Flowers
-- florist = const $ produce @Plants Flower


-- instance Emissions (Plants -> WorldState Flowers) where
--   scope1 = 1
--   scope2 = 0
--   scope3 = 0


-- cafe :: (Beans, Milk) -> WorldState Coffee
-- cafe = const $ safelyProduce @(Beans, Milk) Latte


produce
  :: forall input output
   . Emissions (input -> WorldState output)
  => output
  -> WorldState output
produce = undefined
