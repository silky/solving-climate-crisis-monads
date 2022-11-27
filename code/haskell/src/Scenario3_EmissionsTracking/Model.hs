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

totalEmissions :: forall a. Emissions a => Sum Integer
totalEmissions = scope1 @a + scope2 @a + scope3 @a


-- | A Monad
-- type ProductionState = MaybeT (State Emissions)


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


type Florist = Plants -> WorldState Flowers


florist :: Florist
florist = undefined


type Cafe = (Beans, Milk) -> WorldState Coffee


cafe :: Cafe
cafe = undefined


type GardenCenter = ((Beans, Milk), Plants) -> WorldState (Flowers, Coffee)


gardenCenter :: GardenCenter
gardenCenter (bm, p) = do
  f <- florist p
  c <- cafe bm
  pure (f, c)


-- TODO: I think this is bad. We want this to be computed dynmaically from the
-- operation of the business; but as-is it is hard-coded.

instance Emissions Florist where
  scope1 = 1
  scope2 = 1
  scope3 = 0

instance Emissions Cafe where
  scope1 = 1
  scope2 = 1
  scope3 = 0

instance Emissions GardenCenter where
  scope1 = 1
  scope2 = 1
  scope3 = totalEmissions @Cafe
         + totalEmissions @Florist


produce
  :: forall input output
   . Emissions (input -> WorldState output)
  => output
  -> WorldState output
produce = undefined
