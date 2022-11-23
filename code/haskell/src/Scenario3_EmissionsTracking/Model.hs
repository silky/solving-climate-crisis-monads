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


data BusinessInfo = BusinessInfo
  { scope1 :: Sum Integer
  , scope2 :: Sum Integer
  , scope3 :: Sum Integer
  }



-- class Emissions a where
--   scope1 :: Sum Integer
--   scope2 :: Sum Integer
--   scope3 :: Sum Integer


-- data World = W
--   { resources :: [Resource]
--   , waste     :: [Waste]
--   }


-- materialise :: Default a => Resource -> a
-- regenerate  :: Waste -> Resource
-- consumer    :: Resource -> Waste


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
     , Cost (a -> b)
     )
  => SomeBusiness Text (a -> b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


data Plants  = Plant
data Flowers = Flower


florist :: Plants -> WorldState Flowers
florist = undefined
