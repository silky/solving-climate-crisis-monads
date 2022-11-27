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


-- class Emissions a where
--   scope1 :: Sum Integer
--   scope2 :: Sum Integer
--   scope3 :: Sum Integer



-- | A Monad
-- type ProductionState = MaybeT (State Emissions)




-- | We can compute a 'Cost' for a thing. We will focus on computing costs for
-- businesses (i.e. things of the form a -> b).
class Emissions a where
  scope1 :: Int
  scope2 :: Int
  scope3 :: Int

totalEmissions :: forall a. Emissions a => Int
totalEmissions = scope1 @a + scope2 @a + scope3 @a


data World = World
  { resources  :: Int
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
     , Emissions (a -> WorldState b)
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

-- instance Emissions Florist where
--   scope1 = 1
--   scope2 = 1
--   scope3 = 0

-- instance Emissions Cafe where
--   scope1 = 1
--   scope2 = 1
--   scope3 = 0

-- instance Emissions GardenCenter where
--   scope1 = 1
--   scope2 = 1
--   scope3 = totalEmissions @Cafe
--          + totalEmissions @Florist


-- produce
--   :: forall input output
--    . Emissions (input -> WorldState output)
--   => output
--   -> WorldState output
-- produce = undefined
--
--


-- | Given some input type `input` and some output type `output`, first decide
-- if we can safely produce the output, by checking the amount of resources we
-- would need, and then produce the output and update the world, if it was
-- indeed possible to do so.
safelyProduce
  :: forall input output
   . Emissions (input -> WorldState output)
  => output
  -> WorldState output
safelyProduce output =
  let c = totalEmissions @(input -> WorldState output)
   in mkWorldState $ \w ->
        if safeToConsume c w
           then (Just output, trackProduction w c output)
           else (Nothing, w)


-- | Update the world with the new product and resource cost.
trackProduction :: World -> Int -> output -> World
trackProduction world@World{resources,outputs} cost' output =
  world { resources = resources - cost'
        , outputs   = BusinessOutput output : outputs
        }


-- | Our safety margin.
safeToConsume :: Int -> World -> Bool
safeToConsume c World{resources} = resources - c >= 70
