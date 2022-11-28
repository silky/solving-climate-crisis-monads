{-# language AllowAmbiguousTypes #-}

module Scenario3_EmissionsTracking.Model where

import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
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


data Emissions = Emissions
  { scope1 :: Int
  , scope2 :: Int
  , scope3 :: Int
  }
  deriving Show


noEmissions :: Emissions
noEmissions = Emissions 0 0 0


totalEmissions :: Emissions -> Int
totalEmissions Emissions{scope1, scope2, scope3} = scope1 + scope2 + scope3


type ProductionState = State Emissions

mkProductionState :: (Emissions -> (a, Emissions)) -> ProductionState a
mkProductionState = state


runProductionState :: ProductionState a -> Emissions -> (a, Emissions)
runProductionState = runState


execProductionState :: ProductionState a -> Emissions -> Emissions
execProductionState m = snd . runProductionState m


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
     -- , Cost (a -> WorldState b)
     )
  => SomeBusiness Text (a -> b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


produce :: Int -> Int -> a -> ProductionState a
produce s1 s2 output
  = state $ \p ->
      ( output
      , Emissions
          { scope1 = s1
          , scope2 = s2
          , scope3 = totalEmissions p
          }
      )


runBusiness :: ProductionState a -> WorldState a
runBusiness p = do
  undefined


florist :: Plants -> ProductionState Flowers
florist _ = produce 1 0 Flower


cafe :: (Beans, Milk) -> ProductionState Coffee
cafe _ingredients = produce 1 0 Latte


gardenCenter :: ((Beans, Milk), Plants) -> ProductionState (Flowers, Coffee)
gardenCenter (bm, p) = do
  c <- cafe bm
  f <- florist p
  produce 0 0 (f, c)


-- -- | Given some input type `input` and some output type `output`, first decide
-- -- if we can safely produce the output, by checking the amount of resources we
-- -- would need, and then produce the output and update the world, if it was
-- -- indeed possible to do so.
-- safelyProduce
--   :: forall input output
--    . Emissions (input -> WorldState output)
--   => output
--   -> ProductionState output
-- safelyProduce output =
--   let c = totalEmissions @(input -> WorldState output)
--    in mkWorldState $ \w ->
--         if safeToConsume c w
--            then (Just output, trackProduction w c output)
--            else (Nothing, w)


-- -- | Update the world with the new product and resource cost.
-- trackProduction :: World -> Int -> output -> World
-- trackProduction world@World{resources,outputs} cost' output =
--   world { resources = resources - cost'
--         , outputs   = BusinessOutput output : outputs
--         }


-- -- | Our safety margin.
-- safeToConsume :: Int -> World -> Bool
-- safeToConsume c World{resources} = resources - c >= 70
