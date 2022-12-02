{-# language AllowAmbiguousTypes #-}

module Scenario3_EmissionsTracking.Model where

import "base" Control.Monad (forM)
import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
import "text" Data.Text (Text)
import Types


-- Scope 1 - Direct emissions.
-- Scope 2 - Indirect, powering your operation: electricity.
-- Scope 3 - Emissions of your dependencies

-- <https://www.climatepartner.com/en/climate-action-insights/reducing-scope-emissions>

-- gardenCenter:
--    - florist -> scope 1+2+3 => scope 3
--    - cafe    -> scope 1+2+3 => scope 3


data Emissions = Emissions
  { scope1 :: Int
  , scope2 :: Int
  , scope3 :: Int
  }
  deriving Show


data RegenerativeAction
  = DoNothing
  | PlantTree


noEmissions :: Emissions
noEmissions = Emissions 0 0 0


totalEmissions :: Emissions -> Int
totalEmissions Emissions{scope1, scope2, scope3} = scope1 + scope2 + scope3


type ProductionState = State Emissions
-- type ProductionState = State (Emissions, [RegenerativeAction])

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
     )
  => SomeBusiness Text (a -> ProductionState b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


produce :: Int -> Int -> a -> ProductionState a
produce s1 s2 output
  = state $ \previousEmissions ->
      ( output
      , Emissions
          { scope1 = s1
          , scope2 = s2
          , scope3 = totalEmissions previousEmissions
          }
      )


runBusiness :: ProductionState output -> WorldState output
runBusiness p = do
  let (output, emissions) = runProductionState p noEmissions
      totalCost = totalEmissions emissions
   in mkWorldState $ \w ->
        if safeToConsume totalCost w
           then (Just output, trackProduction w totalCost output)
           else (Nothing, w)


florist :: Plants -> ProductionState Flowers
florist _ = produce 1 0 Flower


cafe :: (Beans, Milk) -> ProductionState Coffee
cafe _ingredients = produce 1 0 Latte


gardenCenter :: ((Beans, Milk), Plants) -> ProductionState (Flowers, Coffee)
gardenCenter (bm, p) = do
  c <- cafe bm
  f <- florist p
  produce 0 0 (f, c)


-- Cost is now computed "dynamically"; per evaluation.
megaGardenCenter :: ([(Beans, Milk)], [Plants]) -> ProductionState ([Flowers], [Coffee])
megaGardenCenter (bms, ps) = do
  fs <- forM bms cafe
  cs <- forM ps florist
  produce 0 0 (cs, fs)


-- | Update the world with the new product and resource cost.
trackProduction :: World -> Int -> output -> World
trackProduction world@World{resources,outputs} cost' output =
  world { resources = resources - cost'
        , outputs   = BusinessOutput output : outputs
        }


-- | Our safety margin.
safeToConsume :: Int -> World -> Bool
safeToConsume c World{resources} = resources - c >= 70
