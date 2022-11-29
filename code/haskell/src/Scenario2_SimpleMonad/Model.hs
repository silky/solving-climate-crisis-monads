{-# language AllowAmbiguousTypes #-}
{-# language InstanceSigs        #-}

module Scenario2_SimpleMonad.Model
  ( Flowers (..)
  , Plants (..)
  , WorldState
  , World (..)
  , SomeBusiness (..)
  , Beans (..)
  , Milk (..)
  , Coffee (..)
  , Cost (..)
  , mkWorldState -- Comment out to prevent the rogueFlorist from operating.
  , trackProduction
  , execWorldState
  , evalWorldState
  , runWorldState
  , florist
  , cafe
  , gardenCenter
  )
where

import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
import "text" Data.Text (Text)
import Types


-- | We can compute a 'Cost' for a thing. We will focus on computing costs for
-- businesses (i.e. things of the form a -> b).
class Cost a where
  cost :: Int


-- | Given some input type `input` and some output type `output`, first decide
-- if we can safely produce the output, by checking the amount of resources we
-- would need, and then produce the output and update the world, if it was
-- indeed possible to do so.
safelyProduce
  :: forall input output
   . Cost (input -> WorldState output)
  => output
  -> WorldState output
safelyProduce output =
  let c = cost @(input -> WorldState output)
   in mkWorldState $ \w ->
        if safeToConsume c w
           then (Just output, trackProduction w c)
           else (Nothing, w)


-- | Update the world by consuming the resources.
trackProduction :: World -> Int -> World
trackProduction world@World{resources} cost' =
  world { resources = resources - cost' }


-- | Our safety margin.
safeToConsume :: Int -> World -> Bool
safeToConsume c World{resources} = resources - c >= 70


-- A variation on our regular business model; we now return WorldState's and
-- only produce things if we can do so safely.

florist :: Plants -> WorldState Flowers
florist = const $ safelyProduce @Plants Flower


instance Cost (Plants -> WorldState Flowers) where
  cost = 1


cafe :: (Beans, Milk) -> WorldState Coffee
cafe = const $ safelyProduce @(Beans, Milk) Latte


instance Cost ((Beans, Milk) -> WorldState Coffee) where
  cost = 1


data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> WorldState b)
     )
  => SomeBusiness Text (a -> WorldState b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


data World = World
  { resources  :: Int
  , businesses :: [SomeBusiness]
  , outputs    :: [BusinessOutput]
  } deriving Show


type WorldState = MaybeT (State World)


mkWorldState :: (World -> (Maybe a, World)) -> WorldState a
mkWorldState = MaybeT . state


runWorldState :: WorldState a -> World -> (Maybe a, World)
runWorldState = runState . runMaybeT


execWorldState :: WorldState a -> World -> World
execWorldState m = snd . runWorldState m


evalWorldState :: WorldState a -> World -> Maybe a
evalWorldState m = fst . runWorldState m


-- We can also define a "combined" business (albeit in a somewhat unergonomic way):

gardenCenter :: (Plants, (Beans, Milk)) -> WorldState (Flowers, Coffee)
gardenCenter (p, bm) = do
  f <- florist p
  c <- cafe bm
  pure (f, c)


instance Cost ((Plants, (Beans, Milk)) -> WorldState (Flowers, Coffee)) where
  cost = cost @(Plants -> WorldState Flowers)
       + cost @((Beans, Milk) -> WorldState Coffee)


-- Summary of notes about this implementation:
--
--  - There's choices about how much flexibility to allow people when
--    constructing their businesses; do they get to check the threshold
--    themselves? Can they bypass our checks?
--
--  - We could get the cost estimates wrong. But at least they aren't orphan
--    instances now; we need them in the model.
--
--  - Combined businesses are fun.
--
--  TODO: Add some more insightful points here.
