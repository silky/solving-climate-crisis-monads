{-# language AllowAmbiguousTypes #-}

module Scenario2_SimpleMonad.Model where

import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
import "base" Data.Monoid (Sum (..))
import "text" Data.Text (Text)
import Types


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
           then (Just output, w { resources = resources w - c
                                , outputs   = BusinessOutput output : outputs w
                                })
           else (Nothing, w)

-- | Our safety margin.
safeToConsume :: Sum Integer -> World -> Bool
safeToConsume c World{resources} = resources - c >= 70


-- A variation on our regular business model; we now return WorldState's and
-- only produce things if we can do so safely.

data Plants  = Plant
data Flowers = Flower


florist :: Plants -> WorldState Flowers
florist = const $ safelyProduce @Plants Flower


instance Cost (Plants -> WorldState Flowers) where
  cost = Sum 1


data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino


cafe :: (Beans, Milk) -> WorldState Coffee
cafe = const $ safelyProduce @(Beans, Milk) Latte


instance Cost ((Beans, Milk) -> WorldState Coffee) where
  cost = Sum 1


gardenCenter :: (Plants, (Beans, Milk)) -> WorldState (Flowers, Coffee)
gardenCenter (p, bm) = do
  f <- florist p
  c <- cafe bm
  pure (f, c)


instance Cost ((Plants, (Beans, Milk)) -> WorldState (Flowers, Coffee)) where
  cost = cost @(Plants -> WorldState Flowers)
       + cost @((Beans, Milk) -> WorldState Coffee)


data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> WorldState b)
     )
  => SomeBusiness Text (a -> WorldState b)


instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"


data World = World
  { resources  :: Sum Integer
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
