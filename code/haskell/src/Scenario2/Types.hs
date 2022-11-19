module Scenario2.Types
  ( WorldState
  , Cost (..)
  , World (..)
  , SomeBusiness (..)
  , BusinessOutput (..)
  , mkWorldState
  , runWorldState
  , execWorldState
  )
where

import "transformers" Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import "transformers" Control.Monad.Trans.State (State, runState, state)
import "data-default" Data.Default (Default)
import "base" Data.Monoid (Sum)
import "text" Data.Text (Text)
import Types


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
