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

import "text" Data.Text (Text)
import "base" Data.Monoid (Sum)
import "data-default" Data.Default (Default)
import Scenario1.Types
  ( Cost (..)
  )
import Control.Monad.Trans.State (State, runState, state)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> WorldState b)
     )
  => SomeBusiness Text (a -> WorldState b)

instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"

instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"

data World = World
  { resources  :: Sum Integer
  , businesses :: [SomeBusiness]
  , outputs    :: [BusinessOutput]
  } deriving Show

data BusinessOutput = forall b. BusinessOutput b

type WorldState = MaybeT (State World)

mkWorldState :: (World -> (Maybe a, World)) -> WorldState a
mkWorldState f = MaybeT (state f)

runWorldState :: WorldState a -> World -> (Maybe a, World)
runWorldState = runState . runMaybeT

execWorldState :: WorldState a -> World -> World
execWorldState m = snd . runWorldState m
