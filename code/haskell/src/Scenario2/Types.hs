module Scenario2.Types
  ( WorldState (..)
  , Cost (..)
  , World (..)
  , SomeBusiness (..)
  , BusinessOutput (..)
  )
where

import "text" Data.Text (Text)
import "base" Data.Monoid (Sum)
import "data-default" Data.Default (Default)
import "base" Control.Monad (ap, liftM)
import Scenario1.Types
  ( Cost (..)
  )
import Control.Monad.Trans.State (State, runState)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> WorldState b)
     )
  => SomeBusiness Text (a -> WorldState b)

-- | What do businesses produce in this world? Why, Arbitrary Business Output,
-- of course!
instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"

-- | All we know about a business, really, is it's name.
instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"

data World = World
  { resources  :: Sum Integer
  , businesses :: [SomeBusiness]
  , outputs    :: [BusinessOutput]
  } deriving Show

-- | We don't know anything about business outputs.
data BusinessOutput = forall b. BusinessOutput b

-- Marco says: Try also `MaybeT (State s)`
-- type X a = MaybeT (State a)
newtype WorldState a = WorldState { runWorld :: World -> (Maybe a, World) }

instance Functor WorldState where
  fmap = liftM

instance Applicative WorldState where
  pure  = return
  (<*>) = ap

instance Monad WorldState where
  return x = WorldState (\w -> (Just x, w))
  p >>= k = WorldState $ \w ->
    let (mx, w') = runWorld p w
     in case mx of
          Nothing -> (Nothing, w')
          Just x  -> runWorld (k x) w'

