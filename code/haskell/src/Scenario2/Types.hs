module Scenario2.Types
  ( WorldState
  , World (..)
  , Cost (..)
  )
  where

import "base" Control.Monad (ap, liftM)
import Scenario1.Types (World (..), Cost (..))

-- Marco says: Try also `MaybeT (State s)`
newtype WorldState a = WorldState { runWorld :: World -> (Maybe a, World) }

instance Functor WorldState where
  fmap = liftM

instance Applicative WorldState where
  pure  = return
  (<*>) = ap

instance Monad WorldState where
  p >>= k = WorldState $ \w ->
    let (mx, w') = runWorld p w
     in case mx of
          Nothing -> (Nothing, w')
          Just x  -> runWorld (k x) w'
