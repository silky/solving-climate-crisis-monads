module Scenario2.Types where

-- import "text" Data.Text (Text)
import "base" Data.Monoid (Sum)
-- import "data-default" Data.Default (Default)
import "base" Control.Monad (liftM, ap)


-- Marco says: Try also `MaybeT (State s)`
newtype WorldState a = WorldState { runWorld :: World -> (Maybe a, World) }

instance Functor WorldState where
  fmap = liftM

instance Applicative WorldState where
  pure  = return
  (<*>) = ap

-- TODO: Maybe do the safety checking in here?

instance Monad WorldState where
  p >>= k = WorldState $ \w ->
    let (mx, w') = runWorld p w
     in case mx of
          Nothing -> (Nothing, w')
          Just x  -> runWorld (k x) w'

data World = World
  { resources  :: Sum Integer
  }
  deriving Show


