{-# language GADTs #-}
{-# language FlexibleInstances #-}

module Scenario1 where

data World a b = World
  { resources :: Int
  -- , entities  :: [Business a b]
  }

-- liftBusiness :: a -> Business a b -> Business (World () ()) (World () ())
-- liftBusiness a b = undefined
--   where
--     b a

data Runnable = forall a b. Business a b => MkRunnable a b

class Business a b where
  run :: a -> IO b


instance Business a (a -> b) where
  run = undefined

data Plants
data Flowers

-- florist :: Business Plants Flowers
-- florist = undefined



data Beans
data Milk   = Oat
data Coffee = Latte | Cappuccino

-- cafe :: Business (Beans, Milk) Coffee
-- cafe = undefined
