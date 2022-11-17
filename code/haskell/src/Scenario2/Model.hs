module Scenario2.Model where

import Scenario2.Types

data Plants  = Plant
data Flowers = Flower

safeToConsume :: World -> Bool
safeToConsume World{resources} = resources > 70

florist :: Plants -> WorldState Flowers
florist = const . WorldState $ \w ->
    if safeToConsume w
       then (Just Flower, w)
       else (Nothing,     w)


-- m :: Maybe Flowers
-- m = do
--   let f = florist Plant
--       m = runWorld w f
  -- f2 <- florist Plant
  -- pure $ f2
