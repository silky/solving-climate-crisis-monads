{-# language AllowAmbiguousTypes #-}

module Scenario1_PureFunctions.Model where

import "data-default" Data.Default (Default)
import "base" Data.Monoid (Sum)
import "text" Data.Text (Text)
import Types


data Plants  = Plant
data Flowers = Flower

florist :: Plants -> Flowers
florist = const Flower


data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> Coffee
cafe = const Latte




-- | Our initial guess at a  world consists:
--    - Some resources.
--    - A list of businesses.
--    - A collection of business outputs.
data World = World
  { resources  :: Sum Integer
  , businesses :: [SomeBusiness]
  , outputs    :: [BusinessOutput]
  }
  deriving Show


-- | What _is_ a business? Well, in our limited and somewhat disconnected
-- model, we know it's a function a -> b, we can let it have a name; we
-- suppose it has a cost, and we allow ourselves the ability to materialise an
-- input for it at will. This is not particularly physically realistic ...
data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> b)
     )
  => SomeBusiness Text (a -> b)


-- | All we know about a business, really, is it's name.
instance Show SomeBusiness where
  show (SomeBusiness name _) = show $ "business <" <> name <> ">"
