{-# language AllowAmbiguousTypes #-}

module Scenario1.Types where

import "text" Data.Text (Text)
import "base" Data.Monoid (Sum)
import "data-default" Data.Default (Default)


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
-- model, we know it's a function a -> b, we know it has a cost, and we allow
-- ourselves the ability to materialise an input for it at will. This is not
-- particularly physically realistic ...
data SomeBusiness
  = forall a b
   . ( Default a
     , Cost (a -> b)
     )
  => SomeBusiness Text (a -> b)


-- | We don't know anything about business outputs.
data BusinessOutput = forall b. BusinessOutput b


-- | What do businesses produce in this world? Why, Arbitrary Business Output,
-- of course!
instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"


-- | All we know about a business, really, is it's name.
instance Show SomeBusiness where
  show (SomeBusiness t _) = show $ "function <" <> t <> ">"


-- | We can compute a 'Cost' for a thing. We will focus on computing costs for
-- businesses (i.e. things of the form a -> b). We're going to want to add
-- these up at some point, so we'll make it a `Sum` so that's a bit easier to
-- do.
class Cost a where
  cost :: Sum Integer
