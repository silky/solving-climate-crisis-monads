{-# language AllowAmbiguousTypes #-}

module Types where

import "aeson" Data.Aeson (Object)
import "base" Data.Monoid (Sum)


-- | We can compute a 'Cost' for a thing. We will focus on computing costs for
-- businesses (i.e. things of the form a -> b). We're going to want to add
-- these up at some point, so we'll make it a `Sum` so that's a bit easier to
-- do.
class Cost a where
  cost :: Sum Integer


-- | What do businesses produce in this world? Why, Arbitrary Business Output,
-- of course!
data BusinessOutput = forall b. BusinessOutput b


instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"


class SomeWorld w where
  someResources  :: w -> Sum Integer
  otherPropertes :: w -> Object
  spinWorld      :: w -> w
