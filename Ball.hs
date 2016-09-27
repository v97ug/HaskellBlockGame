module Ball where

import FreeGame
import Direction

data Ball = Ball{
  ballPos :: Vec2,
  dir :: (Horizontal, Vertical),
  radius :: Double
}
