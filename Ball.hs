module Ball where

import FreeGame
import Direction

data Ball = Ball{
  ballPos :: Vec2,
  dir :: (Horizontal, Vertical),
  radius :: Double
}

moveBall :: Ball -> Vec2
moveBall ball
  | d == (R, Down) = toRightDown pos
  | d == (R, Up) = toRightUp pos
  | d == (L, Up) = toLeftUp pos
  | d == (L, Down) = toLeftDown pos
  | otherwise = pos
  where
    d = dir ball
    pos = ballPos ball
