module Direction where

import FreeGame

data Horizontal = L | R deriving (Eq)
data Vertical = Up | Down deriving (Eq)
data Dir = RightDown | RightUp | LeftUp | LeftDown deriving (Eq)

toRightDown :: Vec2 -> Vec2
toRightDown (V2 x y) = V2 (x+2) (y+2)

toRightUp :: Vec2 -> Vec2
toRightUp (V2 x y) = V2 (x+2) (y-2)

toLeftUp :: Vec2 -> Vec2
toLeftUp (V2 x y) = V2 (x-2) (y-2)

toLeftDown :: Vec2 -> Vec2
toLeftDown (V2 x y) = V2 (x-2) (y+2)

move :: Vec2 -> (Horizontal, Vertical) -> Vec2
move pos dir
  | dir == (R, Down) = toRightDown pos
  | dir == (R, Up) = toRightUp pos
  | dir == (L, Up) = toLeftUp pos
  | dir == (L, Down) = toLeftDown pos
  | otherwise = pos
