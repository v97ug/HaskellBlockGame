module Direction where

import FreeGame

data Dir = RightDown | RightUp | LeftUp | LeftDown deriving (Eq)

toRightDown :: Vec2 -> Vec2
toRightDown (V2 x y) = V2 (x+2) (y+2)

toRightUp :: Vec2 -> Vec2
toRightUp (V2 x y) = V2 (x+2) (y-2)

toLeftUp :: Vec2 -> Vec2
toLeftUp (V2 x y) = V2 (x-2) (y-2)

toLeftDown :: Vec2 -> Vec2
toLeftDown (V2 x y) = V2 (x-2) (y+2)

move :: Vec2 -> Dir -> Vec2
move pos dir
  | dir == RightDown = toRightDown pos
  | dir == RightUp = toRightUp pos
  | dir == LeftUp = toLeftUp pos
  | dir == LeftDown = toLeftDown pos
  | otherwise = pos
