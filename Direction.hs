module Direction where

import FreeGame

data Dir = RightDown | RightUp | LeftUp | LeftDown 

toRightDown :: Vec2 -> Vec2
toRightDown (V2 x y) = V2 (x+2) (y+2)

toRightUp :: Vec2 -> Vec2
toRightUp (V2 x y) = V2 (x+2) (y-2)

toLeftUp :: Vec2 -> Vec2
toLeftUp (V2 x y) = V2 (x-2) (y-2)

toLeftDown :: Vec2 -> Vec2
toLeftDown (V2 x y) = V2 (x-2) (y+2)
