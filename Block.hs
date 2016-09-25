module Block where

import FreeGame

data Block = Block{
  blPos :: Vec2,
  blLen :: Double,
  appear :: Bool
}
-- 
-- isVerticalBlock block ball =
--   let (V2 blockX blockY) = blPos block
--       (V2 ballX ballY) = ballPos ball
--       bl = blLen block
--       r = radius ball
--   in
--     blcokY <= ballY + r && ballY + r <= blockY + bl
--     && blockX <= ballX + r && ballX - r <= blockX + bl
