module Block where

import FreeGame
import Ball

data Block = Block{
  blPos :: Vec2,
  blLen :: Double,
  appear :: Bool
} deriving (Eq)

drawBlocks :: [Block] -> Game()
drawBlocks = mapM_ drawBlock

drawBlock :: Block -> Game()
drawBlock block = do
  let (V2 blockX blockY) = blPos block
  color green $
    let blX' = blockX + blLen block
        blY' = blockY + blLen block
        appr = appear block
    in
      when appr $ polygon [blPos block, V2 blX' blockY, V2 blX' blY', V2 blockX blY']

-- contackBall :: Ball -> Block -> (Ball,Block)
-- contackBall ball block
--   | isVerticalBlock ball block =
--     case dir ball of
--       (a, Up) -> (ball{dir = (a, Down)}, block{appear = False})
--       (b, Down) -> (ball{dir =(b, Up)}, block{appear = False})
--   | isHorizontalBlock ball block =
--     case dir ball of
--       (R, a) -> (ball{dir = (L, a)}, block{appear = False})
--       (L, b) -> (ball{dir = (R, b)}, block{appear = False})
--   | otherwise = (ball{ballPos = nextBallP}, block)
--       where nextBallP = move (ballPos ball) (dir ball)
--

isVerticalBlocks :: Ball -> [Block] -> Bool
isVerticalBlocks _ [] = False
isVerticalBlocks ball (x:xs)
  | isVerticalBlock ball x = True
  | otherwise = isVerticalBlocks ball xs

isHorizontalBlocks :: Ball -> [Block] -> Bool
isHorizontalBlocks _ [] = False
isHorizontalBlocks ball (x:xs)
  | isHorizontalBlock ball x = True
  | otherwise = isHorizontalBlocks ball xs

-- TODO やっぱりx,yに分けたほうがいい？？
-- リストの末尾から消えるのは、なぜ？
aDelBlock :: Ball -> [Block] -> Block
aDelBlock _ [x] = x
aDelBlock ball (x:xs)
  | isVerticalBlock ball x = x
  | isHorizontalBlock ball x = x
  | otherwise = aDelBlock ball xs

blockDel :: Ball -> [Block] -> [Block]
blockDel ball blocks =
  let delBlock = aDelBlock ball blocks
  in filter (/=delBlock) blocks


isVerticalBlock :: Ball -> Block -> Bool
isVerticalBlock ball block =
  let (V2 blockX blockY) = blPos block
      (V2 ballX ballY) = ballPos ball
      bl = blLen block
      r = radius ball
  in
    blockY <= ballY + r && ballY - r <= blockY + 5
      && blockX <= ballX + r && ballX - r <= blockX + bl
    ||  (blockY + bl) <= ballY + r && ballY - r <= (blockY + bl) + 5
      && blockX <= ballX + r && ballX - r <= blockX + bl

isHorizontalBlock :: Ball -> Block -> Bool
isHorizontalBlock ball block =
  let (V2 ballX ballY) = ballPos ball
      (V2 blockX blockY) = blPos block
      blockLen = blLen block
      r = radius ball
  in
   blockY <= ballY + r && ballY - r <= blockY + blockLen
    && blockX <= ballX + r && ballX - r <= blockX + 5
    ||  blockY <= ballY + r && ballY - r <= blockY + blockLen
    && (blockX + blockLen) <= ballX + r && ballX - r <= (blockX + blockLen) + 5
