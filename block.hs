import FreeGame
import Direction
import Block
-- import qualified Debug.Trace as D

data Ball = Ball{
  ballPos :: Vec2,
  dir :: (Horizontal, Vertical),
  radius :: Double
}

data Bar = Bar{
  barPos :: Vec2,
  barL :: Double
}

update :: Ball -> Block -> Game ()
update ball block = do
  (V2 barx _) <- mousePosition
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos ball) $ circleOutline 10

  let (V2 blockX blockY) = blPos block
  color green $
    let blX' = blockX + blLen block
        blY' = blockY + blLen block
    in
      polygon [blPos block, V2 blX' blockY, V2 blX' blY', V2 blockX blY']

  escape <- keyPress KeyEscape
  tick

  -- TODO 反射の実装
  unless escape $
    let nextBallP = move (ballPos ball) (dir ball)
        (direction, ballP)
          | isVerticalRef bar ball{ballPos = nextBallP} block =
            case dir ball of
              (a, Up) -> ((a, Down), ballPos ball)
              (b, Down) -> ((b, Up), ballPos ball)
          | isHorizontalRef bar ball{ballPos = nextBallP} block =
            case dir ball of
              (R, a) -> ((L, a), ballPos ball)
              (L, b) -> ((R, b), ballPos ball)
          | otherwise = (dir ball, nextBallP)

    in update ball{ ballPos = ballP, dir = direction} block

isVerticalRef :: Bar -> Ball -> Block -> Bool
isVerticalRef bar ball block =
  let (V2 barX barY) = barPos bar
      (V2 ballX ballY) = ballPos ball
      (V2 blockX blockY) = blPos block
      barLen = barL bar
      blockLen = blLen block
      r = radius ball
  in
    barY <= ballY + r && ballY + r <= barY + 5
    && barX <= ballX + r && ballX - r <= barX + barLen
    || ballY - r <= 0 || 480 <= ballY + r
    ||  blockY <= ballY + r && ballY + r <= blockY + 5
        && blockX <= ballX + r && ballX - r <= blockX + blockLen
    ||  (blockY + blockLen) <= ballY + r && ballY + r <= (blockY + blockLen) + 5
        && blockX <= ballX + r && ballX - r <= blockX + blockLen

isHorizontalRef :: Bar -> Ball -> Block ->Bool
isHorizontalRef bar ball block =
  let (V2 ballX ballY) = ballPos ball
      (V2 blockX blockY) = blPos block
      blockLen = blLen block
      r = radius ball
  in
    ballX - r <= 0 || 640 <= ballX + r
    ||  blockY <= ballY + r && ballY + r <= blockY + blockLen
        && blockX <= ballX + r && ballX - r <= blockX + 5
    ||  blockY <= ballY + r && ballY + r <= blockY + blockLen
        && (blockX + blockLen) <= ballX + r && ballX - r <= (blockX + blockLen) + 5
    -- TODO x軸についてblockの反射

-- stickContact :: Vec2 -> Double -> Ball -> Bool
-- stickContact (V2 stickX stickY) stickLen ball =
--   let (V2 ballX ballY) = ballPos ball
--       r = radius ball
--       thick = 5 -- 当たり判定の大きさ（棒の太さ）
--   in
--     stickY <= ballY + r && ballY + r <= stickY + thick
--     && stickX <= ballX + r && ballX + r <= stickX + stickLen


main :: IO(Maybe ())
main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    clearColor black
    -- font <- loadFont "VL-PGothic-Regular.ttf"

    let ball = Ball{
      ballPos = V2 25 100,
      dir = (R, Down),
      radius = 5
    }

    let block = Block{
      blPos = V2 100 100,
      blLen = 130,
      appear = True
    }

    update ball block
