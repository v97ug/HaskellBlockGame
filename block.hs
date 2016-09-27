import FreeGame
import Direction
import Block
import Ball
-- import qualified Debug.Trace as D

data Bar = Bar{
  barPos :: Vec2,
  barL :: Double
}

-- update :: Ball -> [Block] -> Game ()
update ball block = do
  (V2 barx _) <- mousePosition
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos ball) $ circleOutline 10

  drawBlock block
  escape <- keyPress KeyEscape
  tick

  unless escape $
    -- let nextBallP = move (ballPos ball) (dir ball)
        -- nextBall
        --   | isVerticalRef bar ball{ballPos = nextBallP} block =
        --     case dir ball of
        --       (a, Up) -> ball{dir = (a, Down)}
        --       (b, Down) -> ball{dir = (b, Up)}
        --   | isHorizontalRef bar ball{ballPos = nextBallP} block =
        --     case dir ball of
        --       (R, a) -> ball{dir = (L, a)}
        --       (L, b) -> ball{dir = (R, b)}
        --   | otherwise = ball{ballPos = nextBallP}

          --前のballを渡す
        -- (nextBall2, nextBlock) = contackBall ball{ballPos = nextBallP} block
    let (nextBall2, nextBlock) = reflectAndDeleteBlock bar ball block

    in update nextBall2 nextBlock


--TODO ここをよーく見よう
reflectAndDeleteBlock :: Bar -> Ball -> Block -> (Ball, Block)
reflectAndDeleteBlock bar ball block =
  let nextBall = ball{ballPos = move (ballPos ball) (dir ball)} in
  if isBar bar nextBall then
    case dir ball of
      (a, Up) -> (ball{dir = (a, Down)}, block)
      (b, Down) -> (ball{dir =(b, Up)}, block)
  else if isSideWall nextBall then
    case dir ball of
      (R, a) -> (ball{dir = (L, a)}, block)
      (L, b) -> (ball{dir = (R, b)}, block)
  else if isUpDownWall nextBall then
    case dir ball of
      (a, Up) -> (ball{dir = (a, Down)}, block)
      (b, Down) -> (ball{dir =(b, Up)}, block)
  else if isVerticalBlock nextBall block then
    case dir ball of
      (a, Up) -> (ball{dir = (a, Down)}, block{appear = False})
      (b, Down) -> (ball{dir =(b, Up)}, block{appear = False})
  else if isHorizontalBlock nextBall block then
    case dir ball of
      (R, a) -> (ball{dir = (L, a)}, block{appear = False})
      (L, b) -> (ball{dir = (R, b)}, block{appear = False})
  else (nextBall, block)

isBar :: Bar -> Ball -> Bool
isBar bar ball =
  let (V2 barX barY) = barPos bar
      (V2 ballX ballY) = ballPos ball
      barLen = barL bar
      r = radius ball
  in
    barY <= ballY + r && ballY + r <= barY + 5
    && barX <= ballX + r && ballX - r <= barX + barLen

isUpDownWall :: Ball -> Bool
isUpDownWall ball =
  let (V2 _ ballY) = ballPos ball
      r = radius ball
  in
    ballY - r <= 0 || 480 <= ballY + r

isSideWall :: Ball -> Bool
isSideWall ball =
  let (V2 ballX _) = ballPos ball
      r = radius ball
  in
    ballX - r <= 0 || 640 <= ballX + r

-- isVerticalRef :: Bar -> Ball -> Block -> Bool
-- isVerticalRef bar ball block =
--   let (V2 barX barY) = barPos bar
--       (V2 ballX ballY) = ballPos ball
--       (V2 blockX blockY) = blPos block
--       barLen = barL bar
--       blockLen = blLen block
--       r = radius ball
--   in
--     barY <= ballY + r && ballY + r <= barY + 5
--     && barX <= ballX + r && ballX - r <= barX + barLen
--     || ballY - r <= 0 || 480 <= ballY + r
--     ||  blockY <= ballY + r && ballY + r <= blockY + 5
--         && blockX <= ballX + r && ballX - r <= blockX + blockLen
--     ||  (blockY + blockLen) <= ballY + r && ballY + r <= (blockY + blockLen) + 5
--         && blockX <= ballX + r && ballX - r <= blockX + blockLen
--
-- isHorizontalRef :: Bar -> Ball -> Block ->Bool
-- isHorizontalRef bar ball block =
--   let (V2 ballX ballY) = ballPos ball
--       (V2 blockX blockY) = blPos block
--       blockLen = blLen block
--       r = radius ball
--   in
--     ballX - r <= 0 || 640 <= ballX + r
--     ||  blockY <= ballY + r && ballY + r <= blockY + blockLen
--         && blockX <= ballX + r && ballX - r <= blockX + 5
--     ||  blockY <= ballY + r && ballY + r <= blockY + blockLen
--         && (blockX + blockLen) <= ballX + r && ballX - r <= (blockX + blockLen) + 5

-- changeBlock :: Ball -> Block -> Block
-- changeBlock ball block =
--   let (V2 ballX ballY) = ballPos ball
--       (V2 blockX blockY) = blPos block
--       blockLen = blLen block
--       r = radius ball
--   in
--     if (blockY <= ballY + r && ballY + r <= blockY + blockLen
--        || blockY <= ballY - r && ballY - r <= blockY + blockLen)
--         && (blockX <= ballX - r && ballX - r <= blockX + blockLen
--        || blockX <= ballX + r && ballX + r <= blockX + blockLen)
--        then block{appear = False}
--        else block

-- stickContact :: Vec2 -> Double -> Ball -> Bool
-- stickContact (V2 stickX stickY) stickLen ball =
--   let (V2 ballX ballY) = ballPos ball
--       r = radius ball
--       thick = 5 -- 当たり判定の大きさ（棒の太さ）
--   in
--     stickY <= ballY + r && ballY + r <= stickY + thick
--     && stickX <= ballX + r && ballX + r <= stickX + stickLen

makeBlocks :: Block -> Double -> [Block]
makeBlocks block x
 | x > 600 = []
 | otherwise = block : makeBlocks block{blPos = V2 x 100} (x+50)

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
      blLen = 30,
      appear = True
    }

    update ball block
    -- update ball (makeBlocks block)
