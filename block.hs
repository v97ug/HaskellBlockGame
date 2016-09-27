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
    let (nextBall2, nextBlock) = reflectAndDeleteBlock bar ball block
    in update nextBall2 nextBlock


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
