import FreeGame
import Direction
import Block
import Ball
-- import qualified Debug.Trace as D

data Bar = Bar{
  barPos :: Vec2,
  barL :: Double
}

update :: Ball -> [Block] -> Game ()
update ball blocks = do
  (V2 barx _) <- mousePosition
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos ball) $ circleOutline 10

  drawBlocks blocks

  escape <- keyPress KeyEscape
  tick

  unless escape $
    let nextDirBall = reflectBarWall bar ball
        (nextBall, nextBlocks) = refAndDelBlock nextDirBall blocks
    in update nextBall nextBlocks


reflectBarWall :: Bar -> Ball -> Ball
reflectBarWall bar ball
  | isBar bar nextBall || isUpDownWall nextBall =
    case dir ball of
      (a, Up) -> ball{dir = (a, Down)}
      (b, Down) -> ball{dir =(b, Up)}
  | isSideWall nextBall =
    case dir ball of
      (R, a) -> ball{dir = (L, a)}
      (L, b) -> ball{dir = (R, b)}
  -- | isVerticalBlock nextBall block =
  --   case dir ball of
  --     (a, Up) -> (ball{dir = (a, Down)}, block{appear = False})
  --     (b, Down) -> (ball{dir =(b, Up)}, block{appear = False})
  -- | isHorizontalBlock nextBall block =
  --   case dir ball of
  --     (R, a) -> (ball{dir = (L, a)}, block{appear = False})
  --     (L, b) -> (ball{dir = (R, b)}, block{appear = False})
  | otherwise = ball
  where nextBall = ball{ballPos = moveBall ball}

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

refAndDelBlock ::  Ball -> [Block] -> (Ball, [Block])
refAndDelBlock ball blocks
  | isVerticalBlocks nextBall blocks =
    case dir ball of
      (a, Up) -> (ball{dir = (a, Down)}, blockDel ball blocks)
      (b, Down) -> (ball{dir =(b, Up)}, blockDel ball blocks)
  | isHorizontalBlocks nextBall blocks =
    case dir ball of
      (R, a) -> (ball{dir = (L, a)}, blockDel ball blocks)
      (L, b) -> (ball{dir = (R, b)}, blockDel ball blocks)
  | otherwise = (nextBall, blocks)
  where nextBall = ball{ballPos = moveBall ball}

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

    -- update ball block
    update ball (makeBlocks block 100)
