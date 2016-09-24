import FreeGame
import Direction
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

update :: Ball -> Game ()
update ball = do
  (V2 barx _) <- mousePosition
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos ball) $ circleOutline 10

  escape <- keyPress KeyEscape
  tick

  -- TODO 反射の実装
  unless escape $
    let nextBallP = move (ballPos ball) (dir ball)
        (direction, ballP)
          | isVerticalRef bar ball{ballPos = nextBallP} =
            case dir ball of
              (a, Up) -> ((a, Down), ballPos ball)
              (b, Down) -> ((b, Up), ballPos ball)
          | isHorizontalRef bar ball{ballPos = nextBallP} =
            case dir ball of
              (R, a) -> ((L, a), ballPos ball)
              (L, b) -> ((R, b), ballPos ball)
          | otherwise = (dir ball, nextBallP)

    in update ball{ ballPos = ballP, dir = direction}

isVerticalRef :: Bar -> Ball -> Bool
isVerticalRef bar ball =
  let (V2 barX barY) = barPos bar
      (V2 ballX ballY) = ballPos ball
      bl = barL bar
      r = radius ball
  in
    barY <= ballY + r && ballY + r <= barY + 5
    && barX <= ballX + r && ballX - r <= barX + bl
    || ballY - r <= 0 || 480 <= ballY + r

isHorizontalRef :: Bar -> Ball -> Bool
isHorizontalRef bar ball =
  let (V2 barX barY) = barPos bar
      (V2 ballX ballY) = ballPos ball
      -- bl = barL bar
      r = radius ball
  in
    ballX - r <= 0 || 640 <= ballX + r

main :: IO(Maybe ())
main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    clearColor black
    -- font <- loadFont "VL-PGothic-Regular.ttf"

    let ball = Ball{
      ballPos = V2 25 100,
      dir = (R, Down),
      radius = 5
    }

    update ball
