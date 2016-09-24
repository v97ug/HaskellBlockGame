import FreeGame
import Direction
-- import qualified Debug.Trace as D

data Ball = Ball{
  ballPos :: Vec2,
  dir :: Dir,
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
        willReflect = reflectable bar ball{ballPos = nextBallP}
        (direction, ballP) = if willReflect then
          case  dir ball of
            RightDown -> (RightUp, ballPos ball)
            RightUp -> (LeftUp, ballPos ball)
            LeftUp -> (LeftDown, ballPos ball)
            LeftDown -> (RightDown, ballPos ball)
          else (dir ball, nextBallP)
    in update ball{ ballPos = ballP, dir = direction}

-- TODO reflectable をなんとかする
-- TODO (Right,Down)などのタプルにする
reflectable :: Bar -> Ball -> Bool
reflectable bar ball =
  let (V2 barX barY) = barPos bar
      (V2 ballX ballY) = ballPos ball
      bl = barL bar
      r = radius ball
  in
   barY <= ballY + r && ballY + r <= barY + 5
    && barX <= ballX + r && ballX - r <= barX + bl
    || ballX - r <= 0 || 640 <= ballX + r
    || ballY - r <= 0 || 480 <= ballY + r



main :: IO(Maybe ())
main = runGame Windowed (Box (V2 0 0) (V2 640 480)) $ do
    clearColor black
    -- font <- loadFont "VL-PGothic-Regular.ttf"

    let ball = Ball{
      ballPos = V2 25 100,
      dir = RightDown,
      radius = 5
    }

    update ball
