import FreeGame
import Direction
-- import qualified Debug.Trace as D

data Ball = Ball{
  ballPos :: Vec2,
  dir :: Dir
}

data Bar = Bar{
  barPos :: Vec2,
  barL :: Double
}

-- TODO rをデータ型の中に入れる
r :: Double
r = 5

update :: Ball -> Game ()
update ball = do
  (V2 barx _) <- mousePosition
  -- let (V2 barx _) = p
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  -- translate p $ color white $ thickness 4 $ circleOutline 16
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos ball) $ circleOutline 10 -- 'thickness' sets the thickness of the lines.

  escape <- keyPress KeyEscape
  tick

  -- TODO 反射の実装
  unless escape $
    let nextBallP = move (ballPos ball) (dir ball)
        willReflect = reflectable bar nextBallP
        (direction, ballP) = if willReflect then
          case  dir ball of
            RightDown -> (RightUp, ballPos ball)
            RightUp -> (LeftUp, ballPos ball)
            LeftUp -> (LeftDown, ballPos ball)
            LeftDown -> (RightDown, ballPos ball)
          else (dir ball, nextBallP)
    in update ball{ ballPos = ballP, dir = direction}

reflectable :: Bar -> Vec2 -> Bool
reflectable bar (V2 ballX ballY) =
  let (V2 barX barY) = barPos bar
      bl = barL bar
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
      dir = RightDown
    }

    update ball
