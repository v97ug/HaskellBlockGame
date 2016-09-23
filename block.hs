import FreeGame
import Direction
import qualified Debug.Trace as D

data State = State{
  ballPos :: Vec2,
  dir :: Dir
}

data Bar = Bar{
  barPos :: Vec2,
  barL :: Double
}

-- barL = 60
r :: Double
r = 5

update :: State -> Game ()
update state = do
  (V2 barx _) <- mousePosition
  -- let (V2 barx _) = p
  let bar = Bar{barPos = V2 barx 400, barL = 60}
  -- translate p $ color white $ thickness 4 $ circleOutline 16
  color cyan $ thickness 5 $ line [barPos bar, V2 (barx+60) 400]

  color magenta $ thickness 3 $ translate (ballPos state) $ circleOutline 10 -- 'thickness' sets the thickness of the lines.

  escape <- keyPress KeyEscape
  tick
  unless escape $
    let isReflect = reflectable bar (ballPos state)
        direction = if isReflect then
          case  dir state of
            RightDown -> RightUp
            RightUp -> LeftUp
            LeftUp -> LeftDown
            LeftDown -> RightDown
          else dir state
        ballP = case direction of
          RightDown -> toRightUp (ballPos state)
          RightUp -> toLeftUp (ballPos state)
          LeftUp -> toLeftDown (ballPos state)
          LeftDown -> toRightDown (ballPos state)
    in update state{ ballPos = ballP, dir = direction}

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

    let state = State{
      ballPos = V2 25 100,
      -- dir = toLeftUp
      dir = RightDown
    }

    update state
