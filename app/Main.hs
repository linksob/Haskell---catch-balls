module Main where

import Data.Fixed (mod')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Lib

window :: Display
window = InWindow "Bouncy Balls" (800, 600) (560, 216)

background :: Color
background = blue

data BallGame = Game
  { ballLoc :: (Float, Float),
    ballVel :: (Float, Float),
    ballAcc :: (Float, Float),
    seconds :: Float
  }
  deriving (Show)

initialState :: (BallGame, BallGame)
initialState =
  ( Game
      { ballLoc = (0, 0),
        ballVel = (0, 0),
        ballAcc = (0, -98),
        seconds = 0
      },
    Game
      { ballLoc = (50, 0),
        ballVel = (0, 0),
        ballAcc = (0, -98),
        seconds = 0
      }
  )

render :: (BallGame, BallGame) -> Picture
render game = pictures [ball1, ball2, drawWalls, translate 250 270 $ scale 0.1 0.1 $ text $ "tempo: " ++ show secs]
  where
    ball1 = uncurry translate (ballLoc $ fst game) $ color ballColor $ circleSolid 10
    ball2 = uncurry translate (ballLoc $ snd game) $ color ballColor $ circleSolid 10
    ballColor = red
    secs = seconds $ fst game

drawWalls :: Picture
drawWalls =
  pictures
    [ translate 130 0 $ color black $ rectangleSolid 30 290,
      translate (-130) 0 $ color black $ rectangleSolid 30 290,
      translate 0 130 $ color black $ rectangleSolid 290 30,
      translate 0 (-130) $ color black $ rectangleSolid 290 30
    ]

moveBall :: Float -> BallGame -> BallGame
moveBall secs game =
  game
    { ballLoc = (x', y'),
      ballVel = (vx', vy')
    }
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    (ax, ay) = ballAcc game
    vx' = vx + ax * secs
    vy' = vy + ay * secs
    x' = x + vx' * secs
    y' = y + vy' * secs

wallBounce :: BallGame -> Float -> BallGame
wallBounce game secs =
  game
    { ballLoc = (x, y'),
      ballVel = (vx', vy'),
      ballAcc = (ax, ay'),
      seconds = secs''
    }
  where
    (vx, vy) = ballVel game
    (ax, ay) = ballAcc game
    (x, y) = ballLoc game
    secs' = seconds game
    vx' = vx
    vy'
      | mod' secs 0.15 < 0.03 && y < -104 = (-vy) * 0.8
      | otherwise = vy
    ay' = if mod' secs 0.15 < 0.03 && y < -105 then 0 else ay
    y' = if mod' secs 0.15 < 0.03 && y < -105 then -105 else y
    secs'' = secs' + secs

update :: ViewPort -> Float -> (BallGame, BallGame) -> (BallGame, BallGame)
update _ seconds (b1, b2) = (moveBall seconds (wallBounce b1 seconds), moveBall seconds (wallBounce b2 seconds))

fps :: Int
fps = 300

main :: IO ()
main = simulate window background fps initialState render update