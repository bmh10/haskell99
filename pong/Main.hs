module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

fps = 60
width = 600
height = 400
offset = 100
wallHeight = 10
wallWidth = fromIntegral $ width - 30
wallYOffset = fromIntegral $ div height 2
ballRadius = 10
paddleX = 260
paddleWidth = 5
paddleHeight = 86
paddleMax = (fromIntegral height/2) - paddleHeight/2 - wallHeight/2
paddleMin = -(fromIntegral height/2) + paddleHeight/2 + wallHeight/2
window = InWindow "Pong" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data PongGame = Game
  { ballPos :: (Float, Float)
  , ballVel :: (Float, Float)
  , player1Pos :: Float
  , player2Pos :: Float
  , player1Score :: Int
  , player2Score :: Int
  , wHeld :: Bool
  , sHeld :: Bool
  , upHeld :: Bool
  , downHeld :: Bool
  , paused :: Bool
  , showMenu :: Bool 
  } deriving Show 

initialState :: PongGame
initialState = Game
  { ballPos = (-10, 30)
  , ballVel = (-170, -500)
  , player1Pos = 40
  , player2Pos = -80
  , paused = False
  , wHeld = False
  , sHeld = False
  , upHeld = False
  , downHeld = False
  , player1Score = 0
  , player2Score = 0
  , showMenu = True
  }

render :: PongGame -> Picture 
render game
  | (showMenu game) = renderMenu
  | otherwise       = renderGame game

renderMenu :: Picture
renderMenu = pictures [translate (-20) 0     $ scale 0.2 0.2 $ color white $ text "Pong",
                       translate (-50) (-50) $ scale 0.1 0.1 $ color white $ text "Press SPACE to start"]

renderGame :: PongGame -> Picture
renderGame game = pictures [ball, walls, scores, paddles]
  where
    ball = uncurry translate (ballPos game) $ color (dark red) $ circleSolid ballRadius

    wall ypos = translate 0 ypos $ color (greyN 0.5) $ rectangleSolid wallWidth wallHeight
    walls = pictures [wall wallYOffset, wall (-wallYOffset)]

    score xpos val = translate xpos 0 $ scale 0.2 0.2 $ color white $ text $ show val
    scores = pictures [score (-20) (player2Score game), score 20 (player1Score game)]

    paddle col x y = translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
    paddles = pictures [paddle rose paddleX $ player1Pos game, paddle orange (-paddleX) $ player2Pos game]
     
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballPos = (x', y')}
  where
    (x, y)   = ballPos game
    (vx, vy) = ballVel game
    x' = x + vx * seconds
    y' = y + vy * seconds

movePaddle :: PongGame -> PongGame
movePaddle = moveLeftPaddle . moveRightPaddle -- Move paddles independently
moveLeftPaddle game 
  | (wHeld game) = game {player2Pos = paddleUp (player2Pos game)}
  | (sHeld game) = game {player2Pos = paddleDn (player2Pos game)}
  | otherwise    = game

moveRightPaddle game
  | (upHeld   game) = game {player1Pos = paddleUp (player1Pos game)}
  | (downHeld game) = game {player1Pos = paddleDn (player1Pos game)}
  | otherwise       = game

paddleUp pos = min (pos + 10) paddleMax
paddleDn pos = max (pos - 10) paddleMin

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2

paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = leftCollision || rightCollision
  where
    (x, y) = ballPos game
    ballLeft = (x - radius, y)
    ballRight = (x + radius, y)
    leftPaddlePos = (-paddleX-paddleWidth/2, (player2Pos game)-paddleHeight/2)
    rightPaddlePos = (paddleX-paddleWidth/2, (player1Pos game)-paddleHeight/2)
    leftCollision = rectCollision ballLeft leftPaddlePos paddleWidth paddleHeight
    rightCollision = rectCollision ballRight rightPaddlePos paddleWidth paddleHeight

rectCollision :: Position -> Position -> Float -> Float -> Bool
rectCollision (bx, by) (rx, ry) width height = 
  rx <= bx && bx <= rx+width &&
  ry <= by && by <= ry+height
  

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel  = (vx, vy')}
  where
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballPos game) ballRadius
          then -vy
          else vy

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy)}
  where
    (vx, vy) = ballVel game
    vx' = if paddleCollision game ballRadius
          then -vx
          else vx

detectEndGame :: PongGame -> PongGame
detectEndGame game
  | x < (-w) = resetBall $ game {player1Score = (player1Score game) + 1}
  | x > w    = resetBall $ game {player2Score = (player2Score game) + 1}
  | otherwise = game
  where 
    (x, y) = ballPos game
    w = fromIntegral width/2

resetBall :: PongGame -> PongGame
resetBall game = game { ballPos = (0, 0) }

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'r') Down _ _) game = 
  resetBall game

handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = (not (paused game))}

handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game =
  game { showMenu = False }

handleKeys (EventKey (Char 'w') state _ _) game =
  game {wHeld = (state == Down)}

handleKeys (EventKey (Char 's') state _ _) game =
  game {sHeld = (state == Down)}

handleKeys (EventKey (SpecialKey KeyUp) state _ _) game = 
  game {upHeld = (state == Down)}

handleKeys (EventKey (SpecialKey KeyDown) state _ _) game = 
  game {downHeld = (state == Down)}

-- Do nothing for all other events.
handleKeys _ game = game


update :: Float -> PongGame -> PongGame
update seconds game 
  | (paused game) = game 
  | (showMenu game) = game
  | otherwise = detectEndGame $ paddleBounce $ wallBounce $ movePaddle $ moveBall seconds game

main :: IO ()
main = play window background fps initialState render handleKeys update
