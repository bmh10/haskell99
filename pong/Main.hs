module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
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

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Right player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Left player paddle height.
  , paused :: Bool             -- ^ Is the game paused.
  , wHeld :: Bool
  , sHeld :: Bool
  , upHeld :: Bool
  , downHeld :: Bool
  , player1Score :: Int
  , player2Score :: Int
  , showMenu :: Bool
  } deriving Show 

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (-170, -500)
  , player1 = 40
  , player2 = -80
  , paused = False
  , wHeld = False
  , sHeld = False
  , upHeld = False
  , downHeld = False
  , player1Score = 0
  , player2Score = 0
  , showMenu = True
  }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game
  | (showMenu game) = renderMenu
  | otherwise = pictures [ball, walls, scores,
            mkPaddle rose paddleX $ player1 game,
            mkPaddle orange (-paddleX) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid wallWidth wallHeight

    wallColor = greyN 0.5
    walls = pictures [wall wallYOffset, wall (-wallYOffset)]

    score :: Float -> Int -> Picture
    score xoffset scr = translate xoffset 0 $ scale 0.2 0.2 $ color white $ text $ show scr

    scores = pictures [score (-20) (player2Score game), score 20 (player1Score game)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
     

    paddleColor = light (light blue)

renderMenu :: Picture
renderMenu = pictures [translate (-20) 0 $ scale 0.2 0.2 $ color white $ text "Pong",
                       translate (-50) (-50) $ scale 0.1 0.1 $ color white $ text "Press SPACE to start"]

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y')}
  where 
    -- Old location and velocity
    (x, y)   = ballLoc game
    (vx, vy) = ballVel game

    -- Updated location
    x' = x + vx * seconds
    y' = y + vy * seconds


type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2

paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = leftCollision || rightCollision
  where
    (x, y) = ballLoc game
    ballLeft = (x - radius, y)
    ballRight = (x + radius, y)
    leftPaddlePos = (-paddleX-paddleWidth/2, (player2 game)-paddleHeight/2)
    rightPaddlePos = (paddleX-paddleWidth/2, (player1 game)-paddleHeight/2)
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
    vy' = if wallCollision (ballLoc game) ballRadius
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

-- | Move paddles independently depending on key presses
movePaddle :: PongGame -> PongGame
movePaddle = moveLeftPaddle . moveRightPaddle
moveLeftPaddle game 
  | (wHeld game) = game {player2 = paddleUp (player2 game)}
  | (sHeld game) = game {player2 = paddleDn (player2 game)}
  | otherwise    = game

moveRightPaddle game
  | (upHeld   game) = game {player1 = paddleUp (player1 game)}
  | (downHeld game) = game {player1 = paddleDn (player1 game)}
  | otherwise       = game

paddleMin, paddleMax :: Float
paddleMax = (fromIntegral height/2) - paddleHeight/2 - wallHeight/2
paddleMin = -(fromIntegral height/2) + paddleHeight/2 + wallHeight/2

paddleUp pos = min (pos + 10) paddleMax
paddleDn pos = max (pos - 10) paddleMin

detectEndGame :: PongGame -> PongGame
detectEndGame game
  | x < (-w) = resetBall $ game {player1Score = (player1Score game) + 1}
  | x > w    = resetBall $ game {player2Score = (player2Score game) + 1}
  | otherwise = game
  where 
    (x, y) = ballLoc game
    w = fromIntegral width/2

resetBall :: PongGame -> PongGame
resetBall game = game { ballLoc = (0, 0) }

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

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds game 
  | (paused game) = game 
  | (showMenu game) = game
  | otherwise = detectEndGame $ paddleBounce $ wallBounce $ movePaddle $ moveBall seconds game

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update
