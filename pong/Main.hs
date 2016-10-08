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
  , playerLPos :: Float
  , playerRPos :: Float
  , playerLScore :: Int
  , playerRScore :: Int
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
  , playerLPos = 40
  , playerRPos = -80
  , playerLScore = 0
  , playerRScore = 0
  , wHeld = False
  , sHeld = False
  , upHeld = False
  , downHeld = False
  , paused = False
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
    scores = pictures [score (-20) (playerLScore game), score 20 (playerRScore game)]

    paddle col x y = translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
    paddles = pictures [paddle rose (-paddleX) $ playerLPos game, paddle orange paddleX $ playerRPos game]
     
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
  | (wHeld game) = game {playerLPos = paddleUp (playerLPos game)}
  | (sHeld game) = game {playerLPos = paddleDn (playerLPos game)}
  | otherwise    = game

moveRightPaddle game
  | (upHeld   game) = game {playerRPos = paddleUp (playerRPos game)}
  | (downHeld game) = game {playerRPos = paddleDn (playerRPos game)}
  | otherwise       = game

paddleUp pos = min (pos + 10) paddleMax
paddleDn pos = max (pos - 10) paddleMin

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel  = (vx, vy')}
  where
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballPos game) ballRadius then -vy else vy

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy)}
  where
    (vx, vy) = ballVel game
    vx' = if paddleCollision game ballRadius then -vx else vx

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
    leftPaddlePos = (-paddleX-paddleWidth/2, (playerLPos game)-paddleHeight/2)
    rightPaddlePos = (paddleX-paddleWidth/2, (playerRPos game)-paddleHeight/2)
    leftCollision = rectCollision ballLeft leftPaddlePos paddleWidth paddleHeight
    rightCollision = rectCollision ballRight rightPaddlePos paddleWidth paddleHeight

rectCollision :: Position -> Position -> Float -> Float -> Bool
rectCollision (bx, by) (rx, ry) width height = 
  rx <= bx && bx <= rx+width && ry <= by && by <= ry+height
  
detectEndGame :: PongGame -> PongGame
detectEndGame game
  | x < (-w) = resetBall $ game {playerRScore = (playerRScore game) + 1}
  | x > w    = resetBall $ game {playerLScore = (playerLScore game) + 1}
  | otherwise = game
  where 
    (x, _) = ballPos game
    w = fromIntegral width/2

resetBall :: PongGame -> PongGame
resetBall game = game { ballPos = (0, 0) }

-- Event handling
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') Down _ _) game = resetBall game
handleKeys (EventKey (Char 'p') Down _ _) game = game { paused = (not (paused game))}
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { showMenu = False }
handleKeys (EventKey (Char 'w') state _ _) game = game {wHeld = (state == Down)}
handleKeys (EventKey (Char 's') state _ _) game = game {sHeld = (state == Down)}
handleKeys (EventKey (SpecialKey KeyUp) state _ _) game = game {upHeld = (state == Down)}
handleKeys (EventKey (SpecialKey KeyDown) state _ _) game = game {downHeld = (state == Down)}
handleKeys _ game = game

update :: Float -> PongGame -> PongGame
update seconds game 
  | (paused game) = game 
  | (showMenu game) = game
  | otherwise = detectEndGame $ paddleBounce $ wallBounce $ movePaddle $ moveBall seconds game

main = play window background fps initialState render handleKeys update
