-- https://rosettacode.org/wiki/2048
-- stack runghc --resolver lts-9.0 --install-ghc --package matrix --package random --package gloss -- 2048.hs
module Main(main) where

import           Data.Matrix
import           Data.Maybe                       (fromMaybe)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

-- Game State
type GameTile = Int
data GameData = Game
  { gameTiles :: Matrix GameTile
  , winValue  :: Int
  , randomGen :: StdGen
  }
  | Won  { randomGen :: StdGen }
  | Lost { randomGen :: StdGen }
instance Show GameData where
  show Won{}       = "You won"
  show Lost{}      = "You lost"
  show game@Game{} = prettyMatrix $ gameTiles game

newGame :: (Int, Int) -> StdGen -> GameData
newGame (r, c) randGen = addTile $ addTile game
  where
    game = Game tiles 2048 randGen
    tiles = zero r c

-- Game Logic
data GameAction = ShiftLeft | ShiftRight | ShiftUp | ShiftDown | Quit

iterateGame :: GameAction -> GameData -> Maybe GameData
iterateGame _      g@Won{}  = Just g
iterateGame _      g@Lost{} = Just g
iterateGame Quit   g        = Just $ Lost $ randomGen g
iterateGame action g@Game{gameTiles=tiles} = game'
  where
    shiftedTiles = case action of
      ShiftLeft  -> shiftLeft  tiles
      ShiftRight -> shiftRight tiles
      ShiftUp    -> shiftUp    tiles
      ShiftDown  -> shiftDown  tiles

    game' = if shiftedTiles == tiles
      then Nothing
      else Just $ checkWin $ addTile g{gameTiles=shiftedTiles}

matrixCells :: Matrix a -> [(Int, Int, a)]
matrixCells m = map (\[r, c] -> (r, c, m ! (r, c))) $ sequence [[1..nrows m], [1..ncols m]]

addTile :: GameData -> GameData
addTile game@Won{}  = game
addTile game@Lost{} = game
addTile game@Game{gameTiles=tiles, randomGen=g} = game'
  where
    openTiles = filter isOpen $ matrixCells tiles

    isOpen :: (Int, Int, GameTile) -> Bool
    isOpen (r, c, v) = v == 0

    game' =
      if null openTiles
      then Lost g
      else game{gameTiles=tiles', randomGen=g''}
        where
          ((r, c, _), g') = pickRandom openTiles g
          (newTile, g'') = pickRandom newTileOptions g'
          tiles' = setElem 2 (r, c) tiles

          -- 10% chance of adding a 4
          newTileOptions = 4:replicate 9 2

checkWin :: GameData -> GameData
checkWin game@Won{}  = game
checkWin game@Lost{} = game
checkWin game@Game{gameTiles=tiles} =
  if winValue game `elem` toList tiles
    then Won $ randomGen game
    else game

shiftRowLeft :: [Int] -> [Int]
shiftRowLeft = shiftZero . shiftRow . shiftZero
  where
    shiftZero :: [Int] -> [Int]
    shiftZero [] = []
    shiftZero (x:xs)
      | x == 0 = shiftZero xs ++ [0]
      | otherwise = x:shiftZero xs

    shiftRow :: [Int] -> [Int]
    shiftRow []     = []
    shiftRow [x] = [x]
    shiftRow (x:y:xs)
      | x == y = [x+y, 0] ++ shiftRow xs
      | otherwise = x : shiftRow (y:xs)

shiftLeft :: Matrix Int -> Matrix Int
shiftLeft = fromLists . map shiftRowLeft . toLists

shiftRight :: Matrix Int -> Matrix Int
shiftRight = fromLists . map shiftRowRight . toLists
  where
    shiftRowRight = reverse . shiftRowLeft . reverse

shiftUp :: Matrix Int -> Matrix Int
shiftUp = transpose . shiftLeft . transpose

shiftDown :: Matrix Int -> Matrix Int
shiftDown = transpose . shiftRight . transpose

-- Utility Functions
-- | Picks a random element of the list.
pickRandom :: [a] -> StdGen -> (a, StdGen)
pickRandom xs g =
    let (idx, g') = randomR (0, length xs - 1) g
      in (xs !! idx, g')

-- | Returns a list with the element at a specific index replaced with a new value.
replace :: [a] -> Int -> a -> [a]
replace xs idx value = before ++ [value] ++ after'
  where
    (before, after) = splitAt idx xs
    after' = tail after

-- User Interface
data GameWorld = GameWorld
  { worldGame :: GameData
  , worldSize :: (Int, Int)
  }

runUI :: GameData -> IO ()
runUI game = play display background fps world render processInput stepWorld
  where
    display = InWindow "2048" size (10, 10)
    background = black
    fps = 60
    size = (400, 400)
    world = GameWorld game size

processInput :: Event -> GameWorld -> GameWorld
processInput (EventResize newSize) world = world{worldSize=newSize}
processInput (EventMotion _) world = world
processInput (EventKey key Up modifiers _) world
  | key == SpecialKey KeyLeft  = iterateWorld world ShiftLeft
  | key == SpecialKey KeyRight = iterateWorld world ShiftRight
  | key == SpecialKey KeyUp    = iterateWorld world ShiftUp
  | key == SpecialKey KeyDown  = iterateWorld world ShiftDown
  | otherwise = world
  where
    iterateWorld :: GameWorld -> GameAction -> GameWorld
    iterateWorld world@GameWorld{worldGame=game} action = world{worldGame=game'}
      where game' = fromMaybe game $ iterateGame action game
processInput _ world = world

stepWorld :: Float -> GameWorld -> GameWorld
stepWorld _ = id

render :: GameWorld -> Picture
render (GameWorld Lost{} size) = color white $ scaledText size "You lost"
render (GameWorld Won{}  size) = color white $ scaledText size "You won"
render (GameWorld game (width, height)) = pictures boxes
  where
    tiles = gameTiles game
    padding = 4
    boxW = width `div` nrows tiles - (padding * 2)
    boxH = height `div` ncols tiles - (padding * 2)

    boxes = map (renderTile . fixRC) $ matrixCells tiles

    fixRC :: (Int, Int, GameTile) -> (Int, Int, GameTile)
    fixRC (r, c, tile) = (nrows tiles - r + 1, c, tile)

    renderTile :: (Int, Int, GameTile) -> Picture
    renderTile (r, c, tile) = offsetTile r c pic
      where
        pic = pictures [background, border, number]
        background = backgroundColor tile $ rectangleSolid (fromIntegral boxW) (fromIntegral boxH)
        border = color white $ rectangleWire (fromIntegral boxW) (fromIntegral boxH)
        number = color white $ scaledText (width, height) $ show tile

    offsetTile :: Int -> Int -> Picture -> Picture
    offsetTile r c = translate offsetX offsetY
      where
        offsetX = fromIntegral $ zeroX + ((c - 1) * (boxW + padding))
        offsetY = fromIntegral $ zeroY + ((r - 1) * (boxH + padding))

        zeroX = boxW `div` 2 - width  `div` 2 + padding * 2
        zeroY = boxH `div` 2 - height `div` 2 + padding * 2

backgroundColor :: GameTile -> Picture -> Picture
backgroundColor tile = color (bgColor tile)
  where
    bgColor :: GameTile -> Color
    bgColor tile
      | tile == 0 = light $ light blue
      | tile < 32 = light blue
      | tile < 64 = blue
      | tile < 128 = dark blue
      | otherwise = orange

scaledText :: (Int, Int) -> String -> Picture
scaledText (width, height) t = pic
  where
    pic = scale scaleX scaleY $ text t

    scaleX = fromIntegral width / 2000
    scaleY = fromIntegral height / 2000

main :: IO ()
main = do
  randomGen <- getStdGen
  runUI $ newGame (4, 4) randomGen
  --runUI $ Game (fromList 4 4 [1..16]) 2048 randomGen
  return ()
