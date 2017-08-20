-- https://rosettacode.org/wiki/2048
-- stack runghc --resolver lts-9.0 --install-ghc --package matrix --package random -- 2048.hs
module Main(main) where

import           Control.Monad             (liftM, replicateM)
import           Control.Monad.Trans.State (runState, state)
import           Data.Matrix
import           System.Random

type GameTile = Int
data GameData = Game
  { gameTiles :: Matrix GameTile
  , randomGen :: StdGen
  }
  | Won  { randomGen :: StdGen }
  | Lost { randomGen :: StdGen }
instance Show GameData where
  show Won{}       = "You won"
  show Lost{}      = "You lost"
  show game@Game{} = prettyMatrix $ gameTiles game

-- | Splits a list into chunks of a size.
chunk :: Int -> [a] -> [[a]]
cunnk _ [] = []
chunk size xs
  | size >= length xs = [xs]
  | otherwise = cur:chunk size rest
    where
      (cur, rest) = splitAt size xs

newGame :: (Int, Int) -> StdGen -> GameData
newGame (r, c) randGen = addTile $ addTile game
  where
    game = Game tiles randGen
    tiles = zero r c

addTile :: GameData -> GameData
addTile game@Won{}  = game
addTile game@Lost{} = game
addTile game@Game{gameTiles=tiles, randomGen=g} = game'
  where
    allTiles = sequence [[1..nrows tiles], [1..ncols tiles]]
    openTiles = filter isOpen allTiles

    isOpen :: [Int] -> Bool
    isOpen [r, c] = tiles ! (r, c) == 0

    game' =
      if null openTiles
      then Lost g
      else game{gameTiles=tiles'}
        where
          ([r, c], g') = pickRandom openTiles g
          tiles' = setElem 2 (r, c) tiles

winValue :: Int
winValue = 2048

checkWin :: GameData -> GameData
checkWin game@Won{}  = game
checkWin game@Lost{} = game
checkWin game@Game{gameTiles=tiles} =
  if winValue `elem` toList tiles
    then Won $ randomGen game
    else game

data GameAction = ShiftLeft | ShiftRight | ShiftUp | ShiftDown | Quit

iterateGame :: GameAction -> GameData -> Maybe GameData
iterateGame _      g@Won{}  = Just g
iterateGame _      g@Lost{} = Just g
iterateGame Quit   g        = Just $ Lost $ randomGen g
iterateGame action g@Game{gameTiles=tiles} = game'
  where 
    shiftedTiles = case action of
      ShiftLeft  -> shiftLeft tiles
      ShiftRight -> shiftRight tiles
      ShiftUp    -> shiftUp tiles
      ShiftDown  -> shiftDown tiles

    game' = if shiftedTiles == tiles
      then Nothing
      else Just $ checkWin $ addTile g{gameTiles=shiftedTiles}

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


-- Utility functions
randomRn :: Random a => (a, a) -> Int -> StdGen -> ([a], StdGen)
randomRn lh n = runState (replicateM n (state $ randomR lh))

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

textLoop :: GameData -> IO GameData
textLoop g@Won{} = do
  print g
  return g
textLoop g@Lost{} = do
  print g
  return g
textLoop g@Game{} = do
  print g
  getChLoop g >>= return . addTile >>= textLoop
  where
    getChLoop :: GameData -> IO GameData
    getChLoop g = do
      ch <- getChar
      case lookup ch actions of
        Just action -> case iterateGame action g of
          Just g' -> return g'
          Nothing -> getChLoop g
        Nothing     -> getChLoop g
    actions :: [(Char, GameAction)]
    actions =
      [ ('q', Quit)
      , ('w', ShiftUp)
      , ('a', ShiftLeft)
      , ('s', ShiftDown)
      , ('d', ShiftRight)
      ]

main :: IO ()
main = do
  randomGen <- getStdGen
  textLoop $ newGame (4, 4) randomGen
  return ()
