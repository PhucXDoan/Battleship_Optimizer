-- @TODO@
-- @performance@ outputting to console is SLOW
-- @hygine@ Tile system is kinda weird and also Grid
-- @improve@ a better algorithm to calculate the next best move
-- @improve@ more safety checks
-- @sticky@ have a function "propagate" that counts how many damaged tiles there are in a chain and the positions; must be fast as possible
{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Data.List
import Data.Either
import Data.Function
import Data.Functor
import Data.Char
import System.IO
import Text.Read
import Auxiliary

data Result   = Empty | Damaged | Sunken deriving (Show, Eq, Read)
type Tile     = Either Int Result
type Grid     = [[Tile]]
type Ship     = Int
type Registry = [(Int, Ship)]
type Position = (Int, Int)

render :: Registry -> Grid -> String
render registry grid =
    intercalate "\n" $
        [
            margin,
            margin ++ (concat $ map (\c -> "   " ++ [c] ++ "  ") $ take dimension ['A' .. 'Z']),
            margin,
            divider
        ] ++ [
            intercalate "\n"
                $ map concat
                $ transpose
                $ ["    |", " " ++ padStart 2 ' ' (show $ y + 1) ++ " |", "    |"] : [renderLinesOfTile $ tileAt grid (x, y) | x <- indices]
            | y <- indices
        ] ++ [
            divider,
            margin ++ " " ++ (intercalate " | " $ map (\(shipCount, shipLength) -> show shipLength ++ "-ship(s): " ++ show shipCount) registry),
            divider
        ]
    where dimension = length grid
          indices   = [0 .. dimension - 1]
          divider   = replicate (5 + 6 * dimension) '-'
          margin    = "    |"
          renderLinesOfTile (Left  n)       = ["      ", "  " ++ padStart 2 '0' (show n) ++ "  ", "      "]
          renderLinesOfTile (Right Empty)   = ["      ", "      ", "      "]
          renderLinesOfTile (Right Damaged) = ["  **  ", "******", "  **  "]
          renderLinesOfTile (Right Sunken)  = ["XXXXXX", "XXXXXX", "XXXXXX"]

tileAt :: Grid -> Position -> Tile
tileAt grid (x, y) = grid !! y !! x

tileAtMaybe :: Grid -> Position -> Maybe Tile
tileAtMaybe grid (x, y) = grid `atMaybe` y >>= (`atMaybe` x)

tilesAt :: Grid -> [Position] -> [Tile]
tilesAt _ [] = []
tilesAt grid (p:ps) =
    maybe rest (: rest) (grid `tileAtMaybe` p)
    where rest = grid `tilesAt` ps

trySetTile :: Tile -> Position -> Grid -> Grid
trySetTile tile (x, y) grid =
    grid `atMaybe` y <&> trySet tile x & maybe grid (\row -> trySet row y grid)

trySetTiles :: Tile -> [Position] -> Grid -> Grid
trySetTiles _    []     grid = grid
trySetTiles tile (p:ps) grid = trySetTiles tile ps $ trySetTile tile p grid

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (x, y) =
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getCornerPositions :: Position -> [Position]
getCornerPositions (x, y) =
    [(x + dx, y + dy) | dx <- [-1, 1], dy <- [-1, 1]]

getNeighborsOfPositions :: [Position] -> [Position]
getNeighborsOfPositions ps =
    ps
        >>= (\p -> getAdjacentPositions p ++ getCornerPositions p)
        & filter (not . (`elem` ps))
        & nub

getAttackData :: Grid -> IO (Position, Result)
getAttackData grid =
    do putStr "Coordinates and Result: "
       hFlush stdout
       getLine
           <&> words
           <&> ensure ((== 2) . length)
           <&> (<&> \[file:strRank, c:cs] -> (toLower file, strRank, toUpper c : map toLower cs))
           <&> (>>= \(file, strRank, strResult) ->
                        do let x = ord file - ord 'a'
                           y <- readMaybe strRank <&> subtract 1
                           result <- readMaybe strResult
                           guard $ any isLeft $ tileAtMaybe grid (x, y)
                           pure $ ((x, y), result)
               )
           >>= maybe (getAttackData grid) pure

updateSinkShip :: Registry -> Position -> Grid -> (Registry, Grid)
updateSinkShip registry sunkenPosition grid =
    (
        tryFindMap ((== length damagedPositions + 1) . snd) (mapFst (subtract 1)) registry,
        trySetTiles (Right Empty) neighborPositions $ trySetTiles (Right Sunken) damagedPositions grid
    )
    where damagedPositions =
              applySuccessivelyUntil
                  nonDisjoint
                  (\positions ->
                      positions
                          >>= getAdjacentPositions
                          & (++ positions)
                          & filter ((== Just (Right Damaged)) . tileAtMaybe grid)
                          & nub
                  )
                  [sunkenPosition]
          neighborPositions = getNeighborsOfPositions (sunkenPosition:damagedPositions)

markCornersEmpty :: Position -> Grid -> Grid
markCornersEmpty = trySetTiles (Right Empty) . getCornerPositions

canShipFitHorizontally :: Position -> Grid -> Ship -> Bool
canShipFitHorizontally (x, y) grid shipLength =
    length shipTiles == shipLength &&
    all (\tile -> isLeft tile || tile == Right Damaged) shipTiles &&
    all (/= Right Damaged) neighborTiles
    where shipPositions = [(x', y) | x' <- [x .. x + shipLength - 1]]
          shipTiles = tilesAt grid shipPositions
          neighborTiles = tilesAt grid $ getNeighborsOfPositions shipPositions

canShipFitVertically :: Position -> Grid -> Ship -> Bool
canShipFitVertically (x, y) grid =
    canShipFitHorizontally (y, x) (transpose grid)

calculateShipCountsInGrid :: Registry -> Grid -> Grid
calculateShipCountsInGrid registry grid =
    [[if isLeft tile then determine (x, y) else tile | x <- indices, let tile = tileAt grid (x, y)] | y <- indices]
    where indices = [0 .. length grid - 1]
          determine p = Left $ sum $ map (count p . snd) $ filter ((0 <) . fst) registry
          count (x, y) shipLength =
              (length $ filter (\p -> canShipFitHorizontally p grid shipLength) [(x', y) | x' <- [x - shipLength + 1 .. x]]) +
              (if shipLength /= 1
               then length $ filter (\p -> canShipFitVertically p grid shipLength) [(x, y') | y' <- [y - shipLength + 1 .. y]]
               else 0
              )

getBestMove :: Grid -> String
getBestMove grid =
    [(x, y) | x <- indices, y <- indices]
        & filter (isLeft . tileAt grid)
        & map (\p -> (p, extractLeft $ tileAt grid p))
        & ensure (/= [])
        <&> foldl1 (\bestPair pair -> if snd bestPair < snd pair then pair else bestPair)
        & maybe "N/A" (\((x, y), n) -> chr (ord 'a' + x) : show (y + 1) ++ " with a ship count of: " ++ show n)
    where indices = [0 .. length grid - 1]

possibleShips :: [Ship]
possibleShips = [1..4]

program :: Registry -> Grid -> IO ()
program registry grid =
    do putStrLn $ render registry grid
       putStrLn $ getBestMove grid

       (position, result) <- getAttackData grid

       let settedGrid = trySetTile (Right result) position grid
       let (registry', grid') = case result of
                                    Sunken  -> updateSinkShip registry position settedGrid
                                    Damaged -> (registry, markCornersEmpty position settedGrid)
                                    _       -> (registry, settedGrid)
       program registry' (calculateShipCountsInGrid registry' grid')

main :: IO ()
main =
    do registry <-
           possibleShips
               & map (\shipLength -> getReadTested ("Amount of " ++ show shipLength ++ "-long ships: ") (0 <=))
               & sequence
               <&> flip zip possibleShips
       dimension <- getReadTested "Dimensions of grid: " (\n -> 0 < n && n <= 26)
       program registry $ calculateShipCountsInGrid registry $ replicate dimension $ replicate dimension $ Left 0

