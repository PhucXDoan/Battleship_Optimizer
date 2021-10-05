import Data.Maybe
import Data.Functor
import Data.Function
import Data.Char
import Data.List
import System.IO
import Text.Read

---

data Result = Empty | Damaged | Sunken
    deriving (Eq, Show, Read)

data Tile = Count Int | Likely Int | Known Result
    deriving Eq

data Grid = Grid Int [Tile]

type Position = (Int, Int)
type Registry = [(Int, Int)]

---

tileAtMaybe :: Grid -> Position -> Maybe Tile
tileAtMaybe (Grid dimension tiles) (x, y) =
    if 0 <= x && x < dimension && 0 <= y && y < dimension
    then Just $ tiles !! (x + y * dimension)
    else Nothing

-- tileAt :: Grid -> Position -> Tile
-- tileAt grid position =
--     fromJust $ grid `tileAtMaybe` position

tilesAt :: Grid -> [Position] -> [Tile]
tilesAt grid = catMaybes . map (tileAtMaybe grid)

trySetTile :: Tile -> Position -> Grid -> Grid
trySetTile tile (x, y) (Grid dimension tiles) =
    if 0 <= x && x < dimension && 0 <= y && y < dimension
    then Grid dimension $ take (x + y * dimension) tiles ++ [tile] ++ drop (x + y * dimension + 1) tiles
    else Grid dimension tiles

trySetTiles :: Tile -> [Position] -> Grid -> Grid
trySetTiles _    []     = id
trySetTiles tile (p:ps) = trySetTiles tile ps . trySetTile tile p

calcAdjacents :: Position -> [Position]
calcAdjacents (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

calcCorners :: Position -> [Position]
calcCorners (x, y) = [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]

calcNeighbors :: Position -> [Position]
calcNeighbors p = calcAdjacents p ++ calcCorners p

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x =
    if p x
    then Just x
    else Nothing

getReadTested :: Read a => String -> (a -> Bool) -> IO a
getReadTested message predicate =
    do putStr message
       hFlush stdout
       getLine
           <&> readMaybe
           <&> (>>= ensure predicate)
           >>= maybe (getReadTested message predicate) pure

getResult :: IO Result
getResult =
    do putStr "Result: "
       hFlush stdout
       getLine
           <&> words
           <&> ensure ((== 1) . length)
           <&> (<&> (\[c:cs] -> toUpper c : map toLower cs))
           <&> (>>= readMaybe)
           >>= maybe getResult pure

padStart :: Int -> a -> [a] -> [a]
padStart n x xs = replicate (n - length xs) x ++ xs

inChunks :: Int -> [a] -> [[a]]
inChunks _ [] = []
inChunks n xs = take n xs : inChunks n (drop n xs)

isTileKnown :: Tile -> Bool
isTileKnown (Known _) = True
isTileKnown _         = False

isTileLikely :: Tile -> Bool
isTileLikely (Likely _) = True
isTileLikely _          = False

extractTile :: Tile -> Int
extractTile (Count  n) = n
extractTile (Likely n) = n
extractTile _          = undefined

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

zipGrid :: Grid -> [(Position, Tile)]
zipGrid (Grid dimension tiles) =
    [((x, y), tiles !! (x + y * dimension)) | y <- indices, x <- indices]
    where indices = [0 .. dimension - 1]

imapGrid :: (Position -> Tile -> Tile) -> Grid -> Grid
imapGrid f (Grid dimension tiles) =
    Grid dimension [f (x, y) (tiles !! (x + y * dimension)) | y <- indices, x <- indices]
    where indices = [0 .. dimension - 1]

-- applyWhen :: Bool -> (a -> a) -> a -> a
-- applyWhen True  f = f
-- applyWhen False _ = id

-- swap :: (a, b) -> (b, a)
-- swap (x, y) = (y, x)

transposeGrid :: Grid -> Grid
transposeGrid (Grid dimension tiles) =
    Grid dimension $ concat $ transpose $ inChunks dimension tiles

tryFindMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
tryFindMap _ _ []     = []
tryFindMap p f (x:xs) =
    if p x
    then f x : xs
    else   x : tryFindMap p f xs

---

render :: Registry -> Grid -> IO ()
render registry (Grid dimension tiles) =
    do putStrLn divider
       putStrLn emptyFiles
       putStrLn $ "|      |" ++ concat [" <" ++ [file, file] ++ "> |" | file <- take dimension ['A'..'Z']]
       putStrLn emptyFiles
       putStrLn divider
       putStrLn
           $ intercalate "\n"
           $ intersperse divider
           $ map (intercalate "\n" . map (concat . map (++ "|")) . transpose)
           $ map (\(rank, row) -> ["|      ", "| <" ++ seralizeInt rank ++ "> ", "|      "] : row)
           $ zip [1..]
           $ inChunks dimension
           $ map calcRenderLines tiles
       putStrLn divider
       putStrLn $ intercalate " | " $ map (\(count, shipLength) -> show shipLength ++ "-length ship(s): " ++ show count) registry
       putStrLn divider
    where divider     = replicate (7 * dimension + 8) '-'
          emptyFiles  = "|      |" ++ concat (replicate dimension ("      |"))
          seralizeInt = padStart 2 '0' . show
          calcRenderLines (Count n)       = ["======", "==" ++ seralizeInt n ++ "==", "======"]
          calcRenderLines (Likely n)      = ["  !!  ", "!!" ++ seralizeInt n ++ "!!", "  !!  "]
          calcRenderLines (Known Empty)   = replicate 3 $ replicate 6 ' '
          calcRenderLines (Known Damaged) = ["  **  ", "******", "  **  "]
          calcRenderLines (Known Sunken)  = replicate 3 $ replicate 6 'X'

calcBestMove :: Grid -> Maybe Position
calcBestMove grid =
    grid
        & zipGrid
        & filter (not . isTileKnown . snd)
        & (\xs -> let xs' = filter (isTileLikely . snd) xs in if xs' == [] then xs else xs')
        & map (mapSnd extractTile)
        & ensure (/= [])
        <&> foldl1 (\p q -> if snd p < snd q then q else p)
        <&> fst

sinkHorizontalShip :: Position -> Registry -> Grid -> (Registry, Grid)
sinkHorizontalShip (x, y) registry grid =
    (
        tryFindMap ((== length shipPositions) . snd) (mapFst (subtract 1)) registry,
        trySetTiles (Known Empty) neighborPositions $ trySetTiles (Known Sunken) shipPositions grid
    )
    where shipPositions =
              [1..]
                  <&> (\dx -> [(x - dx, y), (x + dx, y)])
                  <&> filter ((== Just (Known Damaged)) . tileAtMaybe grid)
                  & takeWhile (/= [])
                  & concat
                  & ((x, y) :)
                  & sortBy (\(x0, _) (x1, _) -> compare x0 x1)
          neighborPositions =
              shipPositions
                  >>= (\(px, py) -> [(px, py - 1), (px, py + 1)])
                  & ([(px + dx, py + dy) | (dx, (px, py)) <- [(-1, head shipPositions), (1, last shipPositions)], dy <- [-1, 0, 1]] ++)

sinkShip :: Position -> Registry -> Grid -> (Registry, Grid)
sinkShip (x, y) registry grid =
    if any ((== Just (Known Damaged)) . tileAtMaybe grid) [(x - 1, y), (x + 1, y)]
    then sinkHorizontalShip (x, y) registry grid
    else mapSnd transposeGrid $ sinkHorizontalShip (y, x) registry (transposeGrid grid)

canHorizontalShipFit :: Grid -> Int -> Position -> Bool
canHorizontalShipFit grid shipLength (x, y) =
    length shipTiles == shipLength &&
    all (\tile -> (not . isTileKnown) tile || tile == Known Damaged) shipTiles &&
    all (/= Known Damaged) neighborTiles
    where shipPositions = [(x + dx, y) | dx <- [0 .. shipLength - 1]]
          shipTiles = grid `tilesAt` shipPositions
          neighborTiles =
              shipPositions
                  >>= (\(px, py) -> [(px, py - 1), (px, py + 1)])
                  & ([(px + dx, py + dy) | (dx, (px, py)) <- [(-1, head shipPositions), (1, last shipPositions)], dy <- [-1, 0, 1]] ++)
                  & tilesAt grid

canVerticalShipFit :: Grid -> Int -> Position -> Bool
canVerticalShipFit grid shipLength (x, y) =
    canHorizontalShipFit (transposeGrid grid) shipLength (y, x)

calcShipCount :: Registry -> Position -> Grid -> Int
calcShipCount registry (x, y) grid =
    do shipLength <- map snd $ filter ((1 <=) . fst) registry
       if shipLength == 1
       then do if any (== Known Damaged) $ grid `tilesAt` (calcNeighbors (x, y))
               then do pure 0
               else do pure 1
       else do let hCount = length $ filter (canHorizontalShipFit grid shipLength) [(x + dx, y) | dx <- [-shipLength + 1 .. 0]]
               let vCount = length $ filter (canVerticalShipFit grid shipLength) [(x, y + dy) | dy <- [-shipLength + 1 .. 0]]
               pure $ hCount + vCount
    & sum

updateUnknownTiles :: Registry -> Grid -> Grid
updateUnknownTiles registry grid =
    imapGrid
        (\position tile ->
            if isTileKnown tile
            then tile
            else let shipCount = calcShipCount registry position grid
                 in if shipCount == 0
                    then Known Empty
                    else if any (== Known Damaged) $ grid `tilesAt` (calcAdjacents position)
                    then Likely shipCount
                    else Count shipCount
        ) grid

program :: Registry -> Grid -> IO ()
program registry grid =
    do render registry grid
       case calcBestMove grid of
           Nothing ->
               do putStrLn "No more possible moves"
           Just bestMove ->
               do putStrLn $ "Best move is at: " ++ [chr (ord 'A' + fst bestMove)] ++ show (snd bestMove + 1)
                  result <- getResult
                  let settedGrid = trySetTile (Known result) bestMove grid
                  let (registry', reactedGrid) = case result of
                                                     Empty   -> (registry, settedGrid)
                                                     Damaged -> (registry, trySetTiles (Known Empty) (calcCorners bestMove) settedGrid)
                                                     Sunken  -> sinkShip bestMove registry settedGrid
                  program registry' $ updateUnknownTiles registry' reactedGrid

main :: IO ()
main =
    do dimension <- getReadTested "Dimensions of grid: " (\n -> 1 <= n && n <= 26)
       registry <-
           shipLengths
               & map (\shipLength -> getReadTested ("Amount of " ++ show shipLength ++ "-long ship(s): ") (\n -> 0 <= n && n <= 9))
               & sequence
               <&> (`zip` shipLengths)
       program registry $ updateUnknownTiles registry $ Grid dimension $ replicate (dimension ^ (2 :: Int)) (Count 0)
    where shipLengths = [1, 2, 3, 4]

