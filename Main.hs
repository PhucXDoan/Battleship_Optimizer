{-# LANGUAGE MultiWayIf #-}
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

data Tile = Count Int | Known Result
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

isKnown :: Tile -> Bool
isKnown (Known _) = True
isKnown _         = False

extractTile :: Tile -> Int
extractTile (Count n) = n
extractTile _         = undefined

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
          calcRenderLines (Known Empty)   = replicate 3 $ replicate 6 ' '
          calcRenderLines (Known Damaged) = ["  **  ", "******", "  **  "]
          calcRenderLines (Known Sunken)  = replicate 3 $ replicate 6 'X'

-- @speed@ make Count tiles have info that says that they are high-priority
calcBestMove :: Grid -> Maybe Position
calcBestMove grid =
    usingList
        & ensure (/= [])
        <&> foldl1 (\p q -> if snd p < snd q then q else p)
        <&> fst
    where zippedGrid = zipGrid grid
          adjacentToDamagedPositions =
              zippedGrid
                  & filter ((== Known Damaged) . snd)
                  >>= calcAdjacents . fst
                  <&> (\position ->
                           grid `tileAtMaybe` position
                               >>= ensure (not . isKnown)
                               <&> extractTile
                               <&> (\n -> (position, n))
                      )
                  & catMaybes
          usingList =
              if adjacentToDamagedPositions == []
              then map (mapSnd extractTile) $ filter (not . isKnown . snd) zippedGrid
              else adjacentToDamagedPositions

sinkShipH :: Position -> Registry -> Grid -> (Registry, Grid)
sinkShipH (x, y) registry grid =
    (
        tryFindMap ((== length shipPositions) . snd) (mapFst (subtract 1)) registry,
        grid
            & trySetTiles (Known Sunken) shipPositions
            & trySetTiles (Known Empty)  neighborPositions
    )
    where shipPositions =
              [1..]
                  <&> (\dx -> [(x - dx, y), (x + dx, y)])
                  <&> filter ((== Just (Known Damaged)) . tileAtMaybe grid)
                  & takeWhile (/= [])
                  & concat
                  & ((x, y) :)
                  & sortBy (\(px, _) (qx, _) -> compare px qx)
          neighborPositions =
              shipPositions
                  >>= (\(px, py) -> [(px, py - 1), (px, py + 1)])
                  & ([(px + dx, py + dy) | (dx, (px, py)) <- [(-1, head shipPositions), (1, last shipPositions)], dy <- [-1, 0, 1]] ++)

sinkShip :: Position -> Registry -> Grid -> (Registry, Grid)
sinkShip (x, y) registry grid =
    if any (== Known Damaged) $ tilesAt grid [(x - 1, y), (x + 1, y)]
    then sinkShipH (x, y) registry grid
    else mapSnd transposeGrid $ sinkShipH (y, x) registry (transposeGrid grid)

canShipFitH :: Grid -> Int -> Position -> Bool
canShipFitH grid shipLength (x, y) =
    (shipPositions
         & tilesAt grid
         & ensure ((== shipLength) . length)
         & any (all (\tile -> (not . isKnown) tile || tile == Known Damaged))
    ) &&
    (shipPositions
         >>= (\(px, py) -> [(px, py - 1), (px, py + 1)])
         & (++ [(px, y + dy) | px <- [x - 1, x + shipLength], dy <- [-1, 0, 1]])
         & tilesAt grid
         & all (/= Known Damaged)
    )
    where shipPositions = [(x + dx, y) | dx <- [0 .. shipLength - 1]]

calcShipCount :: Registry -> Position -> Grid -> Int
calcShipCount registry position@(x, y) grid =
    registry
        & filter ((1 <=) . fst)
        <&> snd
        <&> calcShipCountOfLength
        & sum
    where calcShipCountOfLength 1 =
              position
                  & calcNeighbors
                  & tilesAt grid
                  & all (/= Known Damaged)
                  & fromEnum
          calcShipCountOfLength shipLength =
              [-shipLength + 1 .. 0]
                  <&> (\delta -> ((x + delta, y), (y + delta, x)))
                  >>= (\(p, q) -> [canShipFitH grid shipLength p, canShipFitH (transposeGrid grid) shipLength q])
                  & filter id
                  & length

updateUnknownTiles :: Registry -> Grid -> Grid
updateUnknownTiles registry grid =
    imapGrid
        (\position tile ->
            let shipCount = calcShipCount registry position grid
            in if | isKnown tile
                      -> tile
                  | shipCount == 0
                      -> Known Empty
                  | otherwise
                      -> Count shipCount
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

