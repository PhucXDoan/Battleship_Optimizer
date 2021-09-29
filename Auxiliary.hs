module Auxiliary where
import Data.Functor
import Data.List
import System.IO
import Text.Read

padStart :: Int -> a -> [a] -> [a]
padStart n x xs = replicate (n - length xs) x ++ xs

atMaybe :: [a] -> Int -> Maybe a
atMaybe []     _ = Nothing
atMaybe (x:_)  0 = Just x
atMaybe (_:xs) i = atMaybe xs (i - 1)

trySet :: a -> Int -> [a] -> [a]
trySet _ _ [] = []
trySet x' 0 (_:xs) = x':xs
trySet x' i (x:xs) = x : trySet x' (i - 1) xs

tryFindMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
tryFindMap _ _ [] = []
tryFindMap p f (x:xs)
    | p x       = f x : xs
    | otherwise = x : tryFindMap p f xs

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

extractLeft :: Either a b -> a
extractLeft (Left x) = x
extractLeft _        = undefined

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x
    | p x       = Just x
    | otherwise = Nothing

applySuccessivelyUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
applySuccessivelyUntil p f x
    | p x x'    = x'
    | otherwise = applySuccessivelyUntil p f x'
    where x' = f x

nonDisjoint :: Eq a => [a] -> [a] -> Bool
nonDisjoint xs ys = xs \\ ys == [] && ys \\ xs == []

getReadTested :: Read a => String -> (a -> Bool) -> IO a
getReadTested message predicate =
    do putStr message
       hFlush stdout
       getLine
           <&> readMaybe
           <&> (>>= ensure predicate)
           >>= maybe (getReadTested message predicate) pure

