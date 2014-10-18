module Go.Board
    ( Board
    , (!)
    , dim
    , empty
    , lookup
    , neighbors
    , null
    , size
    , singleton
    , singleton'
    , insert
    , insert'
    , insertWith
    , insertWith'
    , delete
    , adjust
    , adjust'
    , toList
    , toList'
    , fromList
    , fromList'
    , elems
    , elems'
    ) where

import Prelude hiding (lookup, null)

import Data.Word

import Go.Stone
import Go.Board.FFI (Board, (!), dim, empty, insert', lookup)


match :: Board -> (Word8, Word8) -> ((Word8, Word8), Maybe Stone)
match b yx = (yx, lookup yx b)


stonyList :: [((Word8, Word8), Maybe Stone)] -> [((Word8, Word8), Stone)]
stonyList = foldr accumMay []
    where accumMay (yx, Just s) es = (yx, s) : es
          accumMay (_, Nothing) es = es


neighbors :: (Word8, Word8) -> Board -> [((Word8, Word8), Stone)]
neighbors (y,x) b = let match' = match b
                        u = match' (y-1, x)
                        l = match' (y,   x-1)
                        d = match' (y+1, x)
                        r = match' (y,   x+1)
                    in stonyList [u,l,r,d]


null :: Board -> Bool
null b = length (elems b) == 0


size :: Board -> Int
size b = length (elems b)


singleton :: Word8 -> Stone -> (Word8,Word8) -> Board
singleton d s = singleton' d (Just s)


singleton' :: Word8 -> Maybe Stone -> (Word8,Word8) -> Board
singleton' d ms yx = insert' yx ms (empty d)


insert :: (Word8, Word8) -> Stone -> Board -> Board
insert yx s = insert' yx (Just s)


-- | (yx, f new old)
insertWith :: (Stone -> Stone -> Stone) -> (Word8, Word8) -> Stone -> Board -> Board
insertWith f yx s b = insert yx (case lookup yx b of Nothing -> s; Just old -> f s old) b


insertWith' :: (Maybe Stone -> Maybe Stone -> Maybe Stone) -> (Word8, Word8) -> Maybe Stone -> Board -> Board
insertWith' f yx ms b = insert' yx (f ms (lookup yx b)) b


delete :: (Word8, Word8) -> Board -> Board
delete yx = insert' yx Nothing


adjust :: (Stone -> Stone) -> (Word8, Word8) -> Board -> Board
adjust f yx b = case lookup yx b of Nothing -> b; Just s -> insert yx (f s) b


adjust' :: (Maybe Stone -> Maybe Stone) -> (Word8, Word8) -> Board -> Board
adjust' f yx b = insert' yx (f (lookup yx b)) b


toList :: Board -> [((Word8, Word8), Stone)]
toList = stonyList . toList'


toList' :: Board -> [((Word8, Word8), Maybe Stone)]
toList' b = [match' (y,x) | y <- init [0..d], x <- init [0..d]]
    where d = dim b
          match' = match b


fromList :: Word8 -> [((Word8, Word8), Stone)] -> Board
fromList d = foldl (\b (yx,s) -> insert yx s b) (empty d)


fromList' :: Word8 -> [((Word8, Word8), Maybe Stone)] -> Board
fromList' d = foldl (\b (yx,ms) -> insert' yx ms b) (empty d)


-- TODO: don't use `toList'
elems :: Board -> [Stone]
elems = map snd . toList


elems' :: Board -> [Maybe Stone]
elems' = map snd . toList'
