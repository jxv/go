module Go.Board
    ( module Go.Board.FFI
    , neighbors
    , toList
    ) where

import Prelude hiding (lookup)

import Data.Word

import Go.Stone
import Go.Board.FFI (Board, (!), dim, empty, insert, lookup)


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


toList :: Board -> [((Word8, Word8), Stone)]
toList b = stonyList [match' (y,x) | y <- init [0..d], x <- init [0..d]]
    where d = dim b
          match' = match b

