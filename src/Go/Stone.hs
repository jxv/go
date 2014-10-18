module Go.Stone
    ( Stone(..)
    , yinYang
    ) where

data Stone = Black | White deriving (Eq, Show, Read, Enum, Ord, Bounded)

yinYang :: Stone -> Stone
yinYang Black = White
yinYang White = Black
