module Go.Stone
    ( Stone(..)
    , yinYang
    ) where

data Stone = Black | White deriving (Eq, Show, Read, Enum, Bounded)

yinYang :: Stone -> Stone
yinYang Black = White
yinYang White = Black
