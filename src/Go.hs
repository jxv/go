module Go
    ( module Go.Stone
    , module Go.Board
    ) where

import Prelude hiding (lookup)

import Go.Stone (Stone(..), yinYang)
import Go.Board (Board, (!), dim, empty, insert, insert', lookup, neighbors)
