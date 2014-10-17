module Go
    ( module Go.Stone
    , module Go.Board
    ) where

import Prelude hiding (lookup)

import Go.Stone (Stone(..))
import Go.Board (Board, (!), dim, empty, insert, lookup)
