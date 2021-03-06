module Go.Board
    ( Board
    , Pos
    , Group
    --
    , (!)
    , dim
    , empty
    , insert'
    , lookup
    , unsafePrint
    --
    , neighbors
    , neighbors'
    , group
    , liberty
    , liberties
    --
    , null
    , size
    --
    , singleton
    , singleton'
    --
    , insert
    , insertWith
    , insertWith'
    --
    , delete
    , adjust
    , adjust'
    , adjustWithKey
    , adjustWithKey'
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    --
    , map
    , map'
    --
    , elems
    , elems'
    , keys
    --
    , toList
    , toList'
    , fromList
    , fromList'
    , filter
    , filterWithKey
    ) where

import Prelude hiding (lookup, null, map, filter)
import qualified Prelude as P
import qualified Data.List as L

import Data.Word
import Data.Maybe
import Control.Applicative hiding (empty)
import Control.Arrow (second)

import Go.Stone
import Go.Board.FFI (Board, Pos, (!), dim, empty, insert', lookup, unsafePrint)


type Group = ([Pos], Stone)


match :: Board -> Pos -> (Pos, Maybe Stone)
match b yx = (yx, lookup yx b)


stonyList :: [(Pos, Maybe Stone)] -> [(Pos, Stone)]
stonyList = foldr accumMay []
    where accumMay (yx, Just s) es = (yx, s) : es
          accumMay (_, Nothing) es = es


{--------------------------------------------------------------------
Adjacent
--------------------------------------------------------------------}


-- | /O(4)/
neighbors :: Pos -> Board -> [(Pos, Stone)]
neighbors (y,x) b = let match' = match b
                        u = match' (y-1, x)
                        l = match' (y,   x-1)
                        d = match' (y+1, x)
                        r = match' (y,   x+1)
                    in stonyList [u,l,r,d]


-- | /O(4)/
neighbors' :: Pos -> Board -> [(Pos, Maybe Stone)]
neighbors' (y,x) b = let match' = match b
                         u = match' (y-1, x)
                         l = match' (y,   x-1)
                         d = match' (y+1, x)
                         r = match' (y,   x+1)
                         b_d = dim b
                    in P.filter (\((y,x),_) -> y < b_d && x < b_d) [u,l,r,d]


--
group :: Pos -> Board -> Maybe Group
group yx b = (flip fmap $ lookup yx b) $ \stone -> (search stone [] [yx], stone)
 where
    search :: Stone -> [Pos] -> [Pos] -> [Pos]
    search stone visited stack = case stack of
        []         -> visited
        (pos:rest) -> search stone (pos:visited) $ (newNeighbors stone pos stack visited)
                                                   ++ rest
    newNeighbors :: Stone -> Pos -> [Pos] -> [Pos] -> [Pos]
    newNeighbors stone pos stack visited = P.map P.fst $
        P.filter (\(pos', st) -> st == stone
                              && P.not (P.elem pos' stack)
                              && P.not (P.elem pos' visited))
                 (neighbors pos b)


--
liberty :: Pos -> Board -> [Pos]
liberty yx b = P.map fst $ P.filter (isNothing . snd) (neighbors' yx b)


--
liberties :: [Pos] -> Board -> [Pos]
liberties ps b = L.nub $ L.concat $ P.map (flip liberty b) ps


{--------------------------------------------------------------------
Query
--------------------------------------------------------------------}


-- | /O(dim^2)/
null :: Board -> Bool
null b = length (elems b) == 0


-- | /O(dim^2)/
size :: Board -> Int
size b = length (elems b)


{--------------------------------------------------------------------
Construction
--------------------------------------------------------------------}


-- | /O(1)/
singleton :: Word8 -> Stone -> Pos -> Board
singleton d s = singleton' d (Just s)


-- | /O(1)/
singleton' :: Word8 -> Maybe Stone -> Pos -> Board
singleton' d ms yx = insert' yx ms (empty d)


{--------------------------------------------------------------------
Instruction
--------------------------------------------------------------------}


-- | /O(1)/
insert :: Pos -> Stone -> Board -> Board
insert yx s = insert' yx (Just s)


-- | /O(1)/
-- > insertWith (\new old -> ..) (y,x) stone board
insertWith :: (Stone -> Stone -> Stone) -> Pos -> Stone -> Board -> Board
insertWith f yx s b = insert yx (case lookup yx b of Nothing -> s; Just old -> f s old) b


-- | /O(1)/
-- > insertWith' (\mNew mOld -> ..) (y,x) mStone board
insertWith' :: (Maybe Stone -> Maybe Stone -> Maybe Stone) -> Pos -> Maybe Stone -> Board
            -> Board
insertWith' f yx ms b = insert' yx (f ms (lookup yx b)) b


{--------------------------------------------------------------------
Delete/Update
--------------------------------------------------------------------}


-- | /O(1)/
delete :: Pos -> Board -> Board
delete yx = insert' yx Nothing


-- | /O(1)/
adjust :: (Stone -> Stone) -> Pos -> Board -> Board
adjust f yx b = case lookup yx b of Nothing -> b; Just s -> insert yx (f s) b


-- | /O(1)/
adjust' :: (Maybe Stone -> Maybe Stone) -> Pos -> Board -> Board
adjust' f yx b = insert' yx (f (lookup yx b)) b


-- | /O(1)/
adjustWithKey :: (Pos -> Stone -> Stone) -> Pos -> Board -> Board
adjustWithKey f yx b = case lookup yx b of Nothing -> b; Just s -> insert yx (f yx s) b


-- | /O(1)/
adjustWithKey' :: (Pos -> Maybe Stone -> Maybe Stone) -> Pos -> Board -> Board
adjustWithKey' f yx b = insert' yx (f yx (lookup yx b)) b


-- | /O(1)/
update :: (Stone -> Maybe Stone) -> Pos -> Board -> Board
update f yx b = case lookup yx b of Nothing -> b; Just s -> insert' yx (f s) b


-- | /O(1)/
updateWithKey :: (Pos -> Stone -> Maybe Stone) -> Pos -> Board -> Board
updateWithKey f yx b = case lookup yx b of Nothing -> b; Just s -> insert' yx (f yx s) b


-- | /O(1)/
updateLookupWithKey :: (Pos -> Stone -> Maybe Stone) -> Pos -> Board
                    -> (Maybe Stone, Board)
updateLookupWithKey f yx b = case lookup yx b of Nothing -> (Nothing, b)
                                                 Just s -> let ms = f yx s
                                                           in (ms, insert' yx ms b)


-- | /O(1)/
alter :: (Maybe Stone -> Maybe Stone) -> Pos -> Board -> Board
alter f yx b = insert' yx (f (lookup yx b)) b


{--------------------------------------------------------------------
Traversal
--------------------------------------------------------------------}


map :: (Stone -> Stone) -> Board -> Board
map f b = fromList (dim b) $ P.map (second f) (toList b)


map' :: (Maybe Stone -> Maybe Stone) -> Board -> Board
map' f b = fromList' (dim b) $ P.map (second f) (toList' b)


{--------------------------------------------------------------------
Conversion
--------------------------------------------------------------------}


-- TODO: don't use `toList'
-- | /O(dim^2)/
elems :: Board -> [Stone]
elems = P.map snd . toList


-- | /O(dim^2)/
elems' :: Board -> [Maybe Stone]
elems' = P.map snd . toList'


-- | /O(dim^2)/
keys :: Board -> [Pos]
keys = P.map fst . toList


{--------------------------------------------------------------------
Lists
--------------------------------------------------------------------}


-- | /O(dim^2)/
toList :: Board -> [(Pos, Stone)]
toList = stonyList . toList'


-- | /O(dim^2)/
toList' :: Board -> [(Pos, Maybe Stone)]
toList' b = [match' (y,x) | y <- init [0..d], x <- init [0..d]]
    where d = dim b
          match' = match b


-- | /O(n)/
fromList :: Word8 -> [(Pos, Stone)] -> Board
fromList d = foldl (\b (yx,s) -> insert yx s b) (empty d)


-- | /O(n)/
fromList' :: Word8 -> [(Pos, Maybe Stone)] -> Board
fromList' d = foldl (\b (yx,ms) -> insert' yx ms b) (empty d)


{--------------------------------------------------------------------
Filter
--------------------------------------------------------------------}


-- | /O(dim^2)/
filter :: (Stone -> Bool) -> Board -> Board
filter f b =
    let d = dim b
    in foldr (\yx b' -> fromMaybe b' $ fmap (\s -> if f s then b' else delete yx b')
                                            (lookup yx b'))
             b
             [(y,x) | y <- init [0..d], x <- init [0..d]]


-- | /O(dim^2)/
filterWithKey :: (Pos -> Stone -> Bool) -> Board -> Board
filterWithKey f b =
    let d = dim b
    in foldr (\yx b' -> fromMaybe b' $ fmap (\s -> if f yx s then b' else delete yx b')
                                            (lookup yx b'))
             b
             [(y,x) | y <- init [0..d], x <- init [0..d]]

