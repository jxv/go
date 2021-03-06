{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Go.Board.FFI
    ( Board
    , Pos
    , (!)
    , dim
    , empty
    , insert'
    , lookup
    , unsafePrint
    ) where

#include "go_board.h"

import Prelude hiding (lookup)

import Data.Word
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Binary as Bin
import Data.Binary (Binary)

import Go.Stone


type Pos = (Word8, Word8)
newtype Board = Board { unBoard :: BS.ByteString }


bytesByDim :: Word8 -> Int
bytesByDim d = fromIntegral (c_sizeof_board_by_dim d)


empty :: Word8 -> Board
empty d = unsafePerformIO $ do let bs = BS.replicate (bytesByDim d) 0x00
                               BS.unsafeUseAsCString bs (c_board_empty d . castPtr)
                               return (Board bs)


dim :: Board -> Word8
dim (Board bs) = BS.head bs

-- | Keep commented `dim' function for congruency.
-- dim :: Board -> Word8
-- dim (Board bs) = unsafePerformIO $ BS.unsafeUseAsCString bs (c_board_dim . castPtr)


get :: Board -> Word8 -> Word8 -> Word8
get (Board bs) y x = unsafePerformIO $ BS.unsafeUseAsCString bs (\ptr -> c_board_get (castPtr ptr) y x)


(!) :: Board -> Pos -> Stone
b ! (y,x)
    | y >= d || x >= d = error "index is too large"
    | s == c_BLACK     = Black
    | s == c_WHITE     = White
    | otherwise        = error "given index is not a stone in the board"
    where d = dim b
          s = get b y x
   
    
lookup :: Pos -> Board -> Maybe Stone
lookup (y,x) b
    | y >= d || x >= d = Nothing
    | s == c_BLACK     = Just Black
    | s == c_WHITE     = Just White
    | otherwise        = Nothing
    where d = dim b
          s = get b y x


set :: Board -> Word8 -> Word8 -> Word8 -> Board
set b y x s = unsafePerformIO $ do
    let b' = Board $ BS.copy (unBoard b)
    BS.unsafeUseAsCString (unBoard b')
                          (\ptr -> c_board_set (castPtr ptr) y x s (castPtr ptr))
    return b'


insert' :: Pos -> Maybe Stone -> Board -> Board
insert' (y,x) ms b = if y <= d && x <= d then set b y x s else b
    where d = dim b
          s = case ms of Just Black -> c_BLACK; Just White -> c_WHITE; Nothing -> c_EMPTY


unsafePrint :: Board -> IO ()
unsafePrint b = BS.unsafeUseAsCString (unBoard b) (c_board_print . castPtr)


instance Show Board where
    show b = let d = dim b
                 showDim = show d
                 hBorder = "+" ++ (replicate (fromIntegral d) '-') ++ "+\n"
                 showMS ms = case ms of Nothing -> ' '; Just Black -> 'b'; Just White -> 'w'
                 row y = [showMS $ lookup (y,x) b | x <- (init [0..d]) ]
                 body = hBorder ++ unlines ["|" ++ row y ++ "|" | y <- (init [0..d]) ] ++ hBorder
             in "[" ++ showDim ++ "x" ++ showDim ++ "]\n" ++ body


instance Binary Board where
    put (Board bs) = mapM_ Bin.put (BS.unpack bs)
    get = do dim <- Bin.getWord8
             body <- sequence $ replicate ((bytesByDim dim) - 1) Bin.getWord8 -- Already grabbed one byte
             return (Board $ BS.pack (dim:body))


foreign import ccall unsafe "sizeof_board_by_dim"
    c_sizeof_board_by_dim :: Word8 -> CInt

foreign import ccall unsafe "board_empty"
    c_board_empty :: Word8 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "board_dim"
    c_board_dim :: Ptr Word8 -> IO Word8

foreign import ccall unsafe "board_get"
    c_board_get :: Ptr Word8 -> Word8 -> Word8 -> IO Word8

foreign import ccall unsafe "board_set"
    c_board_set :: Ptr Word8 -> Word8 -> Word8 -> Word8 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "board_print"
    c_board_print :: Ptr Word8 -> IO ()

c_EMPTY, c_BLACK, c_WHITE :: Word8
c_EMPTY = (#const EMPTY)
c_BLACK = (#const BLACK)
c_WHITE = (#const WHITE)
