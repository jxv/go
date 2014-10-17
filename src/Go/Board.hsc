{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Go.Board
    ( Board
    , (!)
    , dim
    , empty
    , insert
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

import Go.Stone


newtype Board = Board { unBoard :: BS.ByteString }


instance Show Board where
    show b = let d = dim b
                 showDim = show d
                 hBorder = "+" ++ (replicate (fromIntegral d) '-') ++ "+\n"
                 showMS ms = case ms of Nothing -> ' '; Just Black -> 'b'; Just White -> 'w'
                 row y = [showMS $ lookup (y,x) b | x <- (init [0..d]) ]
                 body = hBorder ++ unlines ["|" ++ row y ++ "|" | y <- (init [0..d]) ] ++ hBorder
             in "[" ++ showDim ++ "x" ++ showDim ++ "]\n" ++ body


empty :: Word8 -> Board
empty d = unsafePerformIO $ do let bs = BS.pack $ replicate (1 + (fromIntegral d) ^ 2) 0x00
                               BS.unsafeUseAsCString bs (c_board_empty d . castPtr)
                               return (Board bs)


dim :: Board -> Word8
dim (Board bs) = unsafePerformIO $ BS.unsafeUseAsCString bs (c_board_dim . castPtr)


get :: Board -> Word8 -> Word8 -> Word8
get (Board bs) y x = unsafePerformIO $ BS.unsafeUseAsCString bs (\ptr -> c_board_get (castPtr ptr) y x)


(!) :: Board -> (Word8, Word8) -> Stone
b ! (y,x)
    | y >= d || x >= d = error "index is too large"
    | s == c_BLACK     = Black
    | s == c_WHITE     = White
    | otherwise        = error "given index is not a stone in the board"
    where d = dim b
          s = get b y x
   
    
lookup ::(Word8, Word8) -> Board -> Maybe Stone
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


insert :: (Word8, Word8) -> Maybe Stone -> Board -> Board
insert (y,x) ms b = set b y x s
    where s = case ms of Just Black -> c_BLACK; Just White -> c_WHITE; Nothing -> c_EMPTY


unsafePrint :: Board -> IO ()
unsafePrint b = BS.unsafeUseAsCString (unBoard b) (c_board_print . castPtr)


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
c_EMPTY = 0x00
c_BLACK = 0x01
c_WHITE = 0x02
