{-# LINE 1 "Curses/Curses.hsc" #-}
{-# OPTIONS -fffi -fglasgow-exts #-}
{-# LINE 2 "Curses/Curses.hsc" #-}

module Curses.Curses (
    --------------------------------------------------------------------
    
    Window,      -- data Window deriving Eq
    stdScr,      -- :: Window
    initScr,     -- :: IO Window
    cBreak,      -- :: Bool -> IO ()
    raw,         -- :: Bool -> IO ()
    echo,        -- :: Bool -> IO ()
    nl,          -- :: Bool -> IO ()
    intrFlush,   -- :: Bool -> IO ()
    keypad,      -- :: Window -> Bool -> IO ()
    noDelay,     -- :: Window -> Bool -> IO ()
    initCurses,  -- :: IO ()
    useDefaultColors, -- :: IO ()
    endWin,      -- :: IO ()
    resizeTerminal,
    
    clearOk,
    leaveOk,
    
    --------------------------------------------------------------------
    
    scrSize,          -- :: IO (Int, Int)
    refresh,          -- :: IO ()
    
    --------------------------------------------------------------------
    
    hasColors,      -- :: IO Bool
    startColor,     -- :: IO ()
    Pair(..),       -- newtype Pair = Pair Int deriving (Eq, Ord, Ix)
    colorPairs,     -- :: IO Int
    Color(..),      -- newtype Color = Color Int deriving (Eq, Ord, Ix)
    colors,         -- :: IO Int
    color,          -- :: String -> Maybe Color
--    black, red, green, yellow, blue, magenta, cyan, white, -- :: Color
    initPair,       -- :: Pair -> Color -> Color -> IO ()
    pairContent,    -- :: Pair -> IO (Color, Color)
    canChangeColor, -- :: IO Bool
    initColor,      -- :: Color -> (Int, Int, Int) -> IO ()
    colorContent,   -- :: Color -> IO (Int, Int, Int)
    
    --------------------------------------------------------------------
    
    Attr,  -- data Attr deriving Eq
    attr0, -- :: Attr
    
    isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis,
    isLeft, isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical,
        -- :: Attr -> Bool
    
    setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical,
        -- :: Attr -> Bool -> Attr
    
    attrSet, -- :: Attr -> Pair -> IO ()
    attrOn, attrOff,
    
    --------------------------------------------------------------------

    wAddStr, 
    addLn,         -- :: IO ()
    mvWAddStr,
    wMove,
    getYX,
    
    --------------------------------------------------------------------
    
    bkgrndSet,      -- :: Attr -> Pair -> IO ()
    erase,          -- :: IO ()
    wclear,         -- :: Window -> IO ()
    clrToEol,       -- :: IO ()
    move,           -- :: Int -> Int -> IO ()

    -- Cursor Routines
    CursorVisibility(..), 
    withCursor,
    cursSet,

    standout,standend,
    attrDim, attrBold,
    attrDimOn, attrDimOff,
    attrBoldOn, attrBoldOff,
    wAttrOn,
    wAttrOff, 
    touchWin,
    --------------------------------------------------------------------
    -- Mouse Routines
    withMouseEventMask,
    ButtonEvent(..),
    MouseEvent(..),
    
    --------------------------------------------------------------------
    
    Key(..),
    getCh, 
    newPad, pRefresh, delWin, newWin,
    wClrToEol,
    withProgram,

    ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling,

    beep, wAttrSet, wAttrGet,

    cursesSigWinch,
    cursesTest,

    decodeKey,
    getch,
    resetParams
    ) 
    
    --------------------------------------------------------------------
    where

import Prelude hiding (pi)
import Monad 
import Char           (chr, ord, isPrint, isSpace, toLower)
import Ix             (Ix)

import Data.Bits
import Control.Concurrent
import Foreign
import CForeign ( CInt, CChar, CShort, withCString )

import System.IO.Unsafe
import Control.Exception hiding(block)

import Ginsu.GenUtil(foldl')

-- #if __GLASGOW_HASKELL__ >= 604
-- import Foreign.C.String
-- #else
import Ginsu.CWString
-- #endif

import System.Posix.Signals
import List 
import Monad
import Maybe



{-# LINE 151 "Curses/Curses.hsc" #-}


------------------------------------------------------------------------

fi = fromIntegral

throwIfErr :: Num a => String -> IO a -> IO a
--throwIfErr name act = do
--    res <- act
--    if res == (#const ERR)
--        then ioError (userError ("Curses: "++name++" failed"))
--        else return res
throwIfErr s = throwIf (== (-1)) (\a -> "Curses[" ++ show a ++ "]:"  ++ s)
{-# LINE 164 "Curses/Curses.hsc" #-}

throwIfErr_ :: Num a => String -> IO a -> IO ()
throwIfErr_ name act = void $ throwIfErr name act

------------------------------------------------------------------------

data WindowTag 
type Window = Ptr WindowTag

stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)
foreign import ccall "static my_curses.h &stdscr" stdscr :: Ptr Window


initScr :: IO Window
initScr = throwIfNull "initscr" initscr
foreign import ccall unsafe "my_curses.h initscr" initscr :: IO Window

cBreak :: Bool -> IO ()
cBreak True  = throwIfErr_ "cbreak"   cbreak
cBreak False = throwIfErr_ "nocbreak" nocbreak
foreign import ccall unsafe "my_curses.h cbreak" cbreak :: IO CInt
foreign import ccall unsafe "my_curses.h nocbreak" nocbreak :: IO CInt

raw :: Bool -> IO ()
raw False = throwIfErr_ "noraw" noraw
raw True  = throwIfErr_ "raw"   raw_c
foreign import ccall unsafe "my_curses.h noraw" noraw :: IO CInt
foreign import ccall unsafe "my_curses.h raw" raw_c :: IO CInt

echo :: Bool -> IO ()
echo False = throwIfErr_ "noecho" noecho
echo True  = throwIfErr_ "echo"   echo_c
foreign import ccall unsafe "my_curses.h noecho" noecho :: IO CInt
foreign import ccall unsafe "my_curses.h echo" echo_c :: IO CInt

nl :: Bool -> IO ()
nl True  = throwIfErr_ "nl"   nl_c
nl False = throwIfErr_ "nonl" nonl
foreign import ccall unsafe "my_curses.h nl" nl_c :: IO CInt
foreign import ccall unsafe "my_curses.h nonl" nonl :: IO CInt

intrFlush :: Bool -> IO ()
intrFlush bf =
    throwIfErr_ "intrflush" $ intrflush stdScr (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h intrflush" intrflush :: Window -> (Word8) -> IO CInt
{-# LINE 210 "Curses/Curses.hsc" #-}

keypad :: Window -> Bool -> IO ()
keypad win bf =
    throwIfErr_ "keypad" $ keypad_c win (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h keypad" keypad_c :: Window -> (Word8) -> IO CInt
{-# LINE 215 "Curses/Curses.hsc" #-}

noDelay :: Window -> Bool -> IO ()
noDelay win bf =
    throwIfErr_ "nodelay" $ nodelay win (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h nodelay" nodelay :: Window -> (Word8) -> IO CInt
{-# LINE 220 "Curses/Curses.hsc" #-}

foreign import ccall unsafe "my_curses.h leaveok" leaveok_c :: Window -> (Word8) -> IO CInt
{-# LINE 222 "Curses/Curses.hsc" #-}

leaveOk True = leaveok_c stdScr 1
leaveOk False = leaveok_c stdScr 0


foreign import ccall unsafe "my_curses.h clearok" clearok_c :: Window -> (Word8) -> IO CInt
{-# LINE 228 "Curses/Curses.hsc" #-}

clearOk True = clearok_c stdScr 1
clearOk False = clearok_c stdScr 0


{-# LINE 233 "Curses/Curses.hsc" #-}
--HAVE_USE_DEFAULT_COLORS
foreign import ccall unsafe "my_curses.h use_default_colors" useDefaultColors :: IO ()

defaultBackground = Color (-1)
defaultForeground = Color (-1)

foreign import ccall unsafe "my_curses.h define_key" define_key :: Ptr CChar -> CInt -> IO ()
defineKey k s =  withCString s (\s -> define_key s k) >> return ()


{-# LINE 253 "Curses/Curses.hsc" #-}

initCurses :: IO ()
initCurses = do
    initScr
    b <- hasColors
    when b startColor
    --when b useDefaultColors
    cBreak True
    echo False
    nl False
    leaveOk True
    intrFlush False
    try $ keypad stdScr True
    defineKey (259) "\x1b[1;2A"
{-# LINE 267 "Curses/Curses.hsc" #-}
    defineKey (258) "\x1b[1;2B"
{-# LINE 268 "Curses/Curses.hsc" #-}
    defineKey (393) "\x1b[1;2D"
{-# LINE 269 "Curses/Curses.hsc" #-}
    defineKey (402) "\x1b[1;2C"
{-# LINE 270 "Curses/Curses.hsc" #-}

resetParams :: IO ()
resetParams = do
    raw True    -- raw mode please
    echo False
    nl False
    intrFlush True
    leaveOk False
    keypad stdScr True
    defineKey (259) "\x1b[1;2A"
{-# LINE 280 "Curses/Curses.hsc" #-}
    defineKey (258) "\x1b[1;2B"
{-# LINE 281 "Curses/Curses.hsc" #-}
    defineKey (393) "\x1b[1;2D"
{-# LINE 282 "Curses/Curses.hsc" #-}
    defineKey (402) "\x1b[1;2C"
{-# LINE 283 "Curses/Curses.hsc" #-}

endWin :: IO ()
endWin = throwIfErr_ "endwin" endwin
foreign import ccall unsafe "my_curses.h endwin" endwin :: IO CInt

------------------------------------------------------------------------

scrSize :: IO (Int, Int)
scrSize = do
    lines <- peek linesPtr
    cols  <- peek colsPtr
    return (fromIntegral lines, fromIntegral cols)
{-# NOINLINE scrSize #-}

foreign import ccall "my_curses.h &LINES" linesPtr :: Ptr CInt
foreign import ccall "my_curses.h &COLS" colsPtr :: Ptr CInt


refresh :: IO ()
refresh = throwIfErr_ "refresh" refresh_c
foreign import ccall unsafe "my_curses.h refresh" refresh_c :: IO CInt

------------------------------------------------------------------------

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors
foreign import ccall unsafe "my_curses.h has_colors" has_colors :: IO (Word8)
{-# LINE 310 "Curses/Curses.hsc" #-}

startColor :: IO ()
startColor = throwIfErr_ "start_color" start_color
foreign import ccall unsafe start_color :: IO CInt

newtype Pair = Pair Int deriving (Eq, Ord, Ix)

colorPairs :: IO Int
colorPairs = fmap fromIntegral $ peek colorPairsPtr
{-# NOINLINE colorPairs #-}
{- With inlining some optimizations will cause stuff to fail to compile
   Due to COLOR_PAIRS not being defined in the .hc file. -}


foreign import ccall "my_curses.h &COLOR_PAIRS" colorPairsPtr :: Ptr CInt

newtype Color = Color Int deriving (Eq, Ord, Ix)

colors :: IO Int
colors = liftM fromIntegral $ peek colorsPtr
{-# NOINLINE colors #-}

foreign import ccall "my_curses.h &COLORS" colorsPtr :: Ptr CInt

--black, red, green, yellow, blue, magenta, cyan, white :: Color

color :: String -> Maybe Color
color "default"  = Just $ Color (-1)
color "black"    = Just $ Color (0)
{-# LINE 339 "Curses/Curses.hsc" #-}
color "red"      = Just $ Color (1)
{-# LINE 340 "Curses/Curses.hsc" #-}
color "green"    = Just $ Color (2)
{-# LINE 341 "Curses/Curses.hsc" #-}
color "yellow"   = Just $ Color (3)
{-# LINE 342 "Curses/Curses.hsc" #-}
color "blue"     = Just $ Color (4)
{-# LINE 343 "Curses/Curses.hsc" #-}
color "magenta"  = Just $ Color (5)
{-# LINE 344 "Curses/Curses.hsc" #-}
color "cyan"     = Just $ Color (6)
{-# LINE 345 "Curses/Curses.hsc" #-}
color "white"    = Just $ Color (7)
{-# LINE 346 "Curses/Curses.hsc" #-}
color _ =  Nothing

data Attribute = Attribute [String] String String
parseAttr :: String -> Attribute 
parseAttr s = Attribute as fg bg where
    rs = filter (not . f . head) $ groupBy (\x y -> f x && f y) (map toLower s) 
    as = filter (`elem` attributes) rs
    col x = if isJust (color x) then return x else Nothing
    fg = fromJust $ msum (map (cGet "fg") rs)  `mplus` msum (map col rs) `mplus` return "default"
    bg = fromJust $ msum (map (cGet "bg") rs) `mplus` return "default"
    f ',' = True
    f c | isSpace c = True
    f _ = False
    cGet p r | (p ++ ":") `isPrefixOf` r = col (drop (length p + 1) r) 
    cGet _ _ = Nothing
    attributes = ["normal", "bold", "blink", "dim", "reverse", "underline" ]


initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ "init_pair" $
        init_pair (fromIntegral p) (fromIntegral f) (fromIntegral b)
foreign import ccall unsafe init_pair :: CShort -> CShort -> CShort -> IO CInt

pairContent :: Pair -> IO (Color, Color)
pairContent (Pair p) =
    alloca $ \fPtr ->
    alloca $ \bPtr -> do
        throwIfErr "pair_content" $ pair_content (fromIntegral p) fPtr bPtr
        f <- peek fPtr
        b <- peek bPtr
        return (Color (fromIntegral f), Color (fromIntegral b))
foreign import ccall unsafe pair_content :: CShort -> Ptr CShort -> Ptr CShort -> IO CInt

canChangeColor :: IO Bool
canChangeColor = liftM (/= 0) can_change_color
foreign import ccall unsafe can_change_color :: IO (Word8)
{-# LINE 383 "Curses/Curses.hsc" #-}

initColor :: Color -> (Int, Int, Int) -> IO ()
initColor (Color c) (r, g, b) =
    throwIfErr_ "init_color" $
        init_color (fromIntegral c) (fromIntegral r) (fromIntegral g) (fromIntegral b)
foreign import ccall unsafe init_color :: CShort -> CShort -> CShort -> CShort -> IO CInt

colorContent :: Color -> IO (Int, Int, Int)
colorContent (Color c) =
    alloca $ \rPtr ->
    alloca $ \gPtr ->
    alloca $ \bPtr -> do
        throwIfErr "color_content" $ color_content (fromIntegral c) rPtr gPtr bPtr
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        return (fromIntegral r, fromIntegral g, fromIntegral b)
foreign import ccall unsafe color_content :: CShort -> Ptr CShort -> Ptr CShort -> Ptr CShort -> IO CInt

foreign import ccall unsafe "my_curses.h hs_curses_color_pair" colorPair :: Pair -> (Word64)
{-# LINE 403 "Curses/Curses.hsc" #-}

{-# LINE 404 "Curses/Curses.hsc" #-}

-------------
-- Attributes 
-------------

foreign import ccall unsafe "my_curses.h attr_set" attr_set :: Attr -> CShort -> Ptr a -> IO Int
-- foreign import ccall unsafe "my_curses.h attr_get" :: Attr -> CShort -> Ptr a -> IO Int

foreign import ccall unsafe "my_curses.h wattr_set" wattr_set :: Window -> Attr -> CInt -> Ptr a -> IO CInt
foreign import ccall unsafe "my_curses.h wattr_get" wattr_get :: Window -> Ptr Attr -> Ptr CShort -> Ptr a -> IO CInt

foreign import ccall "my_curses.h attr_on" attr_on :: (Word64) -> Ptr a -> IO Int
{-# LINE 416 "Curses/Curses.hsc" #-}
foreign import ccall "my_curses.h attr_off" attr_off :: (Word64) -> Ptr a -> IO Int
{-# LINE 417 "Curses/Curses.hsc" #-}
foreign import ccall "my_curses.h attron" attron :: Int -> IO Int
foreign import ccall "my_curses.h attroff" attroff :: Int -> IO Int
foreign import ccall unsafe "my_curses.h wattron" wattron :: Window -> CInt -> IO CInt
foreign import ccall unsafe "my_curses.h wattroff" wattroff :: Window -> CInt -> IO CInt
foreign import ccall standout :: IO Int
foreign import ccall standend :: IO Int

wAttrSet :: Window -> (Attr,Pair) -> IO ()
wAttrSet w (a,(Pair p)) = throwIfErr_ "wattr_set" $ wattr_set w a (fromIntegral p) nullPtr
wAttrGet :: Window -> IO (Attr,Pair)
wAttrGet w =  alloca $ \pa -> alloca $ \pp -> (throwIfErr_ "wattr_get" $ wattr_get w pa pp nullPtr) >> (peek pa >>=  \a -> peek pp >>=  \p -> return (a,Pair (fromIntegral p)))


newtype Attr = Attr (Word64) deriving (Eq,Storable,Bits, Num, Show)
{-# LINE 431 "Curses/Curses.hsc" #-}

attr0 :: Attr
attr0 = Attr (0)
{-# LINE 434 "Curses/Curses.hsc" #-}

isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis, isLeft,
    isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical
    :: Attr -> Bool
isAltCharset = isAttr (4194304)
{-# LINE 440 "Curses/Curses.hsc" #-}
isBlink      = isAttr (524288)
{-# LINE 441 "Curses/Curses.hsc" #-}
isBold       = isAttr (2097152)
{-# LINE 442 "Curses/Curses.hsc" #-}
isDim        = isAttr (1048576)
{-# LINE 443 "Curses/Curses.hsc" #-}
isHorizontal = isAttr (33554432)
{-# LINE 444 "Curses/Curses.hsc" #-}
isInvis      = isAttr (8388608)
{-# LINE 445 "Curses/Curses.hsc" #-}
isLeft       = isAttr (67108864)
{-# LINE 446 "Curses/Curses.hsc" #-}
isLow        = isAttr (134217728)
{-# LINE 447 "Curses/Curses.hsc" #-}
isProtect    = isAttr (16777216)
{-# LINE 448 "Curses/Curses.hsc" #-}
isReverse    = isAttr (262144)
{-# LINE 449 "Curses/Curses.hsc" #-}
isRight      = isAttr (268435456)
{-# LINE 450 "Curses/Curses.hsc" #-}
isStandout   = isAttr (65536)
{-# LINE 451 "Curses/Curses.hsc" #-}
isTop        = isAttr (536870912)
{-# LINE 452 "Curses/Curses.hsc" #-}
isUnderline  = isAttr (131072)
{-# LINE 453 "Curses/Curses.hsc" #-}
isVertical   = isAttr (1073741824)
{-# LINE 454 "Curses/Curses.hsc" #-}

isAttr :: (Word64) -> Attr -> Bool
{-# LINE 456 "Curses/Curses.hsc" #-}
isAttr bit (Attr a) = a .&. bit /= 0

setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical
    :: Attr -> Bool -> Attr
setAltCharset = setAttr (4194304)
{-# LINE 463 "Curses/Curses.hsc" #-}
setBlink      = setAttr (524288)
{-# LINE 464 "Curses/Curses.hsc" #-}
setBold       = setAttr (2097152)
{-# LINE 465 "Curses/Curses.hsc" #-}
setDim        = setAttr (1048576)
{-# LINE 466 "Curses/Curses.hsc" #-}
setHorizontal = setAttr (33554432)
{-# LINE 467 "Curses/Curses.hsc" #-}
setInvis      = setAttr (8388608)
{-# LINE 468 "Curses/Curses.hsc" #-}
setLeft       = setAttr (67108864)
{-# LINE 469 "Curses/Curses.hsc" #-}
setLow        = setAttr (134217728)
{-# LINE 470 "Curses/Curses.hsc" #-}
setProtect    = setAttr (16777216)
{-# LINE 471 "Curses/Curses.hsc" #-}
setReverse    = setAttr (262144)
{-# LINE 472 "Curses/Curses.hsc" #-}
setRight      = setAttr (268435456)
{-# LINE 473 "Curses/Curses.hsc" #-}
setStandout   = setAttr (65536)
{-# LINE 474 "Curses/Curses.hsc" #-}
setTop        = setAttr (536870912)
{-# LINE 475 "Curses/Curses.hsc" #-}
setUnderline  = setAttr (131072)
{-# LINE 476 "Curses/Curses.hsc" #-}
setVertical   = setAttr (1073741824)
{-# LINE 477 "Curses/Curses.hsc" #-}

setAttr :: (Word64) -> Attr -> Bool -> Attr
{-# LINE 479 "Curses/Curses.hsc" #-}
setAttr bit (Attr a) False = Attr (a .&. complement bit)
setAttr bit (Attr a) True  = Attr (a .|.            bit)

attrSet :: Attr -> Pair -> IO ()
attrSet attr (Pair p) = throwIfErr_ "attrset" $
    attr_set attr (fromIntegral p) nullPtr

attrOn :: Attr -> IO ()
attrOn (Attr attr) = throwIfErr_ "attr_on" $
    attr_on attr nullPtr


attrOff :: Attr -> IO ()
attrOff (Attr attr) = throwIfErr_ "attr_off" $
    attr_off attr nullPtr



wAttrOn :: Window -> Int -> IO ()
wAttrOn w x = throwIfErr_ "wattron" $ wattron w (fi x)

wAttrOff :: Window -> Int -> IO ()
wAttrOff w x = throwIfErr_ "wattroff" $ wattroff w (fi x)

attrDimOn :: IO ()
attrDimOn  = throwIfErr_ "attron A_DIM" $
    attron (1048576) 
{-# LINE 506 "Curses/Curses.hsc" #-}

attrDimOff :: IO ()
attrDimOff = throwIfErr_ "attroff A_DIM" $
    attroff (1048576) 
{-# LINE 510 "Curses/Curses.hsc" #-}

attrBoldOn :: IO ()
attrBoldOn  = throwIfErr_ "attron A_BOLD" $
    attron (2097152) 
{-# LINE 514 "Curses/Curses.hsc" #-}

attrBoldOff :: IO ()
attrBoldOff = throwIfErr_ "attroff A_BOLD" $
    attroff (2097152) 
{-# LINE 518 "Curses/Curses.hsc" #-}


attrDim :: Int
attrDim = (1048576)
{-# LINE 522 "Curses/Curses.hsc" #-}
attrBold :: Int
attrBold = (2097152)
{-# LINE 524 "Curses/Curses.hsc" #-}

------------------------------------------------------------------------


mvWAddStr :: Window -> Int -> Int -> String -> IO ()
mvWAddStr w y x str = wMove w y x >> wAddStr w str 

addLn :: IO ()
addLn = wAddStr stdScr "\n" 

sanifyOutput :: String -> String
sanifyOutput = map f . filter (/= '\r') where
    f c | isPrint c  = c
    f c = '~'


{-# LINE 540 "Curses/Curses.hsc" #-}

--wAddStr :: Window -> String -> IO ()
--wAddStr w str = throwIfErr_ ("waddnwstr: " ++ show str) $ withCWStringLen (sanifyOutput str) (\(ws,len) -> waddnwstr w ws (fi len))
    
foreign import ccall unsafe waddnwstr :: Window -> CWString -> CInt -> IO CInt
foreign import ccall unsafe waddch :: Window -> (Word64) -> IO CInt
{-# LINE 546 "Curses/Curses.hsc" #-}

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withCWStringLen  (sanifyOutput s) (\(ws,len) ->  (waddnwstr win ws (fi len)))
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "waddch" $ waddch win ch'
                loop str' id)
    loop str id 


{-# LINE 587 "Curses/Curses.hsc" #-}
{-

wAddStr :: Window -> String -> IO ()
wAddStr w str =  withLCStringLen (sanifyOutput str) (\(ws,len) -> throwIfErr_ ("waddnstr: " ++ show len ++ " " ++ show str) $ waddnstr w ws (fi len))
foreign import ccall unsafe waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withLCString  (sanifyOutput s) (\(ws,len) ->  (waddnstr win ws (fi len)))
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "waddch" $ waddch win ch'
                loop str' id)
    loop str id 
-}
------------------------------------------------------------------------


{-# LINE 615 "Curses/Curses.hsc" #-}

bkgrndSet :: Attr -> Pair -> IO ()
bkgrndSet (Attr a) p = bkgdset $
    fromIntegral (ord ' ') .|.
    (if a .&. 4194304 /= 0 then 4194304 else 0) .|.
{-# LINE 620 "Curses/Curses.hsc" #-}
    (if a .&. 524288 /= 0 then 524288 else 0) .|.
{-# LINE 621 "Curses/Curses.hsc" #-}
    (if a .&. 2097152 /= 0 then 2097152 else 0) .|.
{-# LINE 622 "Curses/Curses.hsc" #-}
    (if a .&. 1048576 /= 0 then 1048576 else 0) .|.
{-# LINE 623 "Curses/Curses.hsc" #-}
    (if a .&. 8388608 /= 0 then 8388608 else 0) .|.
{-# LINE 624 "Curses/Curses.hsc" #-}
    (if a .&. 16777216 /= 0 then 16777216 else 0) .|.
{-# LINE 625 "Curses/Curses.hsc" #-}
    (if a .&. 262144 /= 0 then 262144 else 0) .|.
{-# LINE 626 "Curses/Curses.hsc" #-}
    (if a .&. 65536 /= 0 then 65536 else 0) .|.
{-# LINE 627 "Curses/Curses.hsc" #-}
    (if a .&. 131072 /= 0 then 131072 else 0) .|.
{-# LINE 628 "Curses/Curses.hsc" #-}
    colorPair p
foreign import ccall unsafe bkgdset :: (Word64) -> IO ()
{-# LINE 630 "Curses/Curses.hsc" #-}

erase :: IO ()
erase = throwIfErr_ "erase" $ werase_c  stdScr
foreign import ccall unsafe "werase" werase_c :: Window -> IO CInt

wclear :: Window -> IO ()
wclear w = throwIfErr_ "wclear" $ wclear_c  w
foreign import ccall unsafe "wclear" wclear_c :: Window -> IO CInt



clrToEol :: IO ()
clrToEol = throwIfErr_ "clrtoeol" clrtoeol
foreign import ccall unsafe clrtoeol :: IO CInt

move :: Int -> Int -> IO ()
move y x =
    throwIfErr_ "move" $ move_c (fromIntegral y) (fromIntegral x)
foreign import ccall unsafe "move" move_c :: CInt -> CInt -> IO CInt

wMove :: Window -> Int -> Int -> IO ()
wMove w y x = throwIfErr_ "wmove" $ wmove w (fi y) (fi x)
foreign import ccall unsafe  wmove :: Window -> CInt -> CInt -> IO CInt

------------------
-- Cursor routines
------------------

data CursorVisibility = CursorInvisible | CursorVisible | CursorVeryVisible

vis_c :: CursorVisibility -> CInt
vis_c vis = case vis of
    CursorInvisible   -> 0
    CursorVisible     -> 1
    CursorVeryVisible -> 2
    
c_vis :: CInt -> CursorVisibility
c_vis 0 = CursorInvisible
c_vis 1 = CursorVisible  
c_vis 2 = CursorVeryVisible
c_vis n = error ("Illegal C value for cursor visibility: " ++ show n)

foreign import ccall unsafe "my_curses.h curs_set" curs_set :: CInt -> IO CInt

cursSet' 0 = leaveOk True >> curs_set 0
cursSet' n = leaveOk False >> curs_set n 

cursSet :: CursorVisibility -> IO CursorVisibility
cursSet CursorInvisible = 
    do leaveOk True
       old <- curs_set 0
       return $ c_vis old
cursSet v = 
    do leaveOk False
       old <- curs_set (vis_c v)
       return $ c_vis old


withCursor :: CursorVisibility -> IO a -> IO a
withCursor nv action = Control.Exception.bracket (cursSet' (vis_c nv)) (\v -> case v of 
		(-1) -> return 0
{-# LINE 691 "Curses/Curses.hsc" #-}
		x -> cursSet' x) (\_ -> action)


foreign import ccall unsafe "nomacro.h nomacro_getyx" nomacro_getyx :: Window -> Ptr CInt -> Ptr CInt -> IO ()
getYX :: Window -> IO (Int, Int)
getYX w =  alloca $ \py -> alloca $ \px -> (nomacro_getyx w py px)
           >> (peek py >>=  \y -> peek px >>=  \x -> return (fromIntegral y, fromIntegral x))

------------------------------------------------------------------------


touchWin :: Window -> IO ()
touchWin w = throwIfErr_ "touchwin" $ touchwin w
foreign import ccall touchwin :: Window -> IO CInt

newPad :: Int -> Int -> IO Window
newPad nlines ncols = throwIfNull "newpad" $ newpad (fromIntegral nlines) (fromIntegral ncols)

pRefresh :: Window -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
pRefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol = throwIfErr_ "prefresh" $
    prefresh pad (fromIntegral pminrow) (fromIntegral pmincol) (fromIntegral sminrow) (fromIntegral smincol) (fromIntegral smaxrow) (fromIntegral smaxcol)

delWin :: Window -> IO ()
delWin w = throwIfErr_ "delwin" $ delwin w
    
foreign import ccall unsafe prefresh :: Window -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe newpad :: CInt -> CInt -> IO Window
foreign import ccall unsafe delwin :: Window -> IO CInt


newWin :: Int -> Int -> Int -> Int -> IO Window
newWin nlines ncolumn begin_y begin_x = throwIfNull "newwin" $ newwin (fi nlines) (fi ncolumn) (fi begin_y) (fi begin_x)

foreign import ccall unsafe newwin :: CInt -> CInt -> CInt -> CInt -> IO Window


wClrToEol :: Window -> IO ()
wClrToEol w = throwIfErr_ "wclrtoeol" $ wclrtoeol w
foreign import ccall unsafe wclrtoeol :: Window -> IO CInt




foreign import ccall threadsafe getch :: IO CInt


--foreign import ccall unsafe def_prog_mode :: IO CInt
--foreign import ccall unsafe reset_prog_mode :: IO CInt
foreign import ccall unsafe flushinp :: IO CInt


withProgram :: IO a -> IO a
withProgram action = withCursor CursorVisible $ Control.Exception.bracket_ (endWin) (flushinp) action
--withProgram action = withCursor CursorVisible $ Control.Exception.bracket_ ({-def_prog_mode >> -}endWin) (return ()){-reset_prog_mode-} action


foreign import ccall unsafe "my_curses.h beep" c_beep :: IO CInt
foreign import ccall unsafe "my_curses.h flash" c_flash :: IO CInt

beep :: IO ()
beep = do
    br <- c_beep
    when (br /= (0)) (c_flash >> return ()) 
{-# LINE 754 "Curses/Curses.hsc" #-}


---------------
-- Key Routines
---------------

data Key
    = KeyChar Char | KeyBreak | KeyDown | KeyUp | KeyLeft | KeyRight
    | KeyHome | KeyBackspace | KeyF Int | KeyDL | KeyIL | KeyDC
    | KeyIC | KeyEIC | KeyClear | KeyEOS | KeyEOL | KeySF | KeySR
    | KeyNPage | KeyPPage | KeySTab | KeyCTab | KeyCATab | KeyEnter
    | KeySReset | KeyReset | KeyPrint | KeyLL | KeyA1 | KeyA3
    | KeyB2 | KeyC1 | KeyC3 | KeyBTab | KeyBeg | KeyCancel | KeyClose
    | KeyCommand | KeyCopy | KeyCreate | KeyEnd | KeyExit | KeyFind
    | KeyHelp | KeyMark | KeyMessage | KeyMove | KeyNext | KeyOpen
    | KeyOptions | KeyPrevious | KeyRedo | KeyReference | KeyRefresh
    | KeyReplace | KeyRestart | KeyResume | KeySave | KeySBeg
    | KeySCancel | KeySCommand | KeySCopy | KeySCreate | KeySDC
    | KeySDL | KeySelect | KeySEnd | KeySEOL | KeySExit | KeySFind
    | KeySHelp | KeySHome | KeySIC | KeySLeft | KeySMessage | KeySMove
    | KeySNext | KeySOptions | KeySPrevious | KeySPrint | KeySRedo
    | KeySReplace | KeySRight | KeySRsume | KeySSave | KeySSuspend
    | KeySUndo | KeySuspend | KeyUndo | KeyResize | KeyMouse | KeyUnknown Int
    deriving (Eq,Show,Ord)

decodeKey :: CInt -> Key
decodeKey key = case key of
    _ | key >= 0 && key <= 255 -> KeyChar (chr (fromIntegral key))
    (257)         -> KeyBreak
{-# LINE 783 "Curses/Curses.hsc" #-}
    (258)          -> KeyDown
{-# LINE 784 "Curses/Curses.hsc" #-}
    (259)            -> KeyUp
{-# LINE 785 "Curses/Curses.hsc" #-}
    (260)          -> KeyLeft
{-# LINE 786 "Curses/Curses.hsc" #-}
    (261)         -> KeyRight
{-# LINE 787 "Curses/Curses.hsc" #-}
    (262)          -> KeyHome
{-# LINE 788 "Curses/Curses.hsc" #-}
    (263)     -> KeyBackspace
{-# LINE 789 "Curses/Curses.hsc" #-}
    _ | key >= (264) && key <= (327)
{-# LINE 790 "Curses/Curses.hsc" #-}
                               -> KeyF (fromIntegral (key - 264))
{-# LINE 791 "Curses/Curses.hsc" #-}
    (328)            -> KeyDL
{-# LINE 792 "Curses/Curses.hsc" #-}
    (329)            -> KeyIL
{-# LINE 793 "Curses/Curses.hsc" #-}
    (330)            -> KeyDC
{-# LINE 794 "Curses/Curses.hsc" #-}
    (331)            -> KeyIC
{-# LINE 795 "Curses/Curses.hsc" #-}
    (332)           -> KeyEIC
{-# LINE 796 "Curses/Curses.hsc" #-}
    (333)         -> KeyClear
{-# LINE 797 "Curses/Curses.hsc" #-}
    (334)           -> KeyEOS
{-# LINE 798 "Curses/Curses.hsc" #-}
    (335)           -> KeyEOL
{-# LINE 799 "Curses/Curses.hsc" #-}
    (336)            -> KeySF
{-# LINE 800 "Curses/Curses.hsc" #-}
    (337)            -> KeySR
{-# LINE 801 "Curses/Curses.hsc" #-}
    (338)         -> KeyNPage
{-# LINE 802 "Curses/Curses.hsc" #-}
    (339)         -> KeyPPage
{-# LINE 803 "Curses/Curses.hsc" #-}
    (340)          -> KeySTab
{-# LINE 804 "Curses/Curses.hsc" #-}
    (341)          -> KeyCTab
{-# LINE 805 "Curses/Curses.hsc" #-}
    (342)         -> KeyCATab
{-# LINE 806 "Curses/Curses.hsc" #-}
    (343)         -> KeyEnter
{-# LINE 807 "Curses/Curses.hsc" #-}
    (344)        -> KeySReset
{-# LINE 808 "Curses/Curses.hsc" #-}
    (345)         -> KeyReset
{-# LINE 809 "Curses/Curses.hsc" #-}
    (346)         -> KeyPrint
{-# LINE 810 "Curses/Curses.hsc" #-}
    (347)            -> KeyLL
{-# LINE 811 "Curses/Curses.hsc" #-}
    (348)            -> KeyA1
{-# LINE 812 "Curses/Curses.hsc" #-}
    (349)            -> KeyA3
{-# LINE 813 "Curses/Curses.hsc" #-}
    (350)            -> KeyB2
{-# LINE 814 "Curses/Curses.hsc" #-}
    (351)            -> KeyC1
{-# LINE 815 "Curses/Curses.hsc" #-}
    (352)            -> KeyC3
{-# LINE 816 "Curses/Curses.hsc" #-}
    (353)          -> KeyBTab
{-# LINE 817 "Curses/Curses.hsc" #-}
    (354)           -> KeyBeg
{-# LINE 818 "Curses/Curses.hsc" #-}
    (355)        -> KeyCancel
{-# LINE 819 "Curses/Curses.hsc" #-}
    (356)         -> KeyClose
{-# LINE 820 "Curses/Curses.hsc" #-}
    (357)       -> KeyCommand
{-# LINE 821 "Curses/Curses.hsc" #-}
    (358)          -> KeyCopy
{-# LINE 822 "Curses/Curses.hsc" #-}
    (359)        -> KeyCreate
{-# LINE 823 "Curses/Curses.hsc" #-}
    (360)           -> KeyEnd
{-# LINE 824 "Curses/Curses.hsc" #-}
    (361)          -> KeyExit
{-# LINE 825 "Curses/Curses.hsc" #-}
    (362)          -> KeyFind
{-# LINE 826 "Curses/Curses.hsc" #-}
    (363)          -> KeyHelp
{-# LINE 827 "Curses/Curses.hsc" #-}
    (364)          -> KeyMark
{-# LINE 828 "Curses/Curses.hsc" #-}
    (365)       -> KeyMessage
{-# LINE 829 "Curses/Curses.hsc" #-}
    (366)          -> KeyMove
{-# LINE 830 "Curses/Curses.hsc" #-}
    (367)          -> KeyNext
{-# LINE 831 "Curses/Curses.hsc" #-}
    (368)          -> KeyOpen
{-# LINE 832 "Curses/Curses.hsc" #-}
    (369)       -> KeyOptions
{-# LINE 833 "Curses/Curses.hsc" #-}
    (370)      -> KeyPrevious
{-# LINE 834 "Curses/Curses.hsc" #-}
    (371)          -> KeyRedo
{-# LINE 835 "Curses/Curses.hsc" #-}
    (372)     -> KeyReference
{-# LINE 836 "Curses/Curses.hsc" #-}
    (373)       -> KeyRefresh
{-# LINE 837 "Curses/Curses.hsc" #-}
    (374)       -> KeyReplace
{-# LINE 838 "Curses/Curses.hsc" #-}
    (375)       -> KeyRestart
{-# LINE 839 "Curses/Curses.hsc" #-}
    (376)        -> KeyResume
{-# LINE 840 "Curses/Curses.hsc" #-}
    (377)          -> KeySave
{-# LINE 841 "Curses/Curses.hsc" #-}
    (378)          -> KeySBeg
{-# LINE 842 "Curses/Curses.hsc" #-}
    (379)       -> KeySCancel
{-# LINE 843 "Curses/Curses.hsc" #-}
    (380)      -> KeySCommand
{-# LINE 844 "Curses/Curses.hsc" #-}
    (381)         -> KeySCopy
{-# LINE 845 "Curses/Curses.hsc" #-}
    (382)       -> KeySCreate
{-# LINE 846 "Curses/Curses.hsc" #-}
    (383)           -> KeySDC
{-# LINE 847 "Curses/Curses.hsc" #-}
    (384)           -> KeySDL
{-# LINE 848 "Curses/Curses.hsc" #-}
    (385)        -> KeySelect
{-# LINE 849 "Curses/Curses.hsc" #-}
    (386)          -> KeySEnd
{-# LINE 850 "Curses/Curses.hsc" #-}
    (387)          -> KeySEOL
{-# LINE 851 "Curses/Curses.hsc" #-}
    (388)         -> KeySExit
{-# LINE 852 "Curses/Curses.hsc" #-}
    (389)         -> KeySFind
{-# LINE 853 "Curses/Curses.hsc" #-}
    (390)         -> KeySHelp
{-# LINE 854 "Curses/Curses.hsc" #-}
    (391)         -> KeySHome
{-# LINE 855 "Curses/Curses.hsc" #-}
    (392)           -> KeySIC
{-# LINE 856 "Curses/Curses.hsc" #-}
    (393)         -> KeySLeft
{-# LINE 857 "Curses/Curses.hsc" #-}
    (394)      -> KeySMessage
{-# LINE 858 "Curses/Curses.hsc" #-}
    (395)         -> KeySMove
{-# LINE 859 "Curses/Curses.hsc" #-}
    (396)         -> KeySNext
{-# LINE 860 "Curses/Curses.hsc" #-}
    (397)      -> KeySOptions
{-# LINE 861 "Curses/Curses.hsc" #-}
    (398)     -> KeySPrevious
{-# LINE 862 "Curses/Curses.hsc" #-}
    (399)        -> KeySPrint
{-# LINE 863 "Curses/Curses.hsc" #-}
    (400)         -> KeySRedo
{-# LINE 864 "Curses/Curses.hsc" #-}
    (401)      -> KeySReplace
{-# LINE 865 "Curses/Curses.hsc" #-}
    (402)        -> KeySRight
{-# LINE 866 "Curses/Curses.hsc" #-}
    (403)        -> KeySRsume
{-# LINE 867 "Curses/Curses.hsc" #-}
    (404)         -> KeySSave
{-# LINE 868 "Curses/Curses.hsc" #-}
    (405)      -> KeySSuspend
{-# LINE 869 "Curses/Curses.hsc" #-}
    (406)         -> KeySUndo
{-# LINE 870 "Curses/Curses.hsc" #-}
    (407)       -> KeySuspend
{-# LINE 871 "Curses/Curses.hsc" #-}
    (408)          -> KeyUndo
{-# LINE 872 "Curses/Curses.hsc" #-}

{-# LINE 873 "Curses/Curses.hsc" #-}
    (410)        -> KeyResize
{-# LINE 874 "Curses/Curses.hsc" #-}

{-# LINE 875 "Curses/Curses.hsc" #-}

{-# LINE 876 "Curses/Curses.hsc" #-}
    (409)        -> KeyMouse
{-# LINE 877 "Curses/Curses.hsc" #-}

{-# LINE 878 "Curses/Curses.hsc" #-}
    _                          -> KeyUnknown (fromIntegral key)





--getCh :: IO Key
--getCh = threadWaitRead 0 >> (liftM decodeKey $ throwIfErr "getch" getch)

--getCh :: IO Key
--getCh = liftM decodeKey $ throwIfErr "getch" getch

-- getCh :: IO Key
-- getCh = do
--     nodelay stdScr 1
--     --halfdelay 1
--     v <- getch
--     case v of
-- 	(#const ERR) -> yield >> getCh 
-- 	x -> return $ decodeKey x

getCh :: IO (Maybe Key)
getCh = do
    v <- getch
    return $ case v of
                 (-1) -> Nothing
{-# LINE 904 "Curses/Curses.hsc" #-}
                 k_ -> Just $ decodeKey k_

resizeTerminal :: Int -> Int -> IO ()


{-# LINE 909 "Curses/Curses.hsc" #-}

resizeTerminal a b = throwIfErr_ "resizeterm"  $ resizeterm (fi a) (fi b)

foreign import ccall unsafe "my_curses.h resizeterm" resizeterm :: CInt -> CInt -> IO CInt


{-# LINE 919 "Curses/Curses.hsc" #-}


cursesSigWinch :: Maybe Signal


{-# LINE 924 "Curses/Curses.hsc" #-}

cursesSigWinch = Just (28)
{-# LINE 926 "Curses/Curses.hsc" #-}


{-# LINE 932 "Curses/Curses.hsc" #-}



------------
-- Test case
------------

cursesTest :: IO ()
cursesTest = do
    initScr
    hc <- hasColors 
    when hc startColor
    ccc <- canChangeColor
    (ys,xs) <- scrSize
    cp <- colorPairs
    cs <- colors
    endWin
    putStrLn $ "ScreenSize: " ++ show (xs,ys) 
    putStrLn $ "hasColors: " ++ show hc
    putStrLn $ "canChangeColor: " ++ show ccc
    putStrLn $ "colorPairs: " ++ show cp
    putStrLn $ "colors: " ++ show cs

    


-----------------
-- Mouse Routines
-----------------

data MouseEvent = MouseEvent {
    mouseEventId :: Int, 
    mouseEventX :: Int, 
    mouseEventY :: Int, 
    mouseEventZ :: Int, 
    mouseEventButton :: [ButtonEvent]
   } deriving(Show)

data ButtonEvent = ButtonPressed Int | ButtonReleased Int | ButtonClicked Int | 
    ButtonDoubleClicked Int | ButtonTripleClicked Int | ButtonShift | ButtonControl | ButtonAlt 
	deriving(Eq,Show)

withMouseEventMask :: [ButtonEvent] -> IO a -> IO a


{-# LINE 977 "Curses/Curses.hsc" #-}

foreign import ccall unsafe "my_curses.h mousemask" mousemask :: (Word64) -> Ptr (Word64) -> IO (Word64)
{-# LINE 979 "Curses/Curses.hsc" #-}

withMouseEventMask bes action = do
    ov <- alloca (\a ->  mousemask (besToMouseMask bes) a >> peek a) 
    r <- action 
    mousemask ov nullPtr 
    return r

besToMouseMask :: [ButtonEvent] -> (Word64)
{-# LINE 987 "Curses/Curses.hsc" #-}
besToMouseMask bes = foldl' (.|.) 0 (map cb bes) where
    cb (ButtonPressed 1) = (2)
{-# LINE 989 "Curses/Curses.hsc" #-}
    cb (ButtonPressed 2) = (128)
{-# LINE 990 "Curses/Curses.hsc" #-}
    cb (ButtonPressed 3) = (8192)
{-# LINE 991 "Curses/Curses.hsc" #-}
    cb (ButtonPressed 4) = (524288)
{-# LINE 992 "Curses/Curses.hsc" #-}
    cb (ButtonReleased 1) = (1)
{-# LINE 993 "Curses/Curses.hsc" #-}
    cb (ButtonReleased 2) = (64)
{-# LINE 994 "Curses/Curses.hsc" #-}
    cb (ButtonReleased 3) = (4096)
{-# LINE 995 "Curses/Curses.hsc" #-}
    cb (ButtonReleased 4) = (262144)
{-# LINE 996 "Curses/Curses.hsc" #-}
    cb (ButtonClicked 1) = (4)
{-# LINE 997 "Curses/Curses.hsc" #-}
    cb (ButtonClicked 2) = (256)
{-# LINE 998 "Curses/Curses.hsc" #-}
    cb (ButtonClicked 3) = (16384)
{-# LINE 999 "Curses/Curses.hsc" #-}
    cb (ButtonClicked 4) = (1048576)
{-# LINE 1000 "Curses/Curses.hsc" #-}
    cb ButtonShift = (33554432)
{-# LINE 1001 "Curses/Curses.hsc" #-}
    cb ButtonAlt = (67108864)
{-# LINE 1002 "Curses/Curses.hsc" #-}
    cb ButtonControl = (16777216)
{-# LINE 1003 "Curses/Curses.hsc" #-}
    cb _ = 0



{-# LINE 1010 "Curses/Curses.hsc" #-}




ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling
    :: Char

ulCorner = chr 0x250C
llCorner = chr 0x2514
urCorner = chr 0x2510
lrCorner = chr 0x2518
rTee     = chr 0x2524
lTee     = chr 0x251C
bTee     = chr 0x2534
tTee     = chr 0x252C
hLine    = chr 0x2500
vLine    = chr 0x2502
plus     = chr 0x253C
s1       = chr 0x23BA -- was: 0xF800
s9       = chr 0x23BD -- was: 0xF804
diamond  = chr 0x25C6
ckBoard  = chr 0x2592
degree   = chr 0x00B0
plMinus  = chr 0x00B1
bullet   = chr 0x00B7
lArrow   = chr 0x2190
rArrow   = chr 0x2192
dArrow   = chr 0x2193
uArrow   = chr 0x2191
board    = chr 0x2591
lantern  = chr 0x256C
block    = chr 0x2588
s3       = chr 0x23BB -- was: 0xF801
s7       = chr 0x23BC -- was: 0xF803
lEqual   = chr 0x2264
gEqual   = chr 0x2265
pi       = chr 0x03C0
nEqual   = chr 0x2260
sterling = chr 0x00A3

-- #if defined(__STDC_ISO_10646__)  && defined(HAVE_WADDNWSTR)
-- #else 


{-# LINE 1057 "Curses/Curses.hsc" #-}

recognize :: Char -> IO a -> ((Word64) -> IO a) -> IO a
{-# LINE 1059 "Curses/Curses.hsc" #-}
recognize ch noConvert convert
    | ch <= '\x7F'   = noConvert -- Handle the most common case first.
    | ch == ulCorner = convert =<< hs_curses_acs_ulcorner
    | ch == llCorner = convert =<< hs_curses_acs_llcorner
    | ch == urCorner = convert =<< hs_curses_acs_urcorner
    | ch == lrCorner = convert =<< hs_curses_acs_lrcorner
    | ch == rTee     = convert =<< hs_curses_acs_rtee
    | ch == lTee     = convert =<< hs_curses_acs_ltee
    | ch == bTee     = convert =<< hs_curses_acs_btee
    | ch == tTee     = convert =<< hs_curses_acs_ttee
    | ch == hLine    = convert =<< hs_curses_acs_hline
    | ch == vLine    = convert =<< hs_curses_acs_vline
    | ch == plus     = convert =<< hs_curses_acs_plus
    | ch == s1       = convert =<< hs_curses_acs_s1
    | ch == s9       = convert =<< hs_curses_acs_s9
    | ch == diamond  = convert =<< hs_curses_acs_diamond
    | ch == ckBoard  = convert =<< hs_curses_acs_ckboard
    | ch == degree   = convert =<< hs_curses_acs_degree
    | ch == plMinus  = convert =<< hs_curses_acs_plminus
    | ch == bullet   = convert =<< hs_curses_acs_bullet
    | ch == lArrow   = convert =<< hs_curses_acs_larrow
    | ch == rArrow   = convert =<< hs_curses_acs_rarrow
    | ch == dArrow   = convert =<< hs_curses_acs_darrow
    | ch == uArrow   = convert =<< hs_curses_acs_uarrow
    | ch == board    = convert =<< hs_curses_acs_board
    | ch == lantern  = convert =<< hs_curses_acs_lantern
    | ch == block    = convert =<< hs_curses_acs_block

{-# LINE 1087 "Curses/Curses.hsc" #-}
    | ch == s3       = convert =<< hs_curses_acs_s3
    | ch == s7       = convert =<< hs_curses_acs_s7
    | ch == lEqual   = convert =<< hs_curses_acs_lequal
    | ch == gEqual   = convert =<< hs_curses_acs_gequal
    | ch == pi       = convert =<< hs_curses_acs_pi
    | ch == nEqual   = convert =<< hs_curses_acs_nequal
    | ch == sterling = convert =<< hs_curses_acs_sterling

{-# LINE 1095 "Curses/Curses.hsc" #-}
    | otherwise      = noConvert

foreign import ccall unsafe hs_curses_acs_ulcorner :: IO (Word64)
{-# LINE 1098 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_llcorner :: IO (Word64)
{-# LINE 1099 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_urcorner :: IO (Word64)
{-# LINE 1100 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_lrcorner :: IO (Word64)
{-# LINE 1101 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_rtee     :: IO (Word64)
{-# LINE 1102 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_ltee     :: IO (Word64)
{-# LINE 1103 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_btee     :: IO (Word64)
{-# LINE 1104 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_ttee     :: IO (Word64)
{-# LINE 1105 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_hline    :: IO (Word64)
{-# LINE 1106 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_vline    :: IO (Word64)
{-# LINE 1107 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_plus     :: IO (Word64)
{-# LINE 1108 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_s1       :: IO (Word64)
{-# LINE 1109 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_s9       :: IO (Word64)
{-# LINE 1110 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_diamond  :: IO (Word64)
{-# LINE 1111 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_ckboard  :: IO (Word64)
{-# LINE 1112 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_degree   :: IO (Word64)
{-# LINE 1113 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_plminus  :: IO (Word64)
{-# LINE 1114 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_bullet   :: IO (Word64)
{-# LINE 1115 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_larrow   :: IO (Word64)
{-# LINE 1116 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_rarrow   :: IO (Word64)
{-# LINE 1117 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_darrow   :: IO (Word64)
{-# LINE 1118 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_uarrow   :: IO (Word64)
{-# LINE 1119 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_board    :: IO (Word64)
{-# LINE 1120 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_lantern  :: IO (Word64)
{-# LINE 1121 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_block    :: IO (Word64)
{-# LINE 1122 "Curses/Curses.hsc" #-}

{-# LINE 1123 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_s3       :: IO (Word64)
{-# LINE 1124 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_s7       :: IO (Word64)
{-# LINE 1125 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_lequal   :: IO (Word64)
{-# LINE 1126 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_gequal   :: IO (Word64)
{-# LINE 1127 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_pi       :: IO (Word64)
{-# LINE 1128 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_nequal   :: IO (Word64)
{-# LINE 1129 "Curses/Curses.hsc" #-}
foreign import ccall unsafe hs_curses_acs_sterling :: IO (Word64)
{-# LINE 1130 "Curses/Curses.hsc" #-}

{-# LINE 1131 "Curses/Curses.hsc" #-}


{-# LINE 1133 "Curses/Curses.hsc" #-}

{-# LINE 1134 "Curses/Curses.hsc" #-}

{-# LINE 1135 "Curses/Curses.hsc" #-}

{-# LINE 1136 "Curses/Curses.hsc" #-}

{-# LINE 1137 "Curses/Curses.hsc" #-}

{-# LINE 1138 "Curses/Curses.hsc" #-}

{-# LINE 1139 "Curses/Curses.hsc" #-}

{-# LINE 1140 "Curses/Curses.hsc" #-}

{-# LINE 1141 "Curses/Curses.hsc" #-}

{-# LINE 1142 "Curses/Curses.hsc" #-}

{-# LINE 1143 "Curses/Curses.hsc" #-}

{-# LINE 1144 "Curses/Curses.hsc" #-}

{-# LINE 1145 "Curses/Curses.hsc" #-}

{-# LINE 1146 "Curses/Curses.hsc" #-}

{-# LINE 1147 "Curses/Curses.hsc" #-}

{-# LINE 1148 "Curses/Curses.hsc" #-}

{-# LINE 1149 "Curses/Curses.hsc" #-}

{-# LINE 1150 "Curses/Curses.hsc" #-}

{-# LINE 1151 "Curses/Curses.hsc" #-}

{-# LINE 1152 "Curses/Curses.hsc" #-}

{-# LINE 1153 "Curses/Curses.hsc" #-}

{-# LINE 1154 "Curses/Curses.hsc" #-}

{-# LINE 1155 "Curses/Curses.hsc" #-}

{-# LINE 1156 "Curses/Curses.hsc" #-}

{-# LINE 1157 "Curses/Curses.hsc" #-}

{-# LINE 1158 "Curses/Curses.hsc" #-}

{-# LINE 1159 "Curses/Curses.hsc" #-}

{-# LINE 1160 "Curses/Curses.hsc" #-}

{-# LINE 1161 "Curses/Curses.hsc" #-}

{-# LINE 1162 "Curses/Curses.hsc" #-}

{-# LINE 1163 "Curses/Curses.hsc" #-}

{-# LINE 1164 "Curses/Curses.hsc" #-}

{-# LINE 1165 "Curses/Curses.hsc" #-}

{-# LINE 1166 "Curses/Curses.hsc" #-}


{-# LINE 1168 "Curses/Curses.hsc" #-}

-------------------------
-- code graveyard
-------------------------

 



{-# LINE 1216 "Curses/Curses.hsc" #-}
