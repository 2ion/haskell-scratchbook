{-# OPTIONS_GHC -optc-DCF_CHARSET_SUPPORT #-}
{-# OPTIONS_GHC -optc-DCF_WCHAR_SUPPORT #-}
{-# OPTIONS_GHC -optc-DHAVE_WADDNWSTR #-}
{-# OPTIONS_GHC -optc-DHAVE_RESIZETERM #-}
{-# OPTIONS_GHC -optc-DGHC64 #-}
{-# LINE 1 "Ginsu/Locale.hsc" #-}
{-# OPTIONS -fglasgow-exts -ffi -#include <locale.h> #-}
{-# LINE 2 "Ginsu/Locale.hsc" #-}
-- arch-tag: d48a3194-c698-43c7-b581-08e7a213f0c8
module Ginsu.Locale(
    setupLocale,
    getCharset,
    getDateFmt,
    getDateTimeFmt,
    getTimeFmt,
    getYesRegex,
    getNoRegex
    -- nl_langinfo 
    ) where

import Foreign
import Foreign.C
import Char
import GHC.Exts



{-# LINE 21 "Ginsu/Locale.hsc" #-}

{-# LINE 22 "Ginsu/Locale.hsc" #-}

foreign import ccall unsafe "locale.h setlocale" setlocale :: CInt -> Addr# -> IO (Ptr CChar)
foreign import ccall unsafe "langinfo.h nl_langinfo" nl_langinfo :: (Int32) -> IO (Ptr CChar)
{-# LINE 25 "Ginsu/Locale.hsc" #-}

setupLocale :: IO ()
setupLocale = setlocale (6) ""# >> return () 
{-# LINE 28 "Ginsu/Locale.hsc" #-}

getCharset :: IO String

{-# LINE 31 "Ginsu/Locale.hsc" #-}
getCharset =  nl_langinfo (14) >>= peekCString   
{-# LINE 32 "Ginsu/Locale.hsc" #-}

{-# LINE 35 "Ginsu/Locale.hsc" #-}

getDateFmt :: IO String
getDateFmt = nl_langinfo (131113) >>= peekCString
{-# LINE 38 "Ginsu/Locale.hsc" #-}

getDateTimeFmt :: IO String
getDateTimeFmt = nl_langinfo (131112) >>= peekCString
{-# LINE 41 "Ginsu/Locale.hsc" #-}

getTimeFmt :: IO String
getTimeFmt = nl_langinfo (131114) >>= peekCString
{-# LINE 44 "Ginsu/Locale.hsc" #-}

getYesRegex :: IO String
getYesRegex = nl_langinfo (327680) >>= peekCString
{-# LINE 47 "Ginsu/Locale.hsc" #-}

getNoRegex :: IO String
getNoRegex = nl_langinfo (327681) >>= peekCString
{-# LINE 50 "Ginsu/Locale.hsc" #-}

{- 
LC_COLLATE
    Affects the behavior of regular expressions and the collation functions.
LC_CTYPE
    Affects the behavior of regular expressions, character classification,
    character conversion functions, and wide-character functions.
LC_MESSAGES
     Affects what strings are expected by commands and utilities as affirmative
     or negative responses.  It also affects what strings are given by commands
     and utilities as affirmative or negative responses, and the content of
     messages. 
LC_MONETARY
    Affects the behavior of functions that handle monetary values.
LC_NUMERIC
    Affects the behavior of functions that handle numeric values.
LC_TIME
    Affects the behavior of the time conversion functi

data Locale = LC_CTYPE | LC_NUMERIC | LC_TIME | LC_COLLATE | LC_MONETARY | LC_MESSAGES | LC_ALL | LC_PAPER | LC_NAME | LC_ADDRESS | LC_TELEPHONE | LC_MEASUREMENT | LC_IDENTIFICATION
    deriving(Show, Enum, Read, Ord, Eq)

decodeLocale :: Locale -> CInt 
decodeLocale LC_CTYPE = (#const LC_CTYPE)
decodeLocale LC_NUMERIC = (#const LC_NUMERIC)
decodeLocale LC_COLLATE = (#const LC_COLLATE)
decodeLocale LC_MONETARY = (#const LC_MONETARY)
decodeLocale LC_TIME = (#const LC_TIME)
#ifdef LC_MESSAGES 
decodeLocale LC_MESSAGES = (#const LC_MESSAGES)
#endif
#ifdef LC_PAPER
decodeLocale LC_PAPER = (#const LC_PAPER)
#endif
#ifdef LC_NAME
decodeLocale LC_NAME = (#const LC_NAME)
#endif
#ifdef LC_ADDRESS
decodeLocale LC_ADDRESS = (#const LC_ADDRESS)
#endif
#ifdef LC_TELEPHONE
decodeLocale LC_TELEPHONE = (#const LC_TELEPHONE)
#endif
#ifdef LC_MEASUREMENT
decodeLocale LC_MEASUREMENT = (#const LC_MEASUREMENT)
#endif
#ifdef LC_IDENTIFICATION
decodeLocale LC_IDENTIFICATION = (#const LC_IDENTIFICATION)
#endif
decodeLocale _ = -1

dString :: IO (Ptr CChar) -> IO (Maybe String)
dString action = do 
    v <- action
    if v == nullPtr then return Nothing else fmap Just (peekCString v)

setLocaleAll :: String -> IO (Maybe String)
setLocaleAll s =  dString $ withCString s (setlocale (#const LC_ALL))

getLocaleAll :: IO (Maybe String)
getLocaleAll = getLocale LC_ALL

setLocale :: Locale -> String -> IO (Maybe String)
setLocale l s = case decodeLocale l of
    -1 -> return Nothing
    nl -> dString $ withCString s (setlocale nl)

getLocale :: Locale -> IO (Maybe String)
getLocale l = case decodeLocale l of 
    -1 -> return Nothing
    nl -> dString $ setlocale nl nullPtr 
-}
