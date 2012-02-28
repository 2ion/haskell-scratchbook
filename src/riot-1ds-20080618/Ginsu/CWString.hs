{-# LINE 1 "Ginsu/CWString.hsc" #-}
{-# OPTIONS -fglasgow-exts -ffi #-}
{-# LINE 2 "Ginsu/CWString.hsc" #-}
-- arch-tag: 72067bff-05e1-4c0e-94aa-34b54f437d92

module Ginsu.CWString (
    -- utf8 versions
    withUTF8String,
    withUTF8StringLen,
    newUTF8String,
    newUTF8StringLen,
    peekUTF8String,
    peekUTF8StringLen,
    -- wchar stuff

{-# LINE 25 "Ginsu/CWString.hsc" #-}
    -- locale versions 
    withLCString,
    withLCStringLen,
    newLCString,
    newLCStringLen,
    peekLCStringLen,
    peekLCString,
    charIsRepresentable

    ) where

import Data.Bits

{-# LINE 45 "Ginsu/CWString.hsc" #-}
import Foreign.C.String
import Foreign.C.Types

{-# LINE 48 "Ginsu/CWString.hsc" #-}
import qualified CForeign
import Char
import Foreign
import Monad
import GHC.Exts
import IO



{-# LINE 57 "Ginsu/CWString.hsc" #-}

{-# LINE 58 "Ginsu/CWString.hsc" #-}

{-# LINE 59 "Ginsu/CWString.hsc" #-}



{-# LINE 199 "Ginsu/CWString.hsc" #-}
-- no __STDC_ISO_10646__
wcharIsUnicode = False

{-# LINE 202 "Ginsu/CWString.hsc" #-}

--
-- LCString
--


{-# LINE 290 "Ginsu/CWString.hsc" #-}
-- no CF_WCHAR_SUPPORT

charIsRepresentable :: Char -> IO Bool
charIsRepresentable ch = return $ isLatin1 ch

withLCString = withCString
withLCStringLen = withCStringLen
newLCString = newCString
newLCStringLen = newCStringLen
peekLCString = peekCString
peekLCStringLen = peekCStringLen


{-# LINE 303 "Ginsu/CWString.hsc" #-}
-- no CF_WCHAR_SUPPORT


-----------------
-- UTF8 versions
-----------------


withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String hsStr = CForeign.withCString (toUTF hsStr)

withUTF8StringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8StringLen hsStr = CForeign.withCStringLen (toUTF hsStr)

newUTF8String :: String -> IO CString
newUTF8String = CForeign.newCString . toUTF

newUTF8StringLen :: String -> IO CStringLen
newUTF8StringLen = CForeign.newCStringLen . toUTF

peekUTF8String :: CString -> IO String
peekUTF8String strPtr = fmap fromUTF $ CForeign.peekCString strPtr

peekUTF8StringLen :: CStringLen -> IO String
peekUTF8StringLen strPtr = fmap fromUTF $ CForeign.peekCStringLen strPtr


-- these should read and write directly from/to memory.
-- A first pass will be needed to determine the size of the allocated region

toUTF :: String -> String
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = x:toUTF xs
	     | ord x<=0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs
	     | otherwise     = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			       chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs

fromUTF :: String -> String
fromUTF [] = []
fromUTF (all@(x:xs)) | ord x<=0x7F = x:fromUTF xs
		     | ord x<=0xBF = err
		     | ord x<=0xDF = twoBytes all
		     | ord x<=0xEF = threeBytes all
		     | otherwise   = err
  where
    twoBytes (x1:x2:xs) = chr (((ord x1 .&. 0x1F) `shift` 6) .|.
			       (ord x2 .&. 0x3F)):fromUTF xs
    twoBytes _ = error "fromUTF: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr (((ord x1 .&. 0x0F) `shift` 12) .|.
				    ((ord x2 .&. 0x3F) `shift` 6) .|.
				    (ord x3 .&. 0x3F)):fromUTF xs
    threeBytes _ = error "fromUTF: illegal three byte sequence" 
    
    err = error "fromUTF: illegal UTF-8 character"


