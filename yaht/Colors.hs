module Colors where

import Data.Maybe

--data Color = Rgb Int Int Int
--    | Hsv Int Double Double
--
---- conversion: to RGB
--
--rgb :: Color -> Color
--rgb (Hsv h s v) =
--    let c = s * v
--        m = v - c
--        x = fromIntegral (c * (1 - ((h `mod` 2) - 1)))
--        triple x = (x, x, x)
--        addtripel (x, y, z) (u, v, w) = (x+u, y+v, z+w)
--        fromtripel (x, y, z) = Rgb x y z
--        basetripel
--            | hs == 0 = (0, 0, 0)
--            | hs >= 0 && hs < 1 = (c, x, 0)
--            | hs >= 1 && hs < 2 = (x, c, 0)
--            | hs >= 2 && hs < 3 = (0, c, x)
--            | hs >= 3 && hs < 4 = (0, x, c)
--            | hs >= 4 && hs < 5 = (x, 0, c)
--            | hs >= 4 && hs < 6 = (c, 0, x)
--            where hs = fromIntegral (h/60)
--    in  fromtripel (addtripel (basetripel) (triple m))
--
--colorList (Rgb x y z) = [x, y, z]

hsv2rgb :: (Int, Double, Double) -> (Double, Double, Double)
hsv2rgb (h, s, v) = 
    let c = s * v
        m = v - c
        x = fromIntegral (c * (1 - ((h `mod` 2) -1)))
        triple n = (n, n, n)
        addtriples (x, y, z) (u, v, w) = (x+u, y+v, z+w)
        basetriple
            | hs == 0 = (0, 0, 0)
            | hs >= 0 && hs < 1 = (c, x, 0)
            | hs >= 1 && hs < 2 = (x, c, 0)
            | hs >= 2 && hs < 3 = (0, c, x)
            | hs >= 3 && hs < 4 = (0, x, c)
            | hs >= 4 && hs < 5 = (x, 0, c)
            | hs >= 4 && hs < 6 = (c, 0, x)
            where hs = fromIntegral (h/60)
    in  basetriple

triple2list (x, y, z) = [x, y, z]
