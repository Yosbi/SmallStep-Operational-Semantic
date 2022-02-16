module RandomGen where
import Data.Time.Clock.POSIX
import System.IO.Unsafe



getRandomN :: Int -> Int
getRandomN x = if x == 0 then 
            ((1103515245 * getSeed(0)+ 12345) `mod` (2^32) `mod` 100)
        else
            ((1103515245 * x + 12345) `mod` (2^32) `mod` 100)

getSeed :: Int -> Int
getSeed x = unsafePerformIO(fmap round getPOSIXTime)