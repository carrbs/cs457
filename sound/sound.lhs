> import Codec.Wav
> import Data.Audio
> import Data.Word
> import Data.Int
> import Data.Array.IArray
> import Data.List

> main = do w <- importFile "audacity/guitarMono.wav" -- "ui-shutdown.wav" "8k8bitpcm.wav" "audacity/guitar.wav"
>           case w of
>             Left msg    -> putStr ("fail: " ++ msg)
>             Right audio -> delay audio

> delay      :: Audio Int16 -> IO()
> delay audio = let samples    = sampleData audio
>                   xs         = elems samples
>                   both       = processor xs 16000 15
>                   out        = listArray (0, length both) both
>                   nsamples   = audio{sampleData=out}
>                in exportFile "newdelay.wav" nsamples

> processor xs n m | maximum xs > 1    = ys
>                  -- | m > 0               = ys 
>                  | otherwise         = xs
>                    where ys = zipWith (safePlus) (xs ++ replicate n 0) ds
>                          ds = processor (amp 0.66 (replicate n 0 ++ xs)) n (m-1)

Int16 are limited to this range: -32768 - 32767. For our purposes in this
program, the larger the abs(x), the larger the amplitude. We must keep
positive and negative numbers where they are, so this function will safely
add two numbers and keep there  sign. If the number is larger than the
"threshold" (somewhere's around abs(32767), we'll just use that "max" value
in place of the larger number.

> safePlus x y | x > 0 && y > 0 && s < 0 = max
>              | x < 0 && y < 0 && s > 0 = min
>              | otherwise               = s
>                where s   = x + y
>                      max = 32767  :: Int16
>                      min = -32768 :: Int16

> amp :: (RealFrac a1, Integral a, Integral b) => a1 -> [a] -> [b]
> amp n list    = let floats = map (*n) (map fromIntegral list)
>                  in map (round) floats

