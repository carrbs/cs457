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


> -- add two together
> delay    :: Audio Int16 -> IO()
> delay audio = let samples    = sampleData audio
>                   xs         = elems samples
>                   -- threshold  = (minimum xs, maximum xs)
>                   -- threshold  = (32768::Int16,32767::Int16)
>                   -- both       = zipWith ( safePlus (fst threshold) (snd threshold) ) xs xs
>                   -- both       = zipWith ( safePlus (fst threshold) (snd threshold) )
>                   --                      ( xs ++ replicate 4000 0 )
>                   --                      ( amp 0.5 (replicate 4000 0 ++ xs) )
>                   -- normal     = normalize threshold both
>                   both       = processor xs 16000
>                   out        = listArray (0, length both) both
>                   nsamples   = audio{sampleData=out}
>                in exportFile "newdelay.wav" nsamples--print (minimum normal) >> print  "^normal"
>                  --                    >> print (fst threshold)
>                  --                    >> print "fst threshold"
>                  --                    >> exportFile "foo.wav" nsamples


> processor xs n | (maximum xs) > 5000 = ys
>                | otherwise           = xs
>                  --where ys = zipWith (safePlus (32768) (32767) )
>                  where ys = zipWith (safePlus) (xs ++ replicate n 0) ds
>                        ds = processor (amp 0.66 (replicate n 0 ++ xs)) n

> (addZeroes n) xs = replicate n 0 ++ xs

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
>                      max = 32767::Int16
>                      min = 32768::Int16

> normalize threshold []     = []
> normalize threshold (x:xs) | (x >= 0) = if x > (snd threshold)
>                                           then (snd threshold : normalize threshold xs)
>                                           else ( x : normalize threshold xs)
>                            | (x < 0)  = if x < (fst threshold)
>                                           then (fst threshold : normalize threshold xs)
>                                           else ( x : normalize threshold xs)

pad :: "Int -> [Int] -> [[Int]]"
This function should take a list of numbers, and an int (repeat)
signifying how many lists to return. Each list will be padded by 2*
the delay amount (which I'm hardcoding at the moment but could easily
be parameterized)

> pad 0 list       = [list]
> pad repeats list = (list ++ replicate (repeats*(repeats-1)) 0) : pad (repeats-1) (replicate 3 0 ++ list)


>{-- 
> mix audio = let fst = elems (sampleData audio)
>                 snd = elems (sampleData audio)
> --}


> amp :: (RealFrac a1, Integral a, Integral b) => a1 -> [a] -> [b]
> amp n list    = let floats = map (*n) (map fromIntegral (list))
>                  in map (round) floats

