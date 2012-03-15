> import Codec.Wav
> import Data.Audio
> import Data.Word
> import Data.Int
> import Data.Array.IArray
> import Data.List

> main = do w <- importFile "audacity/guitarMono.wav" -- "ui-shutdown.wav" "8k8bitpcm.wav" "audacity/guitar.wav"
>           case w of
>             Left msg    -> putStr ("fail: " ++ msg)
>             -- Right audio -> delay audio
>             Right audio -> foo audio
>             -- Right audio -> fast audio
>             -- Right audio -> process audio
>             -- Right audio -> same audio
>             -- Right audio -> slow audio
>             -- Right audio -> delay2 audio 1
>             -- Right audio -> newDelay repeats audio
>             -- Right audio -> print (display audio)


> -- add two together
> foo :: Audio Int16 -> IO()
> foo audio = let samples    = sampleData audio
>                 xs         = elems samples
>                 threshold  = (minimum xs, maximum xs)
>                 -- both       = zipWith (safePlus (fst threshold) (snd threshold)) xs xs
>                 both       = zipWith (safePlus (fst threshold) (snd threshold))
>                                      (xs ++ replicate 2000 0)
>                                      (amp 0.5 (replicate 2000 0 ++ xs))
>                 normal     = normalize threshold both
>                 out        = listArray (0, length normal) normal
>                 -- out2       = listArray (0, length xs) xs
>                 nsamples   = audio{sampleData=out}
>              in exportFile "newdelay.wav" nsamples--print (minimum normal) >> print  "^normal"
>                  --                    >> print (fst threshold)
>                  --                    >> print "fst threshold"
>                  --                    >> exportFile "foo.wav" nsamples

Int16 are limited to this range: -32768 - 32767. For our purposes in this
program, the larger the abs(x), the larger the amplitude. We must keep
positive and negative numbers where they are, so this function will safely
add two numbers and keep there  sign. If the number is larger than the
"threshold" (somewhere's around abs(32767), we'll just use that "max" value
in place of the larger number.

> safePlus min max x y |  x > 0 && y > 0 && s < 0 = max
>                      |  x < 0 && y < 0 && s > 0 = min
>                      |  otherwise               = s
>                      where s = x+y

> normalize threshold []     = []
> normalize threshold (x:xs) | (x >= 0) = if x > (snd threshold)
>                                           then (snd threshold : normalize threshold xs)
>                                           else ( x : normalize threshold xs)
>                            | (x < 0)  = if x < (fst threshold)
>                                           then (fst threshold : normalize threshold xs)
>                                           else ( x : normalize threshold xs)

> {--newDelay repeats audio = let samples      = sampleData audio
>                              list         = elems samples
>                              lists        = pad repeats list
> --}

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

> delay :: Audio Int16 -> IO()
> delay audio = let samples    = sampleData audio
>                   list       = elems samples
>                   source     = skip list
>                   combined   = alternate source ((take 32000 (repeat 0)) ++ source)
>                   -- out        = listArray (0, 10000000) (take 1000 (repeat 0) ++ source)
>                   out        = listArray (0, (length combined)) combined
>                   nsamples   = audio{sampleData=out}
>                   --in print (minimum list) >> exportFile "delay.wav" nsamples
>                in exportFile "delay.wav" nsamples
> 

>
> delay2        :: Audio Int16 -> Int -> IO()
> delay2 audio n = if (n>0) then 
>                           let samples = sampleData audio
>                               list       = elems samples
>                               source     = skip list
>                               padded     = replicate 8000 0 ++ source
>                               amped      = amp 0.5 padded
>                               combined   = source `alternate` amped
>                               out        = listArray (0, length combined) combined
>                               nsamples   = audio{sampleData=out}
>                            in delay2 nsamples (n-1)--exportFile "delay2.wav" nsamples
>                  else exportFile "delay2.wav" audio
> 
> {--
> helper audio  =  let samples    = sampleData audio
>                      list       = elems samples
>                      source     = skip list
>                      padded     = replicate 16000 0 ++ source
>                      amped      = amp 0.5 padded
>                      combined   = source `alternate` amped
>                      out        = listArray (0, length combined) combined
>                      nsamples   = audio{sampleData=out}
>                      case n == 0 of
>                       False -> delay audio (n-1)
> --}

> amp :: (RealFrac a1, Integral a, Integral b) => a1 -> [a] -> [b]
> amp n list    = let floats = map (*n) (map fromIntegral (list))
>                  in map (round) floats

> fast :: Audio Int16 -> IO()
> fast audio = let samples   = sampleData audio
>                  list      = elems samples
>                  shortList = skip list
>                  out       = listArray (bounds samples) shortList
>                  nsamples  = audio{sampleData=out}
>                in exportFile "short.wav" nsamples



> skip :: [Int16] -> [Int16]
> skip []       = []
> skip (x:_:xs) = x : skip xs
> skip xs       = xs

> -- process       :: Audio Word8 -> IO()
> process       :: Audio Int16 -> IO()
> process audio = let samples  = sampleData audio
>                     rev      = listArray (bounds samples) (reverse (elems samples))
>                     nsamples = audio{sampleData=rev}
>                  in exportFile "backwards.wav" nsamples

> same :: Audio Int16 -> IO()
> same audio = let samples  = sampleData audio
>                  same     = listArray (bounds samples) (elems samples)
>                  nsamples = audio{sampleData=same}
>               in exportFile "same.wav" nsamples
>

> slow :: Audio Int16 -> IO()
> slow audio = let samples   = sampleData audio
>                  list      = elems samples
>                  combined  = alternate list list
>                  out       = listArray (bounds samples) combined
>                  nsamples  = audio{sampleData=out}
>                in exportFile "longer.wav" nsamples

> alternate []     []     = []
> alternate (xs)   []     = xs
> alternate []     (ys)   = ys
> alternate (x:xs) (y:ys) = [x,y] ++ alternate xs ys


>--                    in print samples
>                   --   (lo, hi) = bounds samples
>                  -- in mapM_ (\i -> putStrLn ("sample "++ show i ++": " ++
>                     --                       show (samples!i)))
>                    --      [lo..hi]
>
