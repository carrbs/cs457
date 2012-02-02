----------------------------------------------------------------------
CS457/557 Functional Languages, Winter 2012                 Homework 3
----------------------------------------------------------------------

Due: At the start of class on February 2, 2012 in person, or by noon
that same day if you submit by email.

Question 1:
-----------
a) Write a function padTo :: Int -> String -> String such that
padTo n s is a string of length n, padded if necessary with extra
spaces on the left.  For example:

  Main> padTo 6 "abc"
  "   abc"
  Main> padTo 4 "abc"
  " abc"
  Main> padTo 2 "abc"
  "bc"
  Main> 

[You are encouraged (although not required) to write your definition
of padTo by composing combine a pipeline of functions.]


> padTo :: Int -> String -> String
> padTo n   = reverse
>           . take n 
>           . flip (++) [' ',' '..] 
>           . reverse


b) The purpose of this question is to construct a function, multable,
that can output (square) multiplication tables of any given size, as
shown in the following examples:

 Main> multable 3
  1  2  3
  2  4  6
  3  6  9

  Main> multable 4
   1   2   3   4
   2   4   6   8
   3   6   9  12
   4   8  12  16

  Main> multable 10
    1    2    3    4    5    6    7    8    9   10
    2    4    6    8   10   12   14   16   18   20
    3    6    9   12   15   18   21   24   27   30
    4    8   12   16   20   24   28   32   36   40
    5   10   15   20   25   30   35   40   45   50
    6   12   18   24   30   36   42   48   54   60
    7   14   21   28   35   42   49   56   63   70
    8   16   24   32   40   48   56   64   72   80
    9   18   27   36   45   54   63   72   81   90
   10   20   30   40   50   60   70   80   90  100

  Main>

Your implementation should be constructed by replacing the "..."
portions of the following code with appropriate expressions.

> multable      = putStr . showTable . makeTable

> makeTable     :: Int -> [[Int]]
> makeTable n   = group n [x*y | x <- [1..n], y <-[1..n]]

> showTable     :: [[Int]] -> String
> showTable     = unlines
>               . map concat
>               . padTable
>               . (map . map) show

> padTable      :: [[String]] -> [[String]]
> padTable table = (map . map) (padTo (n+2)) table
>                   where n = maximum $ map maximum $ (map . map) length table 

> group     :: Int -> [a] -> [[a]]
> group n   = takeWhile (not.null)
>           . map (take n) 
>           . iterate (drop n)

You will probably need to use the padTo function that you defined
in Part (a).  You are also welcome to define additional auxiliary
functions to use in the definitions of makeTable or showTable if
you find it convenient to do so.  Note that multable should ensure
that all of the columns in the output are properly aligned.

Question 2:
-----------

a) There are many different ways to construct a non-empty list using
only non-empty list enumerations and the ++ operator.  For example,
the list [1,2,3] can be constructed by writing [1,2,3], ([1]++[2,3]),
or (([1]++[2])++[3]) (and these are not the only options).

Your task is to define a function

  allWays   :: [Integer] -> [String]
  allWays xs = ...

> allWays     :: [Integer] -> [String]
> allWays xs  = show xs : [ appString l r |
>                               n <- [1..(length(xs) - 1)],
>                               l <- allWays ((take n) xs ),
>                               r <- allWays ((drop n) xs)   ]


that will produce a list of strings that show all of the possible
ways to build the given list of integers in this way, provided that
the input list is not empty.  To help you to display the output
from this function in a readable manner, you may use the following
function:

> layout  :: [String] -> IO ()
> layout   = putStr
>          . unlines
>          . zipWith (\n l -> show n ++ ") " ++ l) [1..]

Remember also that you can convert an arbitrary list of integers
into a printable string by applying the show function.

For example, here is what you might see when you use these functions
together in a Hugs or GHCi session:

  Main> layout (allWays [1,2,3])
  1) [1,2,3]
  2) ([1]++[2,3])
  3) ([1]++([2]++[3]))
  4) ([1,2]++[3])
  5) (([1]++[2])++[3])

  Main>

Note that it is not necessary for your solution to list the generated
strings in the same order as shown in this input.  If you happen to
come up with a different variation of the code that lists them in a
different order, that will be just fine.  However, you should follow
the convention used above in which parentheses are placed around any
use of the ++ operator.  You may find it convenient to use the following
function to help construct strings that are in this format:

> appString    :: String -> String -> String
> appString l r = "(" ++ l ++ "++" ++ r ++ ")"

[hint: The splits function that was used in the natural language
processing example may also be very useful in this task.]

b) Of course, in practice, there is no need to include parentheses
in expressions that construct lists using the notation shown in
Part (a).  For example, ([1]++([2]++[3])) and (([1]++[2])++[3])
are equivalent to [1]++[2]++[3] because the ++ operator is
associative.  Write a new function:

  noParens   :: [Integer] -> [String]
  noParens xs = ...

> -- noParens [] = []
> noParens    :: [Integer] -> [String]
> noParens xs = show xs : [ appString2 l r |
>                               n <- [1..(length(xs) - 1) ],
>                               l <- [show ((take n) xs ) ],
>                               r <- noParens ((drop n) xs)   ]

> appString2    :: String -> String -> String
> appString2 l r =  l ++ "++" ++ r


that generates a list of strings showing all of the possible
ways to construct the given input list using only ++ and list
enumeration, without any repetition.  For example, here is an
example showing how this function might be used in a Haskell
interpreter:

  Main> layout (noParens [1,2,3])
  1) [1,2,3]
  2) [1]++[2,3]
  3) [1]++[2]++[3]
  4) [1,2]++[3]

  Main>

Note here that there are only 4 output lines in this particular
example, so you cannot produce the output for noParens simply
by removing the open and close parenthesis characters from the
output of allWays.  [hint: indeed, your definition of noParens
will probably have a different structure to your definition of
allWays, although it might still make good use of the splits
function ...]

> foobar = appString (show ( fst ( head (splits [1..3]) ) )) (appString ( show (
>           fst ( head (splits ( snd ( head (splits [1..3]) ) ) ) ) ) )   ( show ( snd (
>           head (splits ( snd ( head (splits [1..3]) ) ) ) ) ) ) ) : appString ( show (
>           fst ( head (splits [1..3]) ) ) ) ( show ( snd ( head (splits [1..3]) ) ) ) :
>           show [1..3] :[]

> tails       :: [a] -> [[a]]
> tails [x]    = []
> tails (x:xs) = xs : tails xs

> inits         :: [a] -> [[a]]
> inits [x]     = []
> inits (x:xs) = map (x:) ([]:inits xs)

> splits       :: [a] -> [([a],[a])]
> splits ts     = zip (inits ts) (tails ts)

----------------------------------------------------------------------
