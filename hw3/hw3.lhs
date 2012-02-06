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


  In my version of padTo, I take the n and the list of strings. First
  I reverse the list... let's call the string "hello" and set n = 6,
  the result of reverse gives us:

                       "olleh"

  when we apply the next function (flip (++) [' ',' '..]), we are
  concating an infinite list of " "s to the end of "olleh":

                       "olleh        ..."
  but when we only take n of this result (here n = 6), we get:

                       "olleh "

  finally we reverse this list/string and get the desired result:

                      " hello"

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

  The function makeTable makes the list of rows I will use
  in multable (the argument for showTable) By utilizing
  the group function, we can use the list comprehension
  below and split it up each time we get to the last
  item in the list that y is using. Here's an example:

       makeTable 4 = group 4 [x*y | x <- [1..4], y <- [1..4] ]

  without the grouping, this comprehension takes the first
  element from the x generator (1) and multiplies it with
  each element from the y generator, storing the result in 
  a list:

      [1,2,3,4,...]

  Now we'lll go on to the next element in x's generator (2)
  and repeat the process:

      [1,2,3,4,2,4,6,8,...]

  If we let the whole thing go through all of the x values
  we get this list:

      [1,2,3,4,2,4,6,8,3,6,9,12,4,8,12,16]

  And then we can apply the group n to this list:
      [[1,2,3,4],[2,4,6,8],[3,6,9,12],[4,8,12,16]]

> makeTable     :: Int -> [[Int]]
> makeTable n   = group n [x*y | x <- [1..n], y <-[1..n]]


  Now that we have our sweet list of Ints, we need
  a sweet list of Strings with some padding.
  First off we need to convert all of our lists to
  Strings, I used (map.map) show to convert each element
  of the lists of Ints to Char:

     (map.map) show [[1,2,3,4],[2,4,6,8],[3,6,9,12],[4,8,12,16]]
      =
      [ ["1","2","3","4"],["2","4","6","8"],
        ["3","6","9","12"],["4","8","12","16"]  ]
  I used an auxilary function (padTable) to deal 
  with the padding:

      [ ["   1","   2","   3","   4"],
        ["   2","   4","   6","   8"],
        ["   3","   6","   9","  12"],
        ["   4","   8","  12","  16"] ]

  Once I had this list, I used map concat to smoosh
  the lists of Strings together:

      [ "   1   2   3   4",
        "   2   4   6   8",
        "   3   6   9  12",
        "   4   8  12  16"  ]

  unlines gives me the list as a string with new lines:

  "   1   2   3   4\n   2   4   6   8\n
      3   6   9  12\n   4   8  12  16\n"
      (sorry for the page wrap, making sure it all fits)

   this gets fed into putStr in multable, so we get
   the desired result:

            1   2   3   4
            2   4   6   8
            3   6   9  12
            4   8  12  16

> showTable     :: [[Int]] -> String
> showTable     = unlines
>               . map concat
>               . padTable
>               . (map . map) show

> padTable      :: [[String]] -> [[String]]
> padTable table = (map . map) (padTo (n+2)) table
>                   where n = maximum $ map maximum $ (map . map) length table 

> -- This was borrowed from week 2 lecture
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
>                               r <- allWays ((drop n) xs)    ]


    I beat my head against this one for a long time, trying
    to figure out how to acheive it using inits, splits, tails,
    I knew I wanted to use a list comprehension but I couldn't
    figure out exactly how to do it using those tools. Hope that
    wasn't a requirement! Anyway, I took this approach
    of appending Strings to a list comrehension where I call
    allWays on the different parts of the list. I worked through
    this on a whiteboard with Russell Miller and we came up
    with similar solutions. 
    
    Let's work through an example:

                    let xs = [1,2,3]

    when I call allWays xs the first thing that happens is
    I append show xs to the list comprehension:

                    [ "[1,2,3]" ... ]

    the ... in the list is the rest of the list comprehension.
    Similarly to the makeTable function we are taking the
    first item in l and using appString with all the items we
    get from r. The first time through, n = [1..(length xs) -1)]
    = [1..2] = 1, and we use this n to take/drop on our list
    (which at this point is [1,2,3]):

        l <- allWays ((take 1) [1,2,3] = allWays [1]
        r <- allWays ((drop 1) [1,2,3] = allWays [2,3]

    Now, since we need to run through all the options while
    l = [1], we call allWays on [2,3], which gives us a new
    list like this:

              ["[2,3]" ... ]

   but we need to finish this list, so we call the rest of
   this function:

        l <- allWays ((take 1) [2,3] = allWays [2]
        r <- allWays ((drop 1) [2,3] = allWays [3]

    and now our l = [2]

    We again call allWays in r, which gives us a new list:

        allWays [3] = show [3] : ...
              ["[3]" ...]

    But now our n <- [1..(length(xs) -1 ] = []
    so we don't really apply the allWays at this point.
    We return to the previous call (the one where our list
    is ["[2,3]" ... ] with this new list:

                      ["[3]"]

    Before we apply the appString, l = [2] gets passed to
    allWays, which returns (in a similar fashion to
    allWays [3]):

                      ["[2]"]

    Now we have the l and r that we can apply appString
    to:
        appString "[2]" "[3]"
        = "([2]++[3])"

    This string gets (:) to the list show [2] so now the 
    list is:
            ["[2,3]","([2]++[3])"]

    and we return to the previous call (where l = 1)
    bringing this list with us.

    Now that we have our complete list for r (when 
    l = allWays [1], The list that gets returned from
    allWays [1] = ["[1]"] (similar to above example
    on allWays [3]) we can start applying the appString
    at this "top" level:

            l <- ["[1]"]
            r <- ["[2,3]","([2]++[3])"]

    We are applying appString to these two lists
    (and consing them to the original ["[1,2,3]",...]
    list:

          appString "[1]" "[2,3]" = "([1]++[2,3])"
          appString "[1]" "([2]++[3])" = "([1]++([2]++[3]))"

    Now our top level list looks like this:

    [["[1,2,3]","([1]++[2,3])","([1]++([2]++[3]))",...]

    but we still have a "..." because we've only looked
    at the first item in our n list (namely 1), now we
    have to repeat this process when n = 2...

        l <- allWays ((take 2) [1,2,3] = allWays [1,2]
        r <- allWays ((drop 2) [1,2,3] = allWays [3]

    The return (as described earlier) from allWays [3]
    will be:
                        ["[3]"]

    also simlar to the return value of allWays [2,3]:

          allWays [1,2] =["[1,2]","([1]++[2])"]

    When we apply the appString to these two lists we get
    this list:

        ["([1,2]++[3])","(([1]++[2])++[3])"]

    Which gets appended to the original list:

    [ "[1,2,3]",
      "([1]++[2,3])",
      "([1]++([2]++[3]))",
      "([1,2]++[3])",
      "(([1]++[2])++[3])" ]

    And we're done!!

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

    to avoid the duplicates, we only need to call noParens on the
    rhs. This was actually the first thing I figured out, then 
    realized that it wasn't giving me all the values, so all the
    logic I worked through on the first half of this problem is
    essentially the same.


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
