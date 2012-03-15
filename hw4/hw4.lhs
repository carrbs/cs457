----------------------------------------------------------------------
CS457/557 Functional Languages, Winter 2012                 Homework 4
Benjamin Carr
----------------------------------------------------------------------

### DISCLAMER ###
# This assignment has thoroughly confused for me the numbers/letters 0/O
# and I/1, my apologies for any mix up you find in my explanantions. I
# I ended up taking way too long on this assignment and ran out of time,
# so, apologies for crummy explanantions too. :)
#################

> data Bit = O | I    deriving (Show, Eq)

This datatype has two different values, written O and I, which we
will use to represent the bits 0 and 1.  

> type BinNum = [Bit]

a) Defining functions toBinNum & fromBinNum that convert
    backwards and forwards between Integers and their
    corresponding BitNum representations:

> toBinNum      :: Integer -> BinNum
> toBinNum 0    = [O]
> toBinNum 1    = [I]
> toBinNum n    | even n    = [O] ++ toBinNum (halfOfN)
>               | odd n     = [I] ++ toBinNum (halfOfN)
>                   where halfOfN = n `div` 2

Consider when n = 8:

        toBinNum 8 = [O] ++ toBinNum (halfOfN)

because 8 is an even number. Now halfOfN = 8 `div` 2:

        toBinNum 8 = [O] ++ toBinNum (8 `div` 2)

8 `div` 2 = 4, so now:

        toBinNum 8 = [O] ++ toBinNum (4)
        =
        [O] ++ [O] ++ toBinNum(2)

because 4 was even and 4 `div` 2 = 2. Now:

        [O] ++ [O] ++ [O] ++ toBinNum(1)

because 2 was even and 2 `div` 2 = 1. Now
In this case 1 is odd, But we hit this
function first:
toBinNum 1 = [I]

        [O] ++ [O] ++ [O] ++ [I]
        = [O,O,O,I]
Here is a quick test to show that this program 
is working properly:

Main> map toBinNum [0..16]
[   [O],[I],[O,I],[I,I],
    [O,O,I],[I,O,I],[O,I,I],[I,I,I],
    [O,O,O,I],[I,O,O,I],[O,I,O,I],[I,I,O,I],
    [O,O,I,I],[I,O,I,I],[O,I,I,I],[I,I,I,I],[O,O,O,O,I] ]
Main>

We can see that it is working for all of these numbers,
and will continue to work

Let's examine how these functions will provide
BinNums. First, we have the case where n = 0 or 1, the 
corresponding Bits are O,I and respectively, which are the 
first two cases. Now lets consider when n>1:
If n is even, then we will have to add a O Bit to scoot
everything over, however if n is odd, we need a I Bit:

n = 2:

n is even so we add a [O] to our BinNum and consider what
to do for the rest.

Binary numbers look like this: 10101.
Let's put that number in a comma separated list:

[1,0,1,0,1]

each Bit represents one of these values:

[1,2,4,8,16...] = [2^0, 2^1, 2^2, 2^3, 2^4, ...]

We can figure out what the base 10 value of a binary number
by looking at the Bit and making this decision:
1 = count the number
0 = don't count the number

so, the binary number: 1 0 1 0 1 = 1 + 4 + 16 = 21

Binary representation of base 10 numbers can be acheived
by dividing a number n by 2, and then continually
dividing the integer part of the result until n/2 = 0.
If n/2 is an odd number, we store the value 1, and if
n/2 is even, we store a zero.

Here is a quick example when n = 14:

        14/2    = 7.0 --> 7 is odd,  store a 1 --> [1,
        7/2     = 3.5 --> 3 is odd,  store a 1 --> [1,1,
        3/2     = 1.5 --> 1 is odd,  store a 1 --> [1,1,1,
        1/2     = 0.5 --> 0 is even, store a 0 --> [1,1,1,0,
        0/2     = 0   --> n/2 = 0, we're done  --> [1,1,1,0]

another quick example, n = 16:

        16/2    = 8.0 --> 8 is even, store a 0 --> [0,
        8/2     = 4.0 --> 4 is even, store a 0 --> [0,0,
        4/2     = 2.0 --> 2 is even, store a 0 --> [0,0,0,
        2/2     = 1.0 --> 1 is odd,  store a 1 --> [0,0,0,1,
        1/2     = 0.5 --> 0 is even, store a 0 --> [0,0,0,1,0,
        0/2     = 0   --> n/2 = 0, we're done  --> [


> fromBinNum :: BinNum -> Integer
> fromBinNum []     = 0
> fromBinNum (O:ds) = 2 * (fromBinNum ds)
> fromBinNum (I:ds) = 1 + (2 * (fromBinNum ds))

Consider what a binary number looks like:

[1,0,1,0,1]

This number is odd. The reason I can tell it is odd
is because of the least significant bit:

[1,  <-- that one, at the head of the list.

Now, I also know that adding 1 to an even number will
give me an odd one:

True == odd (1 + 2*n)  where n is any integer

This is true because 2 times any integer is even, and 1
plus an even is odd. Fact of math.

So, my function sees the 1 at the head of the list and
then adds one to (2 * something). the something is from
(fromBinNum ds) which happens to be the tail of the list:

[0,1,0,1]

Here is what we have do far:
    fromBinNum [1,0,1,0,1] = 1 + (2 * fromBinNum([O,I,O,I]))

And what we're gonna do is use the function where O is
at the head of the list:

    = 1 + ( 2 * ( 2 * fromBinNum([I,O,I]) ) )

Continuing with the pattern matching:

    = 1 + ( 2 * ( 2 * ( 1 + 2 * fromBinNum([O,I]) ) ) )
    = 1 + ( 2 * ( 2 * ( 1 + 2 * ( 2 * fromBinNum([I]) ) ) ) )
    = 1 + ( 2 * ( 2 * ( 1 + 2 * ( 2 * (1 + fromBinNum([]) ) ) ) ) )

We have a base case for fromBinNum [] = 0, so:

    = 1 + ( 2 * ( 2 * ( 1 + 2 * ( 2 * (1 + 0) ) ) ) ) )
    = 21

The way Binary numbers work is like this:

[1,2,4,8,16...] = [2^0, 2^1, 2^2, 2^3, 2^4, ...]




b) Defining a BinNum increment function without
    using either toBinNum or fromBinNum, that satisfies
    the property:    inc . toBinNum = toBinNum . (1+)

> inc           :: BinNum -> BinNum
> inc []        = []
> inc (I:O:ds)  = [O] ++ [I] ++ ds
> inc (I:I:ds)  = [O] ++ (inc (I:ds))
> inc (O:ds)    = [I] ++ ds
> inc [I]       = [O,I]

Essentially we're adding one to the list. This is how
Binary addition works:

0 + 0 = 0
1 + 0 = 1
0 + 1 = 1
1 + 1 = 0 (plus a carry bit 1)

I had to look beyond the head of the list to make this work.
If the head of the list has [1,0] at the front of the list,
incrementing is easy, you just replace the 1 with zero and 
the carry is simple because 0 + 1 = 1 (no carry). The second
function that takes a non-empty list sees that the first two
elements are Is, and so we know that we'll have to change the
first to I to an O, but we need to deal with the carry, so we
increment the rest of the list... at that time the I will become
a zero and the carry can be dealt with on this smaller list.
Perhaps an example would be useful:

consider this BinNum: [I,I,O,I,O,I]

Here our list has the condition [I:I:ds]

[I,I,O,I] = [O] ++ inc [I,O,I,O,I]

We now have the first case [I:O:ds], and this is where the
carry actually occurs:

          = [O] ++ [O] ++ [I] ++ [I,O,I]
          = [O,O,I,I,O,I]

This useful function takes care of the situation when
we have many Is preceding an O.

The next function is sees that if the first element is
an O, we simply change it to a I and we're done.

The final function takes care of the end of the list. Namely
if the final element in the list is an I. You can see that the
case where the BinNum has [I,O..ds] or [I,I..ds] is caught by
the first two functions via pattern matching, so the only way we
will fall to this function is if the list only contains a [I].
(I originally had this function written as:

    toBinNum (I:ds) = [O,I]

 but I think the way it is written now is easier to read)

Testing our functions for inc:

Main> map inc $ map toBinNum [0..16]
[   [I],[O,I],[I,I],[O,O,I],[I,O,I],
    [O,I,I],[I,I,I],[O,O,O,I],[I,O,O,I],
    [O,I,O,I],[I,I,O,I],[O,O,I,I],[I,O,I,I],
    [O,I,I,I],[I,I,I,I],[O,O,O,O,I],[I,O,O,O,I] ]

Main> map fromBinNum $ map inc $ map toBinNum [0..100]
[   1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
    21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
    38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
    55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,
    72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,
    89,90,91,92,93,94,95,96,97,98,99,100,101            ]

c) Defining the a function that computes the sum of its arguments.

> add :: BinNum -> BinNum -> BinNum
> add []     ds     = ds
> add ds     []     = ds
> add (O:ds) (e:es) = [e] ++ (add ds es)
> add (I:ds) (O:es) = [I] ++ (add ds es)
> {-- the above function could also be written:
> add (d:ds) (O:es) = [d] ++ (add ds es)
> but I think this is more explanatory --}
> add (I:ds) (I:es) = [O] ++ add [I] (add ds es)

Again the rules for binary addition:
0 + 0 = 0
0 + 1 = 1
1 + 0 = 1
1 + 1 = 0 (plus a carry bit 1)

The first function that takes two non-empty lists:

    add (O:ds) (e:es) = [e] ++ (add ds es)

is the simple case of when you add two binary numbers:

    O + e = e <-- e will either be I or O

(as you can see by the rules defined for binary addition)

The second function that takes two non-empty lists:
    add (I:ds) (O:es) = [I] ++ (add ds es)
is explicit about the head of the to BinNums, we know:

    O + I = I

So, we add an I to the front of the list, and continue
to add the rest of the BinNum list Bits. There is no
carry, so we can simply recursively call the add function
at this point.

The final function takes care of the situation
where we have Is at the beginning of both lists:

    add (I:ds) (I:es) = [O] ++ add [I] (add ds es)

Similar to the increment function we are adding the
zero to the head of the list, but now we deal with the
carry bit in this function, since (add ds es) will return
a BinNum, we can just add the I to whatever we get back
from that call (when the recursion is complete). Here is
a brief example:

let ds = [I,O,I] and es = [I,O,I]

    add [I,O,I] [I,O,I] = [O] ++ add [I] (add [O,I] [O,I])

Now we pattern match on the function
add (O:ds) (e:es) = [e] ++ (add ds es), for the expression
add [O,I] [O,I], and we get:

    = [0] ++ add [I] ([O] ++ (add [I] [I])

In this case we fall to the function that was we used first,
so:

    = [0] ++ add [I] ([O] ++ ( [O] ++ add [I] (add [] []) )

Now we fall to the case where one of the lists is empty:
add []     ds     = ds

since both of the lists are empty, ds = [], so we return
[] and step back to:

    = [O] ++ add [I] ([O] ++ ( [O] ++ add [I] []) )

Again we are at the case where one of our lists are empty,
add [I] [] = [I], so:

    = [O] ++ add [I] ([O] ++ ( [O] ++ [I]) )
    = [O] ++ add [I] [O,O,I]

apply add to two non-empty lists where we know we have I + O:

 add (I:ds) (O:es) = [I] ++ (add ds es)

    = [O] ++ [I] ++ add ([] [O,I])
    = [O] ++ [I] ++ [O,I]
    = [O,I,O,I]

Testing:

I made a function called checkAdd that takes two Integers, x y
and passes those number to addList and add_List, both of
which create lists of BinNums [0..x], [0..y] in a list comprehension
that get fed to my add function and the one given in the 
homework assignment (the expected result). I named this
function. The goal here is to compare the result of my
functions with the function that converts BinNums to Ints,
adds them, and then converts them back to BinNums.

Main> checkAdd 100 100
True
Main>

> checkAdd      :: Integer -> Integer -> Bool
> checkAdd x y  = (addList x y) == (add_List x y)

> add_      :: BinNum -> BinNum -> BinNum
> add_ x y  = toBinNum (fromBinNum x + fromBinNum y)

> add_Int       :: Integer -> Integer -> BinNum
> add_Int x y   = add_ (toBinNum x)  (toBinNum y)

> addInt x y   = add (toBinNum x) (toBinNum y)

> add_List x y = [ add_Int a b | a <-[0..x], b <-[0..y] ]
> addList x y  = [ addInt a b | a <-[0..x], b <-[0..y] ]

d) Defining a function that computes the product of its 
arguments (without converting them to Integer values first).

I did some divide and conquer for this one.

First off the rules for Bit multiplication:
0 * 0 = 0
0 * 1 = 0
1 * 0 = 0
1 * 1 = 1, and no carry or borrow bits

I have a function that takes care of this,
called mulBit.

Here is the way I approached this problem
Consider the multiplication:

        1 0 1 0
      x 1 0 1 0
      ---------
        0 0 0 0
   (1)1 0 1 0 0 <-- that (1) is a carry
    0 0 0 0 0 0
+ 1 0 1 0 0 0 0
---------------
  1 1 0 0 1 0 0

To mul the BinNums xs, ys, you take the first bit 
from xs and multiply it by all the bits in ys.
That is what my function multOne does.

When you're done with the first bit from xs, its
time to move on to the next one, adding a zero at the
head to scoot stuff over. This is how multTwo works:

Main> multTwo [O,I,O,I] [O,I,O,I]
[[O,O,O,O],[O,O,I,O,I],[O,O,O,O,O,O],[O,O,O,O,I,O,I]]

(Notice how these rows are identical to the rows from
my long multipliation example)
Then we add these BinNums up, using foldr we get 
something like:

[[O,O,O,O] + [O,O,I,O,I] + [O,O,O,O,O,O] + [O,O,O,O,I,O,I]]

Main> mul [O,I,O,I] [O,I,O,I]
[O,O,I,O,O,I,I]

> mul :: BinNum -> BinNum -> BinNum
> mul xs ys = foldr add [] (multTwo xs ys)

> multTwo [] ys = []
> multTwo ys [] = []
> multTwo (x:xs) ys = [multOne x ys] ++ multTwo xs (O:ys)

> multOne       :: Bit -> BinNum -> BinNum
> multOne x ys = [mulBit x y | y <- ys]

> mulBit        :: Bit -> Bit -> Bit
> mulBit O y    = O
> mulBit x O    = O
> mulBit I I    = I

I didn't have time to make a cool function to test, I 
got stuck trying to remember how to make an Eq funciton,
and now have to go to class.
Hopefully this gives some proof that I have done a bit of
testing:


Main> [(mul x y,fromBinNum x, fromBinNum y) |
            x <- (map toBinNum [0..5]), y <- (map toBinNum [1..5])]

[   ([O],0,1),([O,O],0,2),([O,O],0,3),([O,O,O],0,4),([O,O,O],0,5),
    ([I],1,1),([O,I],1,2),([I,I],1,3),([O,O,I],1,4),([I,O,I],1,5),
    ([O,I],2,1),([O,O,I],2,2),([O,I,I],2,3),([O,O,O,I],2,4),
    ([O,I,O,I],2,5),([I,I],3,1),([O,I,I],3,2),([I,O,O,I],3,3),
    ([O,O,I,I],3,4),([I,I,I,I],3,5),([O,O,I],4,1),([O,O,O,I],4,2),
    ([O,O,I,I],4,3),([O,O,O,O,I],4,4),([O,O,I,O,I],4,5),([I,O,I],5,1),
    ([O,I,O,I],5,2),([I,I,I,I],5,3),([O,O,I,O,I],5,4),([I,O,O,I,I],5,5) ]

These functions are the same as the checkAdd stuff from the previous
problem.

> checkMul      :: Integer -> Integer -> Bool
> checkMul x y  = (mulList x y) == (mul_List x y)

> mul_      :: BinNum -> BinNum -> BinNum
> mul_ x y  = toBinNum (fromBinNum x * fromBinNum y)

> mul_Int       :: Integer -> Integer -> BinNum
> mul_Int x y   = mul_ (toBinNum x)  (toBinNum y)

> mulInt x y   = mul (toBinNum x) (toBinNum y)

> mul_List x y = [ mul_Int a b | a <-[1..x], b <-[1..y] ]
> mulList x y  = [ mulInt a b | a <-[1..x], b <-[1..y] ]

Main> checkMul 100 100
True

----------------------------------------------------------------------
