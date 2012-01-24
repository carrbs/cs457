Ben Carr
CS457 - Winter 2012
Homework 1

Question 1:
-----------
Experiment with the fractal program that we constructed in class, or with one of the versions that is available from the class web page. Your goal is to produce a new and attractive fractal image, no bigger than a page, by modifying some (or all) of the parameters such as the range of points, the size of the grid, the characters in the palette, or even the fractal function.   Every student should submit a different image.  Be sure to describe the changes that you've made.  The purpose of this question is to make sure that you understand how the fractal program works, and that you are comfortable modifying and running it. So make sure that you achieve those goals, but don't spend too long on this question (although I might give an "artistic" award to the person whose fractal image seems "prettiest" and/or most unusual to me ... :-)

AMOEBA! I changed the values that julia takes in the function juliaFrac in the Fractals.lhs file:  juliaFrac = julia (1.01,0.343)
                            ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                      
                         ,,,,,......,,,,,,,,,,,,,,,,,,,,,,,,                   
                        ,,,,.`~""~"....,,,,,,,,....`~``~`.,,,,                 
                      ,,,,,..`""````..............`~-"""`..,,,,,               
                     ,,,,,...``````...............```````....,,,,              
                    ,,,,,..`````````...............``````````.,,,,             
                    ,,,,.`:"""``````.................````"""~..,,,             
                    ,,,,..`"""~:~```...................``"~:`.,,,,             
                    ,,,,,..`"~~~~``......................``..,,,,,             
                    ,,,,,,...```..........................,,,,,,,              
                    ,,,,,,,,...............,,,,,,,,,,,,,,,,,,,,,               
                     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                
                      ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                  
                       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                   
                       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                     
                        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                      
                        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                       
                        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                        
                        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                        
                        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                        
                       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                        
                      ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                        
                     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                       
                   ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                       
                  ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                      
                ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                     
               ,,,,,,,,,,,,,,,,,,,,,...............,,,,,,,,                    
              ,,,,,,,..........................```...,,,,,,                    
             ,,,,,..``......................``~~~~"`..,,,,,                    
             ,,,,.`:~"``...................```~:~"""`..,,,,                    
             ,,,..~"""````.................``````""":`.,,,,                    
             ,,,,.``````````...............`````````..,,,,,                    
              ,,,,....```````...............``````...,,,,,                     
               ,,,,,..`"""-~`..............````""`..,,,,,                      
                 ,,,,.`~``~`....,,,,,,,,...."~""~`.,,,,                        
                   ,,,,,,,,,,,,,,,,,,,,,,,,......,,,,,                         
                      ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,        

Question 2:
-----------
- powerOfTwo :: Int -> Integer
 (powerOfTwo n) returns the value of 2 to the power n.  For example,
 powerOfTwo 8 should return 256.  Of course, your answer should *not*
 use the built in Haskell functions for raising a value to a power :-)

> powerOfTwo n = product (take n (repeat 2))

Concerned that I wasn't supposed to be using the "^" operator, I wrote this
recursively, but I realized I wasn't supposed to use recursion, so this
is not a valid solution for the assignment:

powerOfTwo :: Int -> Integer
powerOfTwo 0 = 1
powerOfTwo x = 2 * (powerOfTwo (x-1))

I started down a rabbit hole trying to figure out this stuff 
(not realizing I already had a valid solution :( ...) and 
ended up with some funky stuff that might be helpful at 
some point later:

myPairs n = zip [1..n+1] (map (2^) [1..n+1])
powerOfTwo x = snd ( last ( init ( myPairs x ) ) )

I think the easiest would be:
powerOfTwo x = 2^x

- logTwo :: Integer -> Int
  (logTwo v) returns the smallest integer n such that v < powerOfTwo n.

> logTwo :: Integer -> Int
> logTwo v = length (takeWhile (<v) (iterate (2*) 1)) + 1

Recursively:
 logTwo 0 = 1
 logTwo 1 = 1
 logTwo x = ( logTwo ( x `div` 2 ) ) + 1

- copy :: Int -> a -> [a]
  (copy n x) returns a list containing n copies of the value x.  For
  example, copy 3 True should give [True, True, True].  (The Haskell
  prelude includes a similar function called replicate; of course,
  you should not use replicate in your answer; you should not replicate
  replicate either :-)

> copy :: Int -> a -> [a]
> copy n x = take n (repeat x)

Recursively:
 copy 0 x = []
 copy n x = x:( copy (n-1) x )

- multiApply :: (a -> a) -> Int -> a -> a
  (multiApply f n x)  returns the value that is obtained when the
  function f is applied n times to the value x.  For example,
  multiApply (\x -> x*x) 2 2, should return 16, while
  multiApply not 0 True will return True.

> multiApply :: (a -> a) -> Int -> a -> a
> multiApply f 0 x = x
> multiApply f n x = last (take n (iterate (f) (f x)))

Recursively:
 multiApply f 0 x = x
 multiApply f n x = f ( multiApply f (n-1) x )

Now suppose that we define the following function using multiApply:

> q f n m x     = multiApply (multiApply f n) m x

What is the type of this function, and what exactly does it do?
What law of multiApply does this suggest?

Here is the type (from Hugs):

q :: (a -> a) -> Int -> Int -> a -> a

the function q applies f to x n*m times:
q f n m x   = multiApply (multiApply f n) m x
            = multiApply f (n*m) x

Here's an example from the Hugs interpreter:
---
Main> q (\x -> x*x) 2 2 2
65536
Main> multiApply (\x -> x*x) (2*2) 2
65536
Main> q not 1 3 True
False
Main> multiApply not (1*3) True
False
---

Question 3:
-----------
The Haskell prelude includes functions zip and zipWith that can
be used for manipulating pairs of lists:

  zip:: [a] -> [b] -> [(a,b)]
  zipWith :: (a -> b -> c) -> ([a] -> [b] -> [c])

a) Use the online documentation, experiment at the interpreter prompt,
  or browse the prelude source code to find out what these functions
  do, and write a brief explanation to summarize what you have learned,
  including some simple, illustrative examples.

zip takes a list of as = [a_1, a_2,..,a_n] 
and a list of bs = [b_1, b_2,..,b_m]

and makes either a new list of tuples:

cs = [(a_1,b_1), (a_2, b_2),..,(a_n, b_n)] // if n <= m

OR

cs = [(a_1,b_1), (a_2, b_2),..,(a_m, b_m)] // if m <= n

The types of as and bs do not need to match:
Main> zip [1,2,3] ["a","b"] = [(1,"a"),(2,"b")]


zipWith takes a function and two lists and applies 
the function to each item in both lists. for example:

zipWith (+) [1,2,3] [4,5,6] = [5,7,9]

here the function (+) has a type that is: 

(+) :: Num a => a -> a -> a 

because the types for this function must match, it 
depends on the function zipWith is given as a 
parameter whether or not zipWith can apply the function.

For example:

as = [1,2,3], bs = ["a", "b", "c", "d"]

zip as bs = [(1,"a"), (2,"b"), (3,"c")]

zipWith (+) as bs  = ERROR! because the (+) function requires 
both types of list to be Num:

Hugs> :t zipWith (+)
zipWith (+) :: Num a => [a] -> [a] -> [a]

I'm trying to think of a way to apply zipWith to lists with
different types, here's what I came up with:

Main> zipWith zip [[1]] [["a"]]
[[(1,"a")]]

In this case, since the function zip can take any type, zipWith
can apply that function to the lists that have different types.

b) Show how the zipWith function can be defined in terms of zip,
   without explicit recursion.  [Hint: use a list comprehension.]
   (In fact, we can also define zip in terms of zipWith, but I haven't
   quite shown you all the language features we need for that yet.)

> zipWith_ :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith_ f xs ys = [f x y | (x,y) <- (zip xs ys)]

This feels weird, but I think it's right (it seems right with
the output I'm getting anyway :-) ). What we do is first zip the two
lists, so we get something like:

            [1,2,3] [4,5,6] -> [(1,4), (2,5), (3,6)] 

Now we're gonna take that list and apply whatever function
is given to each tuple in the list (zipWith takes two lists 
containing items of the same type and a function that can be
applied to items of that type. I think this is working 
because of the "Sections" thing, rather you can state a function
(like "*" for example) as: (*) 2 3, meaning multiply the two terms
given after the function. So if we used this "*" as our function,
we should get the result:

                        [4,10,18]

c) Write a function, adjacents :: [a] -> [(a,a)], whose output lists
  the adjacent pairs of elements in its input.  For example:
  adjacents [1,3,5,6,7,2] = [ (1,3), (3,5), (5,6), (6,7), (7,2) ]

> adjacents :: [a] -> [(a,a)]
> adjacents x = zip x (tail x)

d) Pascal's triangle is made up of a series of rows of integers in
   which each number in the body of the table is the sum of the two
   numbers immediately above it on the left and the right.  The
   following shows the first few rows of Pascal's triangle (be sure
   to view this in a fixed width font!):
   ...
   Define a function, nextRow :: [Integer] -> [Integer], that will
   produce the (n+1)th row in Pascal's triangle given the nth row
   as its input.  Show how nextRow can be used in a function that
   computes Pascal's triangle.

> nextRow :: [Integer] -> [Integer]
> nextRow [0] = [1]
> nextRow xs = [ x+y | (x,y) <- (adjacents ([0] ++ xs ++ [0]))]

I searched on the internet and found some info for
printing out the list of lists using MapM_ 
(source: http://learnyouahaskell.com/input-and-output)
pascals n :: Int -> IO () will give us the rows 1..n of
Pascal's Triangle.

> pascals :: Int -> IO ()
> pascals n = mapM_ print (take n (iterate nextRow [1]))

 e) The number of different combinations of r elements drawn from
    from a set of n possible items is denoted as:

                              n!
                    nCr  = ---------
                           (n-r)! r!

 Define two different versions of a function,
 comb :: Integer -> Integer -> Integer, such that comb n r = nCr
 using:

 i)   The definition of nCr in terms of factorials given above;

> fact :: Integer -> Integer
> fact n = product [1..n]
> comb :: Integer -> Integer -> Integer
> comb n r = div ( fact n ) ( (fact (n-r)) * (fact r) )

 ii)  The observation that nCr is the rth value in the nth row of
      Pascal's triangle.

> comb_ :: Integer -> Integer -> Integer
> comb_ n r =  ((iterate nextRow[1]) !! fromIntegral(n)) !! fromIntegral(r)

> comb_again :: Int -> Int -> Integer
> comb_again n r =  ((iterate nextRow[1]) !! n) !! r

 Briefly compare these definitions, highlighting the most attractive
(or unattractive!) features in each case.

it seems that (ii) looks nicer in code, but you can't put really big
values into either (comb_ or comb_again) of them:
----------------------------
Main> comb_ 5458 3
27083862256
Main> comb_ 5459 3

ERROR - Control stack overflow
Main>
----------------------------
They both error like that above. When I ran comb it was able to
return a result with the same inputs

Just looking at the prompt, executing the functions, comb took
quite a bit longer to execute than comb_ or comb_again.

It looks like comb is NOT making 3 (possibly long lists) because I'm
saying product [1..n], but I'm guessing that haskell does something
a bit smarter like maybe computing product of (p1, p2), (where p_i is value
p at index i), storing the result and then saying 
"okay, whats the next thing that should be in that list?"
and then it makes the p3 and computes product (result, p3). Keeps 
doing this until it gets to pn. What I'm trying to say is that I
don't think it's making a long list. In (ii) I think Haskell has to
make the long lists with all the values and it runs out of memory.

comb probably has to do a lot of moving the result around, which 
(I'm guessing) is why it takes longer than comb_.

     
