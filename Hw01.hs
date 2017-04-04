{- CS380 Assignment 1
   Name:
   College email:
   Resources / collaborators:

   **DUE BEFORE CLASS ON MONDAY, JANUARY 30, 2017.**
   See forthcoming email for submission instructions.

   (This assignment is directly inspired by an assignment in CIS120 at UPenn.)
-}

module Hw01 where

import Test.HUnit

{- NOTE: you should _not_ use functions in the Haskell standard libraries,
   especially the ones in the Data.List module, except where they are
   explicitly allowed in the comments. The purpose of this assignment is to
   familiarize you with the basics of Haskell programming, so we want you to
   explicitly write out each of these problems even though there is often a
   built-in function that would achieve the same result. You will not receive
   credit for solutions that are contrary to the spirit of the assignment.

   You MAY use helper functions / other top-level definitions as you wish. -}

--------------------------------------------------------------------------------
-- Problem 1 (counting coins)

{- Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to the given
   amount. For example, to make 7 cents, a total of 3 coins are
   needed (two pennies and a nickel); to make 99 cents, 14 coins are
   needed (9 dimes, 1 nickel, and 4 pennies). Fill in the body of the
   function 'coins' below so that it returns the right answer. Prefer
   guards over `if`/`then`/`else`. -}

coins :: Int -> Int
coins n | n < 1 = 0
        | n == 1 = 1
        | n == 5 = 1
        | n == 10 = 1
        | n > 10 = 1 + coins (n - 10)
        | n > 5 = 1 + coins (n - 5)
        | otherwise = 1 + coins (n - 1)

-- I provide two test cases. You must provide two more.
-- See https://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit-Base.html
-- for the functions to create tests. (For now, treat Assertion and Test as
-- interchangeable.)

coins_tests = "coins" ~:
              TestList [ "coins 7"  ~: coins 7  ~?= 3
                       , "coins 99" ~: coins 99 ~?= 14
                       , "coins 1005" ~: coins 1005 ~?= 101
                       , "coins 1503" ~: coins 1503 ~?= 153 ]

--------------------------------------------------------------------------------
-- Problem 2 (geometry)

{- Sometimes street magicians need to use crates as tables in their
   acts.  Given the dimensions of a crate, find the largest surface
   area it can provide when used as a table.

   Hint: Haskell provides built-in max and min functions that take in two
   arguments and behave exactly as you might expect: `max 5 2` returns 5,
   for example.

   Note: The behavior of this function when at least one of the input side
   lengths is <= 0 is undefined. Your function may return any value in this
   case; we will not test this case on submission. -}


myMax :: Int -> Int -> Int
myMax x y | x > y = x
          |otherwise = y
   
maximumTableArea :: Int -> Int -> Int -> Int
maximumTableArea side1 side2 side3
  | side1 > side2 = side1 * myMax side2 side3
  | otherwise = side2 * myMax side1 side3

maximumTableArea_tests = "maximumTableArea" ~:
                         TestList [ "mta 1 2 3" ~: maximumTableArea 1 2 3 ~?= 6
                                  , "mta 4 3 3" ~: maximumTableArea 4 3 3 ~?= 12
                                  , "mta 6 1 1" ~: maximumTableArea 6 1 1 ~?= 6
                                  , "mta 16 16 1" ~: maximumTableArea 16 16 1 ~?= 256]

--------------------------------------------------------------------------------
-- Problem 3 (simulating robot movement)

{- Help a robot move along its track (with spaces numbered 0 through
   99) by calculating its new position when given `dir` (equal to
   "forward" or "backward") and `num_moves` indicating a non-negative
   number of spaces.  Keep in mind that the robot can't move past the
   0 or 99 spot so when it reaches either end it stays there. -}

moveRobot :: Int -> String -> Int -> Int
moveRobot cur_pos dir num_moves
  | dir == "forward" && cur_pos + num_moves <= 99 = cur_pos + num_moves
  | dir == "forward" = 99
  | dir == "backward" && cur_pos - num_moves >= 0 = cur_pos - num_moves
  | otherwise = 0

moveRobot_tests = "moveRobot" ~:
                  TestList [ "10 forward 3" ~: moveRobot 10 "forward" 3 ~?= 13
                           , "1 backward 4" ~: moveRobot 1 "backward" 4 ~?= 0
                           , "89 forward 20" ~: moveRobot 89 "forward" 20 ~?= 99
                           , "14 backward 3" ~: moveRobot 14 "backward" 3 ~?= 11 ]

--------------------------------------------------------------------------------
-- Problem 4 (Philadelphia geography)

{- Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.

   Even streets go one way and odd streets go another:

     East of Broad (<14th): even go south, odd go north
     West of Broad (>14th): even go north, odd go south
     West Philly  (>=32nd): even go south, odd go north
     West Philly  (>=46th): two-way

   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.

   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider Front
       (=1st) through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
     - you might find the infix 'mod' (modulo) function useful:
           (x mod 2)
       evaluates to 0 if x is even and 1 otherwise.
     - sometimes there's no 'simple' way of writing down complex case
       analysis... -}

streetDirection :: Int -> String
streetDirection n
  | n < 1 || n > 69 = "N/A"
  | n == 58 = "N"
  | n == 24 || n == 59 = "S"
  | n == 14 || n == 25 || n == 38 || n == 41 || n == 42 = "NS"
  | n == 14 || n == 25 || n == 38 || n == 41 || n == 42 = "NS"
  | (n < 14 || n >= 32) && n < 46 && n `mod` 2 == 1 = "N"
  | n < 14 = "S"
  | n > 14 && n < 32 && n `mod` 2 == 1 = "S"
  | n > 14 && n < 32 = "N"
  | n >= 46 = "NS"
  | otherwise = "N/A"

streetDirection_tests = "streetDirection" ~:
                        TestList [ "14" ~: streetDirection 14 ~?= "NS"
                                 , "9"  ~: streetDirection 9  ~?= "N"
                                 , "18" ~: streetDirection 18 ~?= "N"
                                 , "123" ~: streetDirection 123 ~?= "N/A"
                                 , "49" ~: streetDirection 49 ~?= "NS" ]

--------------------------------------------------------------------------------
-- Problem 5 (exists)

{- Write a function that determines whether at least one boolean value
   in its input list is true. -}

exists :: [Bool] -> Bool
exists [] = False
exists (l:ls) = l || exists ls 

exists_tests = "exists" ~:
               TestList [ "FF"  ~: exists [False, False]       ~?= False
                        , "FTF" ~: exists [False, True, False] ~?= True
                        , "FFFFF" ~: exists [False, False, False, False, False]       ~?= False
                        , "TFTT" ~: exists [True, False, True, True] ~?= True ]

--------------------------------------------------------------------------------
-- Problem 6 (join)

{- Write a function that takes a list of strings and "flattens" it
   into a single string. This function also takes an additional
   argument, a separator string, which is interspersed between all of
   the strings in the list. -}

join :: String -> [String] -> String
join s [] = ""
join s [l] = l
join s (l:ls) = l ++ s ++ (join s ls)

join_tests = "join" ~:
             TestList [ ", abc" ~: join "," ["a", "b", "c"] ~?= "a,b,c"
                      , "abc"   ~: join ""  ["a", "b", "c"] ~?= "abc"
                      , "empty" ~: join "," []              ~?= ""
                      , "+ wzx" ~: join "+" ["w", "z", "x"] ~?= "w+z+x"
                      , "space hiLake" ~: join " " ["hi", "Lake"] ~?= "hi Lake" ]

--------------------------------------------------------------------------------
-- Problem 7 (finding dolls in a toy store)

{- Write a function that checks whether a list of toys contains some
   particular toy. -}

containsStr :: [String] -> String -> Bool
containsStr [] s = False
containsStr (x:xs) s | s == x = True
                  | otherwise = containsStr xs s

containsStr_tests
  = "containsStr" ~:
    TestList [ "barbie" ~:
               containsStr ["truck", "barbie", "top"] "barbie" ~?= True
             , "woody" ~:
               containsStr ["truck", "barbie", "top"] "woody"  ~?= False
             , "hello" ~:
               containsStr ["hello"] "hello" ~?= True
             , "empty" ~: containsStr [] "wha?" ~?= False ]

{- Next, write a function that, given a list of toys and a list of
   dolls, filters the toys list so that only dolls remain. Your
   function should return a list containing all the elements of a
   given list of toy names that appear in a given list of doll
   names. -}

dollsOf :: [String]  -- all toys
        -> [String]  -- dolls
        -> [String]  -- the toys that are dolls
dollsOf [] dolls = []
dollsOf (toy:toys) dolls | containsStr dolls toy = [toy] ++ dollsOf toys dolls
                         | otherwise = dollsOf toys dolls

dollsOf_tests
  = "dollsOf" ~:
    TestList [ "barbie" ~:
               dollsOf ["truck", "barbie", "top"] ["barbie", "woody"]
                 ~?= ["barbie"]
             , "none" ~:
               dollsOf [] ["barbie", "woody"] ~?= []
             , "barbie, adam" ~: 
               dollsOf ["hello", "", "adam"] ["barbie", "adam"] ~?= ["adam"]
             , "none1" ~: dollsOf ["what?"] ["doll"] ~?= [] ]

--------------------------------------------------------------------------------
-- Problem 8 (merging lists)

{- Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order:
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list.

   The lengths of the two lists may not be the same -- any
   extra elements should appear at the very end of the result. -}

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge [] (l:ls) = [l] ++ merge [] ls
merge (x:xs) [] = [x] ++ merge xs []
merge (x:xs) (l:ls) = [x] ++ [l] ++ merge xs ls

merge_tests = "merge" ~:
              TestList [ "1 through 8" ~: merge [1,3,5,7] [2,4,6,8] ~?= [1..8]
                       , "empty list"  ~: merge [1,2,3]   []        ~?= [1,2,3]
                       , "2 lists of different size" ~: merge [1] [4,5,6] ~?= [1,4,5,6]
                       , "empty 1" ~: merge [] [1] ~?= [1] ]

--------------------------------------------------------------------------------
-- Problem 9 (is_sorted)

{- Write a function that determines whether a given list of integers
   is SORTED -- that is, whether the elements appear in ascending
   order. It is okay if the list has repeated elements, so long as they
   are next to each other.

   For the purposes of this function, we consider lists containing zero
   or one elements to be sorted. -}

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) | x <= head xs = True && isSorted xs
                | otherwise = False

isSorted_tests = "isSorted" ~:
                 TestList [ "123" ~: isSorted [1,2,3] ~?= True
                          , "321" ~: isSorted [3,2,1] ~?= False
                          , "52345" ~: isSorted [5,2,3,4,5] ~?= False
                          , "empty" ~: isSorted [] ~?= True ]

--------------------------------------------------------------------------------
-- Problem 10 (merge_sorted)

{- Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. -}

mergeSorted :: [Int] -> [Int] -> [Int]
mergeSorted [] [] = []
mergeSorted (x:xs) [] = [x] ++ mergeSorted xs []
mergeSorted [] (l:ls) = [l] ++ mergeSorted [] ls
mergeSorted (x:xs) (l:ls) | x < l = [x] ++ mergeSorted xs (l:ls)
                          | otherwise = [l] ++ mergeSorted (x:xs) ls

mergeSorted_tests
  = "mergeSorted" ~:
    TestList [ "primes"     ~: mergeSorted [2,7] [3,5,11] ~?= [2,3,5,7,11]
             , "sequential" ~: mergeSorted [1,2,3] [4,5,6] ~?= [1,2,3,4,5,6]
             , "first empty" ~: mergeSorted [] [1,2] ~?= [1,2]
             , "both empty" ~: mergeSorted [] [] ~?= [] ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- NOTE:
--   From here on out (other than the challenge problem), **NO RECURSION** is
--   allowed. Instead, use the library functions `map`, `filter`, and `zipWith`.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Problem 11 (evens)

{- Write a function that takes a list of integers and returns a list containing
   only the even numbers from the input list. -}
   
isEven :: Int -> Bool
isEven n | n `mod` 2 == 0 = True
         | otherwise = False

evensOnly :: [Int] -> [Int]
evensOnly = filter isEven

evensOnly_tests = "evensOnly" ~:
                  TestList [ "12345" ~: evensOnly [1,2,3,4,5] ~?= [2,4]
                           , "2468"  ~: evensOnly [2,4,6,8]   ~?= [2,4,6,8]
                           , "" ~: evensOnly [] ~?= []
                           , "11111" ~: evensOnly [1,1,1,1,1] ~?= [] ]

--------------------------------------------------------------------------------
-- Problem 12 (squares)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the numbers in the input list. -}

square :: Int -> Int
square n = n * n 
   
squares :: [Int] -> [Int]
squares = map square

squares_tests = "squares" ~:
                TestList [ "123"  ~: squares [1,2,3]    ~?= [1,4,9]
                         , "negs" ~: squares [-1,-2,-3] ~?= [1,4,9]
                         , "" ~: squares [] ~=? []
                         , "12" ~: squares [12] ~=? [144] ]

--------------------------------------------------------------------------------
-- Problem 13 (wurble)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the negative integers in the input list, as long as that square's last digit is a 6. 
   Used: http://stackoverflow.com/questions/17144997/gets-last-digit-of-a-number
   to help me quickly remember how to get last digit of number
   
   -}

isNeg :: Int -> Bool
isNeg n | n < 0 = True
        | otherwise = False

ends6 :: Int -> Bool
ends6 n | n `mod` 10 == 6 = True
        | otherwise = False
   
wurble :: [Int] -> [Int]
wurble l = filter ends6 (map square (filter isNeg l))

wurble_tests = "wurble" ~:
               TestList [ "negs" ~: wurble [-1,-2,-3,-4,-5] ~?= [16]
                        , "neg6" ~: wurble [1,2,3,4,5,-6]   ~?= [36]
                        , "empty" ~: wurble [] ~?= []
                        , "only pos" ~: wurble [2,4,14] ~?= [] ]

--------------------------------------------------------------------------------
-- Problem 14 (sums)

{- Write a function that takes two lists of integers and returns a list
   containing the sums of corresponding integers. If one list is longer than
   the other, simply ignore the extra elements. -}

myAdd :: Int -> Int -> Int
myAdd x y = x + y
   
sums :: [Int] -> [Int] -> [Int]
sums xs ls = zipWith myAdd xs ls

sums_tests = "sums" ~:
             TestList [ "123,456" ~: sums [1,2,3] [4,5,6] ~?= [5,7,9]
                      , "1234,00" ~: sums [1,2,3,4] [0,0] ~?= [1,2]
                      , " 12" ~: sums [] [1,2] ~?= []
                      , "123111 321" ~: sums [1,2,3,1,1,1] [3,2,1] ~?= [4,4,4] ]

--------------------------------------------------------------------------------
-- Problem 15 (permutations)

-- This one is a challenge problem, so it's worth 0 points -- kudos only.
-- You *MAY* use recursion here.

{- A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list). For example,

       permutations [1,2,3]

   might yield

       [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]].

   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result

       [[1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1], [1,2,3]]

   would also be correct.)

   Hint: Begin by writing a unit test or two, to make sure you understand the
   problem (even though you may need to rewrite them if your answer comes out
   in a different order, the exercise is useful). Also, you'll probably want
   to break the problem down into one or more sub-problems, each of which can
   be solved by recursion.

   Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if 'permutations' is
   missing. -}

permutations :: [a] -> [[a]]
permutations = error "permutations: unimplemented"

{- Note that you will also have to think about how to TEST
   permutations, as there may be several correct solutions for each
   input. -}


permutations_tests
  = "permutations" ~:
    TestList [ "your test" ~: assertFailure "unwritten test"
             , "your test" ~: assertFailure "unwritten test" ]

--------------------------------------------------------------------------------
-- All the tests, for a quick overview.
-- You may remove the permutations tests if you don't want them here.

all_tests = TestList [ coins_tests
                     , maximumTableArea_tests
                     , moveRobot_tests
                     , streetDirection_tests
                     , exists_tests
                     , join_tests
                     , containsStr_tests
                     , dollsOf_tests
                     , merge_tests
                     , isSorted_tests
                     , mergeSorted_tests
                     , evensOnly_tests
                     , squares_tests
                     , wurble_tests
                     , sums_tests
                     , permutations_tests ]

--------------------------------------------------------------------------------
{- Now that you've finished the assignment, please answer the following
   questions:

1. How did this assignment go for you?
It went well! Felt like a good level of challenge.
I guess maybe you can't see past submissions, but I kind of wish you could, becuase the autograder didn't compute till this morning and it finally
told me there was a recursion error. I went back and looked through the code and added the first clause in Problem 1, that accounted for negative numbers.
This fixed the problem and the autograder now works. Hope the final submission being after due date doesn't affect the grade.
I'm resubmitting a final time to add this comment.

2. What questions do you have?
No questions currently. I couldn't figure out the permutation, and I'm curious about that.

3. How long did this assignment take?
About 2.5 hours. But I've used Haskell before :)

-}

