{- CS380 Assignment 2
   Name:
   College email:
   Resources / collaborators:

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 8, 2017.**
-}

--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE GADTSyntax #-}

module Hw02 where

import Test.HUnit

--------------------------------------------------------------------------------
-- Binary Search Trees

{- In this assignment, you will be writing a binary search tree implementation
   of a finite map. A finite map is a data structure that associates *keys*
   (of one type) with *values* (of, potentially, another type). This is useful for,
   say, storing an address book or counting word frequencies in a file. Java's
   finite map interface is Map (https://docs.oracle.com/javase/8/docs/api/java/util/Map.html);
   Python calls this type a dictionary.

   Note that this is a *different*, unrelated use of the word "map" than the
   higher-order function that we've seen.

   To keep things simple, our map will use Strings as its *key* type.

   The map will be implemented using a binary search tree.
   https://en.wikipedia.org/wiki/Binary_search_tree

   The tree type will store a String and a value (of type `a`) at each interior
   node. It will obey the usual binary search tree properties. Given a interior
   node T with left child L and right child R:

   1. The key stored in every node in the tree rooted at L is less than T's key.
   2. The key stored in every node in the tree rooted at R is greater than T's key.

   By "less" and "greater" here, I mean by using the operators (<) and (>), which
   work on Strings.

   **** UNIT TESTS ****
   Each function must be tested against at least 4 test cases. Add to mine!
-}

-- The tree datatype:
data Tree a where
  Leaf :: Tree a
  Node :: String  -- key
       -> a       -- value
       -> Tree a  -- left child
       -> Tree a  -- right child
       -> Tree a
    deriving (Eq, Show)   -- this allows (==) on trees and (show :: Tree a -> String)

sampleTree1 :: Tree Int
sampleTree1 = Node "pickle" 5
       (Node "avocado" 2
     Leaf           (Node "clementine" 10
                      Leaf    Leaf))           (Node "tomato" 7
                                        (Node "radish" 9
                                          Leaf   Leaf)         (Node "yam" 1
                                                                 Leaf   Leaf))

sampleTree2 :: Tree Char
sampleTree2 = Node "Haskell" 'x'
          (Node "C" 'q'
   (Node "Ada" 'p'
   Leaf   Leaf)    (Node "C++" 'r'
                     Leaf (Node "F#" 'e'
                            Leaf   Leaf))) (Node "OCaml" 'd'
                                                Leaf     Leaf)

smallTree :: Tree Int
smallTree = Node "D" 1
           Leaf Leaf

leftNodeTree :: Tree Int
leftNodeTree = Node "D" 1
           (Node "C" 2 Leaf Leaf) Leaf


rightNodeTree :: Tree Int
rightNodeTree = Node "D" 1
            Leaf (Node "E" 2 Leaf Leaf)
--------------------------------------------------------------------------------
-- Problem 1 

{- Write a function that gets the size (number of interior nodes) of a tree. -}

-- Used to help with non-exhaustive patter in function error:
--http://stackoverflow.com/questions/2737650/better-exception-for-non-exhaustive-patterns-in-case

sizeTree :: Tree a -> Int
sizeTree tree = case tree of
    Leaf -> 0
    (Node s a l r) -> 1 + sizeTree l + sizeTree r

sizeTree_tests = "sizeTree" ~:
                 TestList [ "sampleTree1" ~: sizeTree sampleTree1 ~?= 6
                          , "sampleTree2" ~: sizeTree sampleTree2 ~?= 6
                          , "smallTree"   ~: sizeTree smallTree   ~?= 1
                          , "leftNodeTree"~: sizeTree leftNodeTree~?= 2]

--------------------------------------------------------------------------------
-- Problem 2

{- Write a function that finds a key in a binary search tree and
   returns the associated value, if there is one. -}

-- Used to help with case and guard combo:
-- https://wiki.haskell.org/Case
   
findTree :: Tree a -> String -> Maybe a
findTree tree str = case tree of
    Leaf -> Nothing
    (Node s a l r) | s == str  -> Just a
                   | s > str   -> findTree l str
                   | otherwise -> findTree r str
  
findTree_tests = "findTree" ~:
                 TestList [ "pickle" ~: findTree sampleTree1 "pickle" ~?= Just 5
                          , "Java"   ~: findTree sampleTree2 "Java"   ~?= Nothing
                          , "E"      ~: findTree rightNodeTree "E"    ~?= Just 2
                          , "What?"  ~: findTree smallTree "What?"    ~?= Nothing]
  -- Add more tests

--------------------------------------------------------------------------------
-- Problem 3

{- Write a function that inserts a key into a binary search tree. If the key
   is already in the tree, then update the value associated with the key. -}

insertTree :: Tree a -> String -> a -> Tree a
insertTree tree str val = case tree of
    Leaf -> Node str val Leaf Leaf
    (Node s a l r) | s == str -> Node str val l r 
                   | s > str  -> Node s a (insertTree l str val) r 
                   | otherwise-> Node s a l (insertTree r str val)

insertTree_tests
  = "insertTree" ~:
    TestList [ "insert/find" ~: findTree (insertTree Leaf "hi" "there") "hi" ~?= Just "there"
             , "update"      ~: findTree (insertTree sampleTree1 "clementine" (-5)) "clementine"
                                  ~?= Just (-5)
             , "insert/find left" ~: findTree (insertTree leftNodeTree "A" 5) "A" ~?= Just 5
             , "insert/display" ~: insertTree smallTree "X" 10 ~?= Node "D" 1 Leaf 
                    (Node "X" 10 Leaf Leaf)]
  -- Add more tests

--------------------------------------------------------------------------------
-- Problem 4

{- Write a function that maps a function over all the *values* in your tree. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f tree = case tree of
    Leaf -> Leaf
    (Node s a l r) -> Node s (f a) (mapTree f l) (mapTree f r)

mapTree_tests
  = "mapTree" ~:
    TestList [ "isVowel" ~: findTree (mapTree (`elem` "aeiou") sampleTree2) "F#" ~?= Just True
             , "samesize" ~: sizeTree (mapTree (+1) sampleTree1) ~?= 6
             , "plusOne show" ~: mapTree (+1) smallTree ~?= Node "D" 2 Leaf Leaf
             , "Leaf tree" ~: mapTree (+1) Leaf ~?= Leaf]
  -- Add more tests

--------------------------------------------------------------------------------
-- Problem 5

{- Write a function that returns all the key/value pairs of a tree in preorder.
   That is, return the key/value pair of a node before recurring into its children. -}

preorder :: Tree a -> [(String, a)]
preorder tree = case tree of
    Leaf -> []
    (Node s a l r) -> [(s,a)] ++ (preorder l) ++ (preorder r)
    

preorder_tests
  = "preorder" ~:
    TestList [ "sampleTree1" ~: preorder sampleTree1 ~?= [ ("pickle", 5), ("avocado",2)
                                                         , ("clementine", 10), ("tomato", 7)
                                                         , ("radish", 9), ("yam", 1) ]
             , "empty" ~: preorder (Leaf :: Tree Integer) ~?= []
             , "sampleTree2" ~: preorder sampleTree2 ~?= [ ("Haskell",'x'), ("C",'q') 
                                                         , ("Ada",'p'), ("C++",'r')
                                                         , ("F#", 'e'), ("OCaml",'d')]
             , "leftNodeTree" ~: preorder leftNodeTree ~?= [ ("D",1), ("C",2) ]]
                                    -- HUnit struggles if it doesn't know the type
                                    -- of data stored in a polymophic structure
  -- Add more tests

--------------------------------------------------------------------------------
-- Problem 6

{- Write a function that uses your tree structure to efficiently compute the
   frequencies of words in a list of words. The input to your function is a list
   of words (Strings). The output is an association list associating each word
   with the number of times it occurs in the list. -}

frequencies :: [String] -> [(String, Int)]
frequencies strL = preorder (createTree (Leaf :: Tree Int) strL)

createTree :: Tree Int -> [String] -> Tree Int
createTree tree [] = tree
createTree tree [s] = case findTree tree s of
    Nothing -> insertTree tree s 1
    Just v  -> insertTree tree s (v+1)
createTree tree (s:ss) = case findTree tree s of
    Nothing -> createTree (insertTree tree s 1) ss
    Just v -> createTree(insertTree tree s (v+1)) ss
    {-
        | findTree tree s == Nothing = createTree((insertTree tree s 1) ss)
        | otherwise = createTree((mapTree (+1) tree) ss) -}

frequencies_test
  = "frequencies" ~:
    TestList [ "palindrome a   " ~: lookup "a"    (frequencies words) ~?= Just 3
             , "palindrome plan" ~: lookup "plan" (frequencies words) ~?= Just 1
             , "empty" ~: frequencies []                              ~?= []
             , "small sample" ~: frequencies ["a","a","b","a"]        ~?= [("a",3),("b",1)]]
  where
    words = ["a", "man", "a", "plan", "a", "canal", "Panama"]

