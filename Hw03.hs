{- CS380 Assignment 3
   Name: Solomon Lake Giffen-Hunter
   College email: sgiffenh@haverford.edu
   Resources / collaborators:

   Note:
   I had 75/90 when I turned it in. I wish you could see the series of commits. I'm trying to get it done right, which
   means I will be resubmitting so it can autograde. Not sure how it being "marked late" affects the grade, but I 
   care more about learning more about the code then timeliness or grade.

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 15, 2017.**
-}


{-# LANGUAGE GADTSyntax, StandaloneDeriving #-}

module Hw03 where

import Arith
import Parser
import Data.Ratio

--------------------------------------------------------------------------------
-- The Halgebra computer algebra system
--

{-
   In this assignment, you will write functions that can be used to solve
   linear equations in one variable, x. The final result will be the `solve`
   function, at the end of this file. `solve`'s type is `Equation -> Rational`;
   it takes an `Equation` in the variable x and returns the `Rational` that
   is a solution to the equation. (`Rational`, exported in the `Prelude` but
   originally from `Data.Ratio`, is a numerical type that can store rational
   numbers of arbitrary precision. By "numerical type", I mean that `Rational`
   in an instance of the `Num` class, meaning that `3 :: Rational` is accepted.)

   This assignment is less prescribed than previous ones, with a few function
   signatures given to you, and the rest left for you to figure out.

   Here is the general approach you will take:

   1. Set one side of the input equation to 0. That is, create a `Sum` that
      evaluates to 0 whenever the original equation holds. (This step
      is really simple!)

   2. Simplify your `Sum` into a list of `SimpleTerm`s. Each `SimpleTerm`
      is a `Rational` coefficient perhaps multiplied by x. This step is done
      by writing the three functions `simpleSum`, `simpleTerm`, and
      `simpleFactor`, which will be mutually recursive. (That is, they
      will call one another.) You will likely need to write several helper
      functions.

   3. Separate out the list of `SimpleTerm`s into those that mention x and
      those that don't.

   4. Add together the coefficients of x and the `SimpleTerm`s that do not
      mention x. Call the former `x_coef` and the latter `constant`.

   5. The solution to the equation is `(-constant)/x_coef`.

   Here is an example:

   Start:  1 + 2*x = 3 * (x - 5*(x + 1))
   After step 1: (1 + 2*x) - (3 * (x - 5*(x + 1)))
   After step 2: [1, 2x, -3x, 15x, 15]
   After step 3: ([2x, -3x, 15x], [1, 15])
   After step 4: (14, 16)
   After step 5: -8/7

   This homework assignment requires the Arith.hs and Parser.hs files as
   posted on our syllabus page. It also requires the `parsec` package. You
   might need to

      cabal install parsec

   to install this on your system.

   Hints:
     * The `fromInteger :: Num a => Integer -> a` function can convert from
       `Integer` to any other numerical type (like `Rational`).

     * By default, `Rational`s print out somewhat oddly in GHCi, using a `%`
       to denote division. I've given you `prettyRational` that does a better
       job.

     * There are three ways solving can fail:
       1) You can have a non-linear equation, where you try to multiply x by x.
       2) You can have a non-linear equation, where you try to divide by x.
       3) All the x's can cancel out.
       In any of these scenarios, just call `error :: String -> a` with an
       appropriate error message. Do *not* try to detect (and accept) an equation
       with x/x in it, such that the division by x is OK. Any division by
       an expression with x in it is rejected.

       (This approach toward failure is regrettable. Really, we should return
        a `Maybe Rational`. But that will complicate things too much at this
        stage. We shall return!)

     * Simplifying (a + b + c) * (d + e + f) means you multiply everything
       in the left sum by everything in the right sum, producing *nine* output
       terms. A list comprehension might be useful.

     * Simplifying (a + b + c) / (d + e + f) is harder. Because we reject any
       denominator mentioning x, we can just add the d, e, and f (which must be
       x-less `SimpleTerm`s), and then divide each of a, b, and c by the sum.

     * Write unit tests! You will thank yourself later.
-}

-- a prettier rendering of Rationals
prettyRational :: Rational -> String
prettyRational r
  | denominator r == 1
  = show (numerator r)

  | otherwise
  = "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

-- a SimpleTerm is a coefficient and, perhaps, an x
data SimpleTerm where
  SimpleTerm :: Rational       -- the coefficient
             -> Bool           -- True <=> there is an x; False <=> there isn't
             -> SimpleTerm

-- You may wish to uncomment one of these instances for debugging / unit testing:


-- This Show instance is pretty
instance Show SimpleTerm where
  show (SimpleTerm coef has_x) = show_coef ++ maybe_show_x
    where
      show_coef = prettyRational coef

      maybe_show_x | has_x     = "x"
                   | otherwise = ""


{-
-- This Show instance is ugly
deriving instance Show SimpleTerm
-}

-- Sample Equations:
sum4 :: Sum
sum4 = (parseSum "4 + x")

sumMult :: Sum
sumMult = (parseSum "5 * x")

sumInt :: Sum
sumInt = (parseSum "4 + 4")

eQ :: Equation
eQ = (parseEquation "1 + 2*x = 3 * (x - 5*(x + 1))")

sumEQ :: Sum
sumEQ = gatherOnOneSide eQ
-- "1 + 2 * x - 3 * (x - 5 * (x + 1))"

simSum4 :: [SimpleTerm]
simSum4 = simpleSum sum4

test :: Equation
test = (parseEquation "1 = x - 1")
--1 = 1 / (1 - 1)

testSum :: Sum
testSum = gatherOnOneSide test



simEQ :: [SimpleTerm]
simEQ = simpleSum (gatherOnOneSide eQ)

-- Step 1
gatherOnOneSide :: Equation -> Sum
gatherOnOneSide (Equation lhs rhs) = (Minus lhs rhs)

-- Simplify a Sum to a list of SimpleTerms (Step 2)
simpleSum :: Sum -> [SimpleTerm]
simpleSum sum = case sum of
  --(Plus (Term (Factor (Lit a))) (Term (Factor (Lit b)))) -> [(SimpleTerm (iTOr(a+b)) False)]
  (Plus a b) -> stPlus (simpleSum a) (simpleSum b)
  (Minus a b) -> stMinus (simpleSum a) (simpleSum b)--simpleSum a ++ (minusSimpleSum (simpleSum b))--stMinus (simpleSum a) (simpleSum b)
  (Term a) -> simpleTerm a

-- NOTE: ACCOUNT FOR CASE WHERE ONLY ONE VAL IN FIRST LIST
-- helper Plus
stPlus :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
stPlus [] [] = []
stPlus [] s1 = s1
stPlus s [] = s
stPlus ((SimpleTerm r b):sx) ((SimpleTerm r1 b1):s1x) | b == b1   = [(SimpleTerm (r+r1) b)] ++ sx ++ s1x
                                                      | otherwise = [(SimpleTerm r b)] ++ stPlus sx ((SimpleTerm r1 b1):s1x)

-- NOTE: CASE WHERE ONLY ONE VAL IN SEC LIST? DO THESE NEED TO GO THROUGH ALL LIST?
-- helper Minus
stMinus :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
stMinus [] [] = []
stMinus [] ((SimpleTerm r1 b1):s1x) = [(SimpleTerm (r1*(-1)) b1)] ++ stMinus [] s1x
stMinus s [] = s
stMinus ((SimpleTerm r b):sx) ((SimpleTerm r1 b1):s1x) | b == b1   = [(SimpleTerm (r-r1) b)] ++ stMinus sx s1x
                                                       | otherwise = [(SimpleTerm r b)] ++ [(SimpleTerm (r1*(-1)) b1)] ++ stMinus sx s1x


minusSimpleSum :: [SimpleTerm] -> [SimpleTerm]
minusSimpleSum [] = []
minusSimpleSum s = map (multSS (SimpleTerm (-1) False)) s

minusSS :: SimpleTerm -> SimpleTerm
minusSS (SimpleTerm r b) = (SimpleTerm (r * (-1)) b)


-- Simplify a Term to a list of SimpleTerms (Step 2)
simpleTerm :: Term -> [SimpleTerm]
simpleTerm term = case term of
  (Mult a b) -> multSimpleSum (simpleTerm a) (simpleTerm b)
  (Div a b) -> divSimpleSum (simpleTerm a) (simpleTerm b)
  (Factor a) -> simpleFact a

divSimpleSum :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
divSimpleSum [] [] = []
divSimpleSum [] s1 = s1
divSimpleSum s [] = s
divSimpleSum [s] s1 = map (divSS s) s1
divSimpleSum (s:sx) (s1:s1x) = [divSS s s1] ++ divSimpleSum sx (s1:s1x) --map (divSS s) s1 ++ divSimpleSum sx s1

divSS :: SimpleTerm -> SimpleTerm -> SimpleTerm
divSS (SimpleTerm r b) (SimpleTerm r1 b1) | b == True = (SimpleTerm (r/r1) True)
                                           | b1 == True = error "x is bottom term in div"
                                           | otherwise = (SimpleTerm (r/r1) False)

-- helper Mult
multSimpleSum :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
multSimpleSum [] [] = []
multSimpleSum [] s1 = s1
multSimpleSum s [] = s
multSimpleSum [s] s1 = map (multSS s) s1
multSimpleSum (s:sx) s1 = map (multSS s) s1 ++ multSimpleSum sx s1

multSS :: SimpleTerm -> SimpleTerm -> SimpleTerm
multSS (SimpleTerm r b) (SimpleTerm r1 b1) | (b == b1 && b == True)    = error "x*x, non-linear"
                                           | (b == True || b1 == True) = (SimpleTerm (r*r1) True)
                                           | otherwise                 = (SimpleTerm (r*r1) False)


-- Simplify a Factor to a list of SimpleTerms (Step 2)
simpleFact :: Factor -> [SimpleTerm]
simpleFact fact = case fact of
  (Lit a) -> [(SimpleTerm (iTOr a) False)]
  (Var) -> [(SimpleTerm 1 True)]
  (Sum a) -> simpleSum a



iTOr :: Integer -> Rational
iTOr a = fromInteger a







-- Step 3
partitionTerms :: [SimpleTerm]
               -> ( [SimpleTerm]   -- these mention x
                  , [SimpleTerm] ) -- these don't
partitionTerms s = (sX, sNoX)
    where
    sX = filter hasX s
    sNoX = filter noX s
    
hasX :: SimpleTerm -> Bool
hasX (SimpleTerm r b) = b

noX :: SimpleTerm -> Bool
noX (SimpleTerm r b) = not b


-- changed
-- Step 4
sumPartitions :: ( [SimpleTerm]    -- these mention x
                 , [SimpleTerm] )  -- these don't
              -> ( Rational        -- sum of coefficients of x
                 , Rational )      -- sum of constants
sumPartitions (sX, sNoX) = (addRats (map stToRat sX), addRats (map stToRat sNoX))

stToRat :: SimpleTerm -> Rational
stToRat (SimpleTerm r b) = r

addRats :: [Rational] -> Rational
addRats [] = 0
addRats [r] = r
addRats (r:rx) = r + addRats rx

checkXCancel :: Rational -> Rational
checkXCancel r | r == 0    = error "x cancels out"
               | otherwise = r

-- changed
-- Step 5
extractSolution :: ( Rational     -- coefficient of x, "a"
                   , Rational )   -- constant, "b"
                -> Rational       -- solution to "a*x + b = 0"
extractSolution (rX, rNoX) = ((-1) * rNoX) / rX

-- Put them all together
solve :: Equation -> Rational
solve = extractSolution .
        sumPartitions .
        partitionTerms .
        simpleSum .
        gatherOnOneSide

solveEq :: Equation -> [SimpleTerm]
solveEq = simpleSum .
          gatherOnOneSide
