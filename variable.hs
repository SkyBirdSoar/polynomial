{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polynomial.Variable
( Base -- One letter char, eg 'x', 'y'
, Power -- Integer, eg 1, 2
, Variable (Variable) -- Base -> Power -> Variable
, x -- Helper to construct a 'x' variable quickly
, x' -- ^  (Power -> Variable)
, y -- ^
, y' -- ^  (Power -> Variable)
, var -- ^
, var_ -- ^ (Power -> Variable)
, sameBase -- Check if two variables are of the same base (Variable -> Variable -> Bool)
, samePower -- Check if two variables are of the same power (Variable -> Variable -> Bool)
, condenseVariables -- Merge like variables together in a list aka simplify ([Variable] -> [Variable])
, showVariables -- Turns a list of variables into a String nicely in alphabetical order (does not condense first) ([Variable] -> String)
) where

import Data.List

-- Example: Algebra Unknowns
--          x, y, a, b, n
type Base = Char

-- Power: x^n, y^n
type Power = Integer

-- Note: Variables can only have 1 unknown
-- eg 'x', 'y', 'z', etc 
-- but not "xy", "ab", etc
data Variable = Variable Base Power

--------------------------------
-- Helper functions
--------------------------------

-- Converts a list of variables into a String
-- Variables are sorted base on Polynomial.Variable.compare
showVariables :: [Variable] -> String
showVariables = showVar . sort
                where
                showVar :: [Variable] -> String
                showVar xs = concat $ map show xs

-- Short hand for `Variable 'x' n`
-- read as x^n (replace ' with ^ mentally)
-- Eg x' 2 #=> x^2
x' :: Power -> Variable
x' = Variable 'x'

-- Short hand for `Variable 'x' 1` to create x
-- Eg x #=> x
x :: Variable
x = x' 1

-- Same as x' but for y
y' :: Power -> Variable
y' = Variable 'y'

-- Same as x but for y
y :: Variable
y = y' 1

-- Shorthand for `Variable a 1`
-- Eg var 'a' #=> a
var :: Base -> Variable
var a = Variable a 1

-- Shorthand for `Variable a n`
-- Eg var_ 'a' 2 #=> a^2
var_ :: Base -> Power -> Variable
var_ = Variable

-- Check if two variables have same base
-- Eg x `sameBase` x' 2     == True
--    x `sameBase` y        == False
--    x `sameBase` x' (-44) == True
--    var 'xy' `sameBase` x == ERROR: Variables can only have one unknown (or char).
sameBase :: Variable -> Variable -> Bool
sameBase (Variable a _) (Variable a2 _) = a == a2

-- Check if two variables have same power
-- Eg x `samePower` x' 2    == False
--    x `samePower` y       == True
--    x' 2 `samePower` y' 2 == True
samePower :: Variable -> Variable -> Bool
samePower (Variable _ b) (Variable _ b2) = b == b2

-- Condense a list of variables into a shorter list of variables aka simplify
-- Treats the list as multiplication
-- i.e x : y : [] == x * y * []
-- Merges repeated variables
--     Eg condenseVariables [x, x' 2] #=> [x^3] -- x^1 * x^2 = x^(1+2) = x^3
--        condenseVariables [x, x' (-100)] #=> [x^-99]
--        condenseVariables [x, y] #=> [x, y]
--        condenseVariables [x, y, x, y, x, y] #=> [x^3, y^3]
condenseVariables :: [Variable] -> [Variable]
condenseVariables = mergeLikeVariables . (groupBy sameBase) . sort
                    where
                    mergeLikeVariables :: [[Variable]] -> [Variable]
                    mergeLikeVariables [] = []
                    mergeLikeVariables (c:cs) = (foldr1 (\(Variable a b) (Variable _ b2) -> Variable a (b + b2)) c) : mergeLikeVariables cs


instance Eq Variable where
    (Variable a b) == (Variable a2 b2) =   a == a2   &&   b == b2

instance Show Variable where
    -- show (Variable b 0) = [b] ++ "^0"  -- Important as it explicitly tells people ^0 is shown
    show (Variable b 1) = [b]
    show (Variable b p) = [b] ++ "^" ++ show p

instance Ord Variable where
    c@(Variable a b) < d@(Variable a2 b2)
        | sameBase c d = b < b2   -- Technically, this may not be true if x is negative and power is odd
        | otherwise = a < a2 -- Technically, this may not be true if x = 4 and y = 2

    a <= b =   a < b   ||   a == b
    a > b = not $ a <= b
    a >= b = not $ a < b

    Variable a b `compare` Variable a2 b2
        | a == a2 = b `compare` b2
        | otherwise = a `compare` a2