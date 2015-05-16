{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polynomial.Term
( module Polynomial.Variable
, Term -- Coefficient * Variable = Term
, Substitutable (..) -- Term is an instance of Substitutable
, term -- Smart constructor for Term (Coefficient -> [Variable] -> Term)
, mterm -- Smart constructor for Term (Coefficient -> Variable -> Term)
, (><) -- alias for mterm
, orderTerms -- Sorts terms in order: alphabetical, ascending powers, coefficients ([Term] -> [Term])
, comparableTerms -- Checks if 2 terms have the same variables (Exactly the same!) (Term -> Term -> Bool)
, likeTerms -- alias for comparableTerms
, condenseTerms -- adds 2 like terms together, ignores the rest. aka simplify ([Term] -> [Term])
, safeCompare -- safely compare two terms. Returns Left String if not comparable. (Term -> Term -> Either String Ordering)
, showVariables1 -- edited version of showVariables in Polynomial.Variable. Used by Term. Nothing much changed. ([Variable] -> String)
, tMultN -- Multiply a Term by a Number (duplicate functionality as *) (Term -> Coefficient -> Term)
, tDivT -- Divide a Term by another Term (Term -> Term -> Term)
, sub -- Helper function for Substitutable into
, (|=|) -- Helper function for Substitutable into
) where

import Polynomial.Variable
import Data.List

-- The number of the variable...
-- i.e the 3 in 3x
--         4 in 4axy^2
--         3.92 in 3.92t
type Coefficient = Double

-- Coefficient * Variable = Term!
-- eg of terms: 4xy
--              4   -- (4x^0)
--              0   -- (0x^n)
--              y   -- (1y)
data Term = Term Coefficient [Variable]

-------------------------------------------------------
-- Smart constructors
-------------------------------------------------------

-- Condenses the variables first before creating a Term
-- Eg term 1 [x, x, x] #=> x^3 (Term 1 [Variable 'x' 3])
term :: Coefficient -> [Variable] -> Term
term c v = Term c $ condenseVariables v

-- Short hand for creating one unknowns as terms
-- mterm - monoterm
-- Eg mterm 1 x #=> x :: Term (Term 1 [Variable 'x' 1])
--    mterm 1 (var 'a') #=> a :: Term (Term 1 [Variable 'a' 1])
mterm :: Coefficient -> Variable -> Term
mterm c v = Term c [v]

-- Infix shorthand for mterm
(><) :: Coefficient -> Variable -> Term
(><) = mterm

-------------------------------------------------------
-- Helpers
-------------------------------------------------------

-- Takes a list of terms and then sorts it by alphabetical order and increasing powers as well as coefficient
-- Eg orderTerms [mterm 1 y, mterm 1 x] #=> [x, y] ([Term 1 [x], Term 1 [y]])
--    orderTerms [mterm 1 (y' 3), mterm 1 (var 'a'), mterm 2 y] #=> [a, 2y, y^3]
orderTerms :: [Term] -> [Term]
orderTerms xs = sortBy orderT xs
                where
                orderT :: Term -> Term -> Ordering
                orderT (Term c v) (Term c2 v2) = case v `compare` v2 of
                                                      LT -> if signum c2 == -1 -- v2 is negative, hence v2 is smaller
                                                            then GT
                                                            else LT
                                                      EQ -> c `compare` c2
                                                      GT -> if signum c == -1
                                                            then LT
                                                            else GT

-- Checks to see if two terms are comparable
-- i.e, they are made up of the same variables (coefficient doesn't matter) (power matters!)
-- this is because + and - can't work on different variables
-- Eg comparableTerms (mterm 1 x) (mterm 1 y) #=> False
--    comparableTerms (mterm 1 (x' 2)) (mterm 1 x) #=> False
--    comparableTerms (mterm 1 x) (mterm 43 x) #=> True
comparableTerms :: Term -> Term -> Bool
comparableTerms (Term _ v) (Term _ v2) = sort v == sort v2

-- Alias for comparableTerms
likeTerms :: Term -> Term -> Bool
likeTerms = comparableTerms

-- Condense a list of terms into a shorter list of terms aka simplify
-- Treats the list as addition
-- i.e x : y : [] == x + y + []
-- Merges repeated terms
--     Eg condenseTerms [mterm 1 x, mterm 1 x' 2] #=> [x, x^2] -- x^1 * x^2 = x^(1+2) = x^3
--        condenseTerms [mterm 3 x, mterm 4 x] #=> [7x]
condenseTerms :: [Term] -> [Term]
condenseTerms = mergeLikeTerms . (groupBy likeTerms) . orderTerms
                where
                mergeLikeTerms :: [[Term]] -> [Term]
                mergeLikeTerms [] = []
                mergeLikeTerms (a:as) = (foldr1 (+) a) : mergeLikeTerms as

-- Multiply a Term by a Coefficient
-- * also works
-- Can also divide using coefficients lesser than one, or their reciprocals.
-- Eg divide by 5 = multiply by 1/5
tMultN :: Term -> Coefficient -> Term
Term c v `tMultN` n = term (c*n) v

-- Divide a term by a term
-- Eg mterm 3 x `tDivT` mterm 1 x #=> 3 :: Term
--    mterm 3 x `tDivT` mterm 1 y #=> 3x(y^-1)
tDivT :: Term -> Term -> Term
Term c v `tDivT` Term c2 v2 = Term (c/c2) (condenseVariables $ v ++ (map (\(Variable a b) -> Variable a (-b)) v2))

-- Safely compare 2 terms
-- Left String -- Error message
-- Right Ordering
-- Eg mterm 1 x `compare` mterm 1 y #=> Left String
--    mterm 2 x `compare` mterm 2 (x' 2) #=> Left String
--    mterm 3 x `compare` mterm 32 x #=> Right Ordering
safeCompare :: Term -> Term -> Either String Ordering
a@(Term _ v) `safeCompare` b@(Term _ v2)
    | comparableTerms a b = Right $ a `compare` b
    | otherwise           = Left $ "Unlike terms: " ++ showVariables v ++ showVariables v2

-- Edited form of showVariables
-- Hides ^0
-- Wraps powers in brackets except the last
-- Eg showVariables1 [x' 0] #=> "" :: String
--    showVariables1 [x]    #=> "x" :: String
--    showVariables1 [x' 2] #=> "x^2" :: String
--    showVariables1 [x' 2, y] #=> "(x^2)y" :: String
--    showVariables1 [x, y' 2] #=> "xy^2" :: String
showVariables1 :: [Variable] -> String
showVariables1 = showVar . sort
                 where
                 showVar :: [Variable] -> String
                 showVar [] = ""
                 showVar ((Variable _ 0):_) = "" -- Hide things such as x^0, y^0 even tho they are still existing
                 showVar (a@(Variable _ 1):as) = show a ++ showVar as
                 showVar (a@(Variable _ _):[]) = show a
                 showVar (a@(Variable _ _):as) = "(" ++ show a ++ ")" ++ showVar as

class Substitutable a where
    -- See sub & |=| below
    into :: (Base, Coefficient) -> a -> a

instance Substitutable Term where
    (b, n) `into` Term c v = Term (c * replace n affectedVariables) unaffectedVariables
                            where
                            affectedVariables = filter (\(Variable b2 _) -> b2 == b) v
                            unaffectedVariables = filter (\(Variable b2 _) -> b2 /= b) v
                            replace :: Num a => a -> [Variable] -> a
                            replace _ [] = 1
                            replace a v_ = foldr1 (*) $ map (\(Variable _ p) -> a ^ p) v_

-- Only exists to make substituting an unknown with a number look nicer in code
-- How to use:
-- sub 'x' |=| 5 `into` term 5 [x]
-- => 25.0
-- Read as substitute x = 5 into term 5 [x]
sub :: a -> a
sub = id

-- Only exists to make substituting an unknown with a number look nicer in code
-- How to use:
-- sub 'x' |=| 5 `into` term 5 [x]
-- => 25.0
-- Read as substitute x = 5 into term 5 [x]
(|=|) :: Base -> Coefficient -> (Base, Coefficient)
b |=| c = (b, c)

instance Show Term where
    show (Term 1 v) = showVariables1 v
    show (Term c v) = show c ++ showVariables1 v

instance Eq Term where
    a@(Term c _) == b@(Term c2 _) = comparableTerms a b && c == c2

instance Ord Term where
    a@(Term c v) < b@(Term c2 v2)
        | comparableTerms a b = c < c2
        | otherwise = error $  "Unlike terms: " ++ showVariables1 v ++ showVariables1 v2

    a <= b =   a < b   ||   a == b
    a > b = not $ a <= b
    a >= b = not $ a < b

instance Num Term where
    a@(Term c v) + b@(Term c2 v2)
        | comparableTerms a b = Term (c + c2) v
        | otherwise = error $ "Unlike terms: " ++ showVariables1 v ++ showVariables1 v2

    Term c v * Term c2 v2 = Term (c * c2) $ variables
                            where
                            variables :: [Variable]
                            variables = condenseVariables $ v ++ v2
                                 
    abs (Term c v) = Term (abs c) v
    signum (Term c v) = Term (signum c) [ Variable b 0 | (Variable b _) <- v]
    fromInteger i = Term (fromIntegral i) [x' 0]

    negate (Term c v) = Term (negate c) v 