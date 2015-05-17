{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Polynomial.Polynomial
( Polynomial
, module Polynomial.Term
, mkpoly -- Smart constructor ([Term] -> Polynomial)
, asPoly -- Smart constructor (Term -> Polynomial)
) where

import Polynomial.Term
import Data.List

-- A polynomial is a list of terms
newtype Polynomial = Polynomial [Term]

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- Smart constructor for Polynomials
-- Condenses terms before turning it into a Polynomial
-- Eg mkpoly [mterm 1 x, mterm 2 x] #=> 3.0x :: Polynomial
mkpoly :: [Term] -> Polynomial
mkpoly xs = Polynomial $ condenseTerms xs

-- Converts a term to a polynomial
-- Eg asPoly (mterm 1 x) #=> x :: Polynomial
asPoly :: Term -> Polynomial
asPoly a = mkpoly [a]

-------------------------------------------------------------------------------
instance Show Polynomial where
    show (Polynomial a) = intercalate " + " $ map show a

instance Eq Polynomial where
    Polynomial a == Polynomial b = orderTerms a == orderTerms b -- condenseTerms seems like more work to do for the runtime...
-------------------------------------------------------------------------------

class PolynomialType a b where
    (|+|) :: a -> b -> Polynomial
    (|*|) :: a -> b -> Polynomial
    (|-|) :: a -> b -> Polynomial -- Shorthand for a + (-b)

instance PolynomialType Polynomial Term where
    Polynomial a |+| b = mkpoly $ b : a
    Polynomial a |*| b = mkpoly $ map (* b) a
    Polynomial a |-| b = mkpoly $ (-b) : a

instance PolynomialType Term Polynomial where
    (|+|) = flip (|+|)
    (|*|) = flip (|*|)
    (|-|) = flip (|-|)

instance PolynomialType Polynomial Polynomial where
    Polynomial a |+| Polynomial b = mkpoly $ a ++ b
    Polynomial a |*| Polynomial b = mkpoly $ [ c * d | c <- a, d <- b ]
    Polynomial a |-| Polynomial b = mkpoly $ a ++ [ -c | c <- b ]

instance PolynomialType Term Term where
    a |+| b = mkpoly [a, b]
    a |*| b = mkpoly $ [a * b]
    a |-| b = a |+| (-b)

class DivisiblePolynomialType a b where
    (|/|) :: a -> b -> Polynomial

instance DivisiblePolynomialType Polynomial Term where
    Polynomial a |/| b = mkpoly $ map ((flip tDivT) b) a

instance DivisiblePolynomialType Polynomial Polynomial where
    a@(Polynomial _) |/| Polynomial b = foldr (\d c -> c |/| d) a b

instance Substitutable Polynomial where
    -- see Explanation in term.hs
    a `into` Polynomial b = mkpoly $ map (a `into`) b

instance Trimable Polynomial where
    -- see Explanation in term.hs
    trim (Polynomial a) = mkpoly $ map trim a