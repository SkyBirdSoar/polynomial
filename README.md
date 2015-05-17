# polynomial
Some basic operations on polynomials in Haskell (created when I am a Haskell noob)

### Variable
``` haskell
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
```

`Variable` is an instance of `Show`, `Eq` and `Ord`.

### Term
``` haskell
module Polynomial.Term
( module Polynomial.Variable
, Term -- Coefficient * Variable = Term
, Substitutable (..) -- Term is an instance of Substitutable
, Trimable (..) -- Term is an instance of Trimable
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
```

`Term` is an instance of `Substitutable`, `PolynomialType Term Polynomial`, `PolynomialType Polynomial Term`, `PolynomialType Term Term`, `DivisiblePolynomialType Polynomial Term`, `Num`, `Eq`, `Show`, `Ord`

It implements:
- |+| -- adds Terms together to form a polynomial (From PolynomialType)
- |-| -- subtract Terms to form a polynomial (From PolynomialType)
- |*| -- multiplies Terms to form a polynomial (From PolynomialType)
- |/| -- simple division of Terms to form a polynomial (From DivisiblePolynomialType)
- into -- substitute an unknown/variable in Term (From Substitutable)
- trim -- trim all zero-powered variables (From Trimable)

### Polynomial
``` haskell
module Polynomial.Polynomial
( Polynomial
, module Polynomial.Term
, mkpoly -- Smart constructor ([Term] -> Polynomial)
, asPoly -- Smart constructor (Term -> Polynomial)
) where
```

`Polynomial` is an instance of `Substitutable`, `PolynomialType Term Polynomial`, `PolynomialType Polynomial Term`, `PolynomialType Polynomial Polynomial`, `DivisiblePolynomialType Polynomial Term`, `DivisiblePolynomialType Polynomial Polynomial`

It implements:
- |+| -- adds Polynomials/Terms together to form a polynomial (From PolynomialType)
- |-| -- subtract Polynomials/Terms to form a polynomial (From PolynomialType)
- |*| -- multiplies Polynomials/Terms to form a polynomial (From PolynomialType)
- |/| -- simple division of Polynomials/Terms to form a polynomial (From DivisiblePolynomialType)
- into -- substitute an unknown/variable in Term(s in a polynomial) (From Substitutable)
- trim -- trim all zero-powered variables (From Trimable)

If in doubt, consult the relevant files for a more detailed explanation!

Note: 
- `Substitutable` is in `term.hs`
- `PolynomialType` is in `polynomial.hs`
- `DivisiblePolynomialType` is in `polynomial.hs`