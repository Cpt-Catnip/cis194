# Algebraic Data Types

## Enumeration types

- like enumerations

```haskell
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show
```

- we can use pattern matching on enumerations

```haskell
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False
```

## Beyond enumerations

- enumerations are only a special case of algebraic data types
- this is also algebraic

```haskell
data FailableDouble = Failure
                    | OK Double
  deriving Show  
```

- an example of how to use such a type

```haskell
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)
```

- the data constructors aren't limited to a single argument

## Algebraic data types in general

- in general, an algebraic data type has one or more constructors and each constructor can have zero or more arguments
- side note: type and data constructors must begin with a capital letter and variables (including function names) must begin with lower case letters

## Pattern-matching

- pattern matching works by finding out which constructor a value was built with

```haskell
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr 4

foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...
```

- parens are required any time the constructor has arguments
- we can use a pattern of the form `x@pat` to match the pattern `pat` but also give the name `x` to the entire value being used in the pattern
  - neat!
- the following is the general rule for what can be used as a pattern

```haskell
pat ::= _
     | var
     | var @ (pat)
     | ( Constructor pat1 pat2 ... patn)
```

- what each line means, in order
  - `_` is a pattern and matches anything
  - a variable is a pattern that matches anything and binds the matched value tot he variable
  - this is for @-patterns, as described above
  - a constructor name followed by a series of patterns is itself a pattern
    - such a pattern matches if that value was construced using the given constructor and all provided patterns
- There are actually more rules but these are the important ones for now
- literal values like `2` or `'c'` are treated like constructors without arguments
  - they aren't actually that, but it's helpful to think about it that way

## Case expressions

- in general, a case expression looks like

```haskell
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
```

- the expression `exp` is matched against each of the patterns `pat1`, `pat2`, etc.
- the first matching pattern wins
- the way of defining functions we've seen with multiple patterns is actually just syntactic sugar for a case statement

```haskell
-- this
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- is the same as this
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                      Failure -> 0
                      OK d    -> d
```

- The former is obviously way more readable

## Recursive data types

- data types can be defined in terms of themselves (like the list)
- another example of a custom list and a binary tree
