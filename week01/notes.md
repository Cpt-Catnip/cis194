# Haskell Basics
* Is it necessary to take notes on _all_ the readings? How redundant is this going to get?

## What is Haskell?
* When we say Haskell is functional, we have two things in mind
  * functions are first-class
  * running a haskell program is centered around evaluating expressions rather than executing instructions
* one downside to lazy evaluation is that it makes thinking about time and space complexity more... well, complicated

## Themes
* __Types!__
  * A large part of writing haskell programs is writing down all the types
* Abstraction
  * haskell is good at abstraction
  * parametric polymorphism, higher order functions, and type classes all help in this
* wholemeal programming
* I'm not getting it yet
* Okay we're through that section now and I don't know what it means LOL

## Literate Haskell
* a cool jupyter notebook-like file with text and executable code intermingled

## Declaration and Variables
```haskell
x :: Int
x = 3
-- single line comment 
{- this is a
   multiline comment
-}
```

* when stating the type of a variable, we say "x has type int"
* The value of `x` cannot be changed later
* variables are not mutable boxes, they are names for values!
* what does `y = y + 1` do?

## Basic Types
* `Int`: machine-sized integers
* `Integer`: arbitrary-precision integers
  * limited by the amount of memory on your machine
* `Double`: double-precision floating point
* `Float`: single-precision floating point
* `Bool`: boolean
* `Char`: uinicode characters
* `String`: lists of characters with special sytax

## GHCi
* the REPL!

## Arithmetic
* heskell does not do implicit conversion
* `/` is exclusively for floating-point division while `div` is for integer division

## Boolean Logic
* `&&` for logical _AND_ and `||` for logical _OR_
* `not` is the _NOT_ operator
* `/=` is the _NOT EQUAL_ operator
* the rest is as you'd expect
* haskell has if-expressions, though idiomatic haskell prefers pattern matching and _guards_

## Defining basic functions
* we can write functions on integers by cases

```haskell
-- Compute the sum of integers from 1 to n
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)
```

* Looks like you can specify cases by doing something like `sumtorial 0 = 0`
  * kinda neat honestly
* clauses are checked from top to bottom and the first one that matches is evaluated
* we can also use _guards_ to make choices

```haskell
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
```

* you can use any number of guards for a clause in a function definition
* guards are evaluated from top to bottom and the first to evaluate to true is chosen
* consider `hailstone 3`
  * `3` is matched against `n`, which passes since a variable matches anything
  * `` n `mod` 2 == 0`` evaluates to false
  * finally, `otherwise` is a synonym for `True`, so `hailstone 3` evalutes to `3*3 + 1 = 10`
* another example

```haskell
foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0           = 0
  | n `mod` 17 == 2 = -43
  | otherwise       = n + 3
```

* okay so I guess the `| ...` things are guards and the `foo n` things are cluases
* okay so here's a too complicated even checker (allegedly)

```haskell
isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | oterwise       = False
```

* I guess prof wants us to define it as ``isEven n = n `mod` 2 == 0``?

## Pairs
* a 2-tuple is a pair
  * e.g. `(3, 'x')` which has type `(Int, Char)`
* we can extract values from pairs (and tuples in general?) using pattern matching as such

```haskell
sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y
```

* prof suggests there are better ways to pair three or more elements, so we should never use tuples for _anything_ other than pairs
  * as we've seen in the readings, tuples aren't defined for singletons like `(1)`

## Using functions, and multiple arguments
* as we know, just list the arguments after the function with spaces

```haskell
f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8
```

* I only wrote that down because look  at that wild type signature
* We'll learn why it's like that in a few weeks but it's supposed to feel strange right now
* note that function application has the highest precendence than any infix operation
* therefore, something like `f 3 n+1 7` gets interpreted as `(f 3 n) + (1 7)`
  * instead, you have to write `f 3 (n+1) 7`

## Lists
* I know a lot of this so I won't write it down, but you can declare the type of multiple vars at once if they share a type

```haskell
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]
```

* all standard library functions for processing lists can be used on strings

## Constructing lists
* `[]` is the simplist possible list
* other lists are simply built up from using the _cons_ operator (`:`)
* `ex21 = [2,3,4] == 2 : 3 : 4 : []` evaluates to true
  * the former is shorthand for the latter
* These are singly linked lists and not arrays, whatever that means
  * oh I think it means index lookup doesn't happen in constant time since arrays are just a specific implementation of hash maps

```haskell
-- Generate the sequence of hailstone iterations from a starting number
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
```

## Functions on lists
* we can write functions on lists using pattern matching

```haskell
-- Compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs
```

* What happens when `xs` has one element? is the recursive `xs` just `[]`? Probs.
* We can also use _nested patterns_

```haskell
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs
```

* yeah okay why not
* you don't actually need the parens so you could also just write `x:y:zs` though that kinda sucks

## Combining functions
* it's good haskell style to build up complex functions from more simple functions
* for example

```haskell
-- The number of hailstone steps needed to reach 1 from a starting
-- number
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
```

* this seems inefficient at first
* first you genereate the whole sequence, then you calculate its length
* due to haskell's lazy evaluation, each each element of the sequence is only generated as needed, so the sequence generation and list length calculatoin are interleaved
* this means the computation is O(1)
  * this is not entirely true but we will later learn how to make it true

## A word about error messages
* Don't be scare!
* Read error messages; they usually contain a lot of useful information
  * Interesting that every resource has been saying this