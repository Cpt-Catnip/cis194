# Getting Started

## The interpreter
* `Prelude` or "the prelude" is essentially the stadard library of Haskell
* The `Prelude` is always implicitly available
* To get access to other modules, use the `:module` command

```haskell
ghci> :module + Data.Ration
```

_This particular module lets us work with rational numbers (fractions)_

## ghci as a Calculator
* you can use normal _infix_ syntax with ghci like `2 + 2` or `7.0 / 2.0`
* You can also write in _prefix_ be wrapping the operator in a parenthesis

```haskell
ghci> (+) 2 2
4
```

* You usually have to wrap negative numbers in parentheses in Haskell
* When we write `-3` in haskell, we're not writing "negative three"
* Rather, `-` is a unary operator acting on `3` turning it into `negative 3`.
  * `-` is actually Haskell's only unary operator
* Doing something like `2 + -3` is mixing infix and prefix and that's a no-no
* Instead we have to do `2 + (-3)` or `3 + (-(13 * 37))`
* In haskess, we apply a function by writing the name of the function and then its arguments (`f 3`)
  * If you think about it, doing infix arithmetic like this is actually an exception to how most functions work

## Boolean Logic
* Mostly all the same as other languages with one main distinction
* not equals in Haskell is written as `/=`
  * this is pretty cool, tbh
* In a similar vein, Haskell doesn't use `!` for logical negation, it uses `not`
  * also cool, imo

## Operator Precedence
* Operators have precedence from 1 (lowest) to 9 (highest)
* we can use the haskell `:info` command to get info about an operator, including its precedence

```haskell
ghci> :info (+)
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +
```

* The `infixl` part demonstrates that `+` is interpreted from left to right
  * The power operator (`^`) is `infixr`, which means it's interpreted right to left
* these rules are referred to as _fixity_ rules
  * lol

## Undefined and Variables
* use `let` to define a new variable

```haskell
let e = exp 1
```

* `^` only lets us raise a number to an integer value
* for non-integer powers, `like e to the power of pi`, do `e ** pi`
* Beep beep! `let` is only for use in `ghci`! We'll see how to do it in a normal Haskell program later.