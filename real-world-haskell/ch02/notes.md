# Types and Functions
* Every expression and function has a type

## Haskell's Type System
* Haskell types are __strong__, __static__, and can be __inferred__

### Strong Types
* Ensures that we can't misuse types and get crazy runtime errors
  * for instance you can add a function to an integer in JS

```javascript
// amazingly, this is valid javascript
const x = () => true;
console.log(x + 1)  // '() => true1'
```

* `well typed`: an expression that obeys a language's type rules
* `ill typed`: an expression that _disobeys_ the type rules
  * this kind of expression will cause a `type error`
* haskell does _no_ type coercion/casting

### Static Types
* The compiler knows the type of every value and expression at compile time
* That is, haskell code _won't_ run if we are trying to misuse its type system - for instance if we try to do `True && "false"`
* Dynamically typed languages certainly have their benefits (think expressions like `true && someFunc()` in JS) but _typeclasses_ in haskell make up for this
* A haskell program that compiles _will not_ suffer from type error when it runs

### Type Inference
* Haskell can infer the types of almost all expressions
* This means that we don't usually have to specify the type of a value or expression

## Some Common Basic Types
* `Char`: a unicode character
* `Bool`: `True` or `False`
* `Int`: signed, fixed width integer values
* `Integer`: signed integer of unbounded size
  * These are less common than `Int`
* `Double`: floating-point number
* use `::` to specify the type of an expression
* `::` followed by the type is called a _type signature_

## Function Application
* To apply a function, write the name of a function followed by its arguments like `odd 3`
* we don't use parentheses!!!
* Function applications have higher precedence than using operators (like `==`)
* There are other times where using parens are needed

## Lists and Tuples
* These are both considred "composite data types" which means they're composed of other data types
* `head` returns the first element in a list
* `tail` returns all _but_ the head
* a list can have any value inside, so we call it polymorphic
* we need to use a _type variable_ when using a polymorphic type, which always begins with a lowercase letter
* a `tuple` is a fixed-size collection of values where each value can have a different type
  * strange!
* list elements must all be the same type
* Write a tuple by enclosing the data in parens like `(1964, "Labyrinths")`
* Defining the type is the same
  * `:type (True, "hello")`
* Haskell has a special type `()` called a unit
  * is has one value, which is also `()`
* There are no single-elemend tuples
* The order of the elements in a tuples matters and changes its type

## Functions over Lists and Tuples
* `take` returns the first `n` elements in a list
  * `take 2 [1, 2, 3, 4, 5]` returns `[1, 2]`
* `drop` is the opposite and returns all _but_ the first `n` elements
  * `drop 3 [1, 2, 3, 4, 5]` returns `[4, 5]`
* note that we separate a function's arguments by whitespace, not commas
* `fst` and `snd` return the first and second element of a tuple, respectively
  * these functions are only applicable to 2-tuples
  * `fst :: (a, b) -> a`

## Passing an Expression to a Function
* without parens, functions are evaluated __left to right__
* This differs from something like JS, where the rightmost function gets evaluated first
* For example, `a b c d` is equivalent to `(((a b) c) d)` while in JS you'd have to write it `a(b)(c)(d)`
  * okay that's not actualy that different

## Function Types and Purity
* If a function has side effects, the type of the function's result will begin with `OI`

```shell
ghci> :type readFile
readFile :: FilePath -> IO String
```

* The side effect (hidden input variable) here is the state of the file being pointed to by `FilePath`

## Haskell Source Files, and Writing a Simple Function
* haskell files end in the `.hs` suffix
* you can load source files into GHCI by doing `:load add.hs`
* you need to either be in the directory of the file you're loading or point to the file with a relative filepath
  * To change GHCI's working dir, use the `:cd` command

## Just What Is a Variable, Anyway?
* Variables give names to expressions
* Once a variable is bound to an expression, it's value can't be changed
* For example, the following is valid python

```python
x = 10
x = 11
print x    # 11
```

* but the following haskell will return an error

```haskell
x = 10
x = 11
```

## Conditional Evaluation
* haskell has `if`
* Here's an implementation of `drop`

```haskell
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
```

* `null` is a function that checks if a list is empty
* indentation matters in haskell
  * indenting lines continues an existing definition
  * damn I actuall prefer code blocks but oh well
* So some notes on `if`
  * immediately following `if` is the boolean condition, known as the _predicate_
  * `then` followed by another expression is what will run if the predicate evaluates to `True`
  * `else` followed by another expression is what will run if the predicate evaluates to `False`
  * each of these will be referred to as _branches_
* Branches must have the same type
* Additionally, `if` statements __must__ have an `else` branch
* The `(||)` operator short circuits, meaning if the left-hand operator evaluates to `True`, the right-hand side won't evaluate
* by convention, we align the `then` and `else` keywords under `if` though the exact amount of indentation isn't actually important
  * we could even write the whole expression in one line

## Understanding Evaluation by Example
```haskell
-- file: ch02/RoundToEven.hs
isOdd n = mod n 2 == 1
```

* Consider `isOdd (1 + 2)`
* In most languages that use _strict_ evaluation, `(1 + 2)` would be evaluated first, then passed to `isOdd` and so on
* Haskell uses __lazy evaluation__, which means a "promise" is created that `isOdd (1 + 2)` will be evaluated once needed
* The record of unevaluated expressions is called a __thunk__
* If the result of the expression is never needed, it's never evaluated
* For instance, consider `print(myDrop 2 "abcd")`

```haskell
-- file ch02/myDrop.hs
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
```

* After the first recursive call, the predicate is _not_ `if 1 <= 0 || null "bcd"`
* Because of lazy evaluation, we instead have the thunks `if (2 - 1) <= 0 || null (tail "abcd")`
* Once we're done with all the recursive calls though, we still just have `tail "bcd"` since we don't know if we'll need that result yet
* `tail "bcd"` is only evaluated once ghci needs to print the expression
* __n.b.__ the result of evaluating an expression may be a thunk!

## Polymorphism in Haskell
* consider the `last` list function
* `last` should always return the last element of a list regardless of what type the list's elements are

```shell
ghci> :type last
last :: [a] -> a
```

* the type of `last` uses a type _variable_ (`a`) instead of an actual type like `Int` or something
* if a function type has a type variable in its signature, that function is polymorphic
* both functions _and types_ can be polymorphic
* Here's something important: Haskell has _no way_ to determine the real type of a value with a parameterized type
  * that is, it can't operate on the value, manipulate it, create a value or whatever; haskell just treats it as a black box
  * what "it" means here I'm still a little unclear on
* for example

```shell
ghci> :type fst
fst :: (a, b) -> a
```

* since we know haskell can't create a new value with type `a` given polymorphism and all that, then it _must_ be that haskell just returns the value in `a`'s position

## Further Reading
* There is a deep mathematical sense in which any nonpathological function of type (a,b) -> a must do exactly what fst does
* Moreover, this line of reasoning extends to more complicated polymorphic functions
* The paper “Theorems for free” by Philip Wadler (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.9875) covers this procedure in depth

## The Type of a Function of More Than One Argument
* Finally!

```shell
ghci> :type take
take :: Int -> [a] -> [a]
```

* like... huh?
* haskell groups the arrows from right to left - the `->` is right associative
* the above is the same as `Int -> ([a] -> [a])`
* think about it like chaining operations
  * pass an `Int`, and you get a function that accepts and returns a `[a]` list
  * however, we're passing an `Int` AND a `[a]` so it's like we're making two function calls at once (to put it in imperative language)
