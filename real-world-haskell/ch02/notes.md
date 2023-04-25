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
* 
