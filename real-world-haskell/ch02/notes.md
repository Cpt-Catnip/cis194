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
