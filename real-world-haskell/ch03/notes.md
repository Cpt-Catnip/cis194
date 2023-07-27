# Defining Types, Streamlining Functions

## Defining a New Type

- It's often useful to create our own data types to add structure to our code
- For example, a bookstore might want to store data about books

```haskell
data BookInfo = Book Int String [String]
                deriving (Show)
```

- what ever comes after `data` - here `BookInfo` - is the name of the datatype
  - this name is called the _type constructor_
- `Book` is the _value constructor_
  - we use the _value constructor_ to create a variable of the _type constructor_ type
- Both identifiers need to start with a capital letter
- The things that follow the _value constructor_ - here `Int`, `String`, and `[String]` - are the components, which act like fields in objects
- Here the `Int` is the ID of the book, the `String` is the title, and the `[String]` is the list of authors
  - It seems that the names for the fields are not set and are just known by the human programmer
- Only a value of `BookInfo` can be used where it's expected even if another type with another consturctor has all the same components, e.g.

```haskell
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
```

- The `deriving (Show)` bit will be explained in chapter 6 but for now know that it's how _ghci_ knows how to print a value of that type
- create a new `BookInfo` value by treating `Book` as a function

```haskell
MyInfo = Book 12345 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]
```

## Naming Types and Values

- Above the type and value constructors were given different names but in fact it's conventional to use the same name for both
- this is because their uses are independant and which constructor you are using can be inferred from where you're reading it
  - If it's in the type signature, it's the type constructor
  - If it's in an expression, it must be the value constructor

```haskell
data BookReview = BookReview BookInfo CustomerID String
```

- `CustomerID` type to be introduced shortly

## Type Synonyms

- Creating type synonyms lets us give our data more descriptive meaning
- For example `String` in `BookReview` doesn't tell us what that component represents
- Instead, we can do

```haskell
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
```

- `type` instroduces a synonym
- the two names (e.g. `CustomerID` and `Int`) identify _the same type_
- You can also use synonyms to to create shorter names for verbose types

```haskell
type BookRecord = (BookInfo, BookReview)
```

- Note that synonyms only refer to type definitions and you still need to use the same value constructor to create a value of that type

## Algebraic Data Types

- Bool is an algebraic data type! Cool!
- Algebraic data types can have more than one data constuctor

```haskell
data bool = False | True
```

- When a type has more than one constructor, they're called _alternatives_ or _cases_
- Each of an alg. types constructors can take zero or more arguments
  - that is, they don't have to have the same signatures and don't even require data
- for example

```haskell
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
```

## Tuples, Algebraic Data Types, and When to Use Each

- Two tuples can, in theory, have the same structure but represent different things
- In this case, the compiler considers the two equivalent and you could send a tuple representing a turtle to a function expecting furniture
- Since alg types have different names, they are different types to the compiler

```haskell
-- file: ch03/AlgebraicVector.hs
-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
  deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
  deriving (Eq, Show)
```

- the `Eq` that we added to `deriving` makes haskell generate code that lets us compare values of each
- When deciding which to use, tuple or algebraic data type, there's only a rule of thumb
- Use algebraic data types if you're using compound data types widley in your code. For small/local uses, use a tuple.

## Analogues to Algebraic Data Types in Other Languages

- `struct` in C/C++
- `enum` in C/C++
  - I guess you can just have a bunch of zero-arg value constructors and then it's an enum
- `union`
  - This is unfamiliar to me
- Anything defined using the `data` keyword is an algebraic data type
  - B)

## Pattern Matching

- Cool I keep seeing this come up
- when using alg types
  - we need to find our which value constructor was used
  - we need to extract data if the constructor had data fields
- in comes \*~pattern matching~\*
- "A pattern letse us look inside a value and bind variables to the data it contains"
- For example?

```haskell
-- file: ch03/add.hs
myNot True = False
myNot False = True
```

- The above defines the behavior of `myNot` for two different _patterns_
  - The pattern is what comes after `myNot` and until `=`
- Another, perhaps better, example

```haskell
-- file: ch03/add.hs
sumList (x:xs) = x + sumList xs
sumList []     = 0
```

- the haskell linter wants me to use `foldr` for `sumList` but I don't know what that does yet...
- When a pattern isn't matched, we "fall through" to the next pattern
- patterns are checked in the order they are defined and matching stops at the first success

## Construction and Destruction

- When doing destruction, the compiler first checks to see if the value was made using that constructor
  - _that_ meaning the one used in the destruction I guess
- for example, in `(Book id name authors) = Book 9 "Close Calls" ["John Long"]`
  - `id` gets `9`
  - `name` gets `"Close Calls"`
  - `authors` gets `["John Long"]`
- destruction is sometimes called deconstruction

## Further Adventures

- Pattern matching a tuple looks similar to constructing one

```haskell
third (a, b, c) = c
```

- You can look arbitrarily deep into a value for patterns

```haskell
complicated (True, a, x:xs, 5) = (a, xs)
```

- Here we looked into the list within the patterh
- Also here we specified specific values, so if the first and last elements aren't `True` and `5`, respectively, the pattern won't match
- To pattern match on an algebraic type, we use its value constructor
  - e.g. `bookID (Book id title autors) = id`

## Variable Naming in Patterns

- `(x:xs)`
- `xs` is plural of `x`
- "ex" and "exes"

## The Wild Card Pattern

- The wildcard, `_`, means we don't care what's in this part of the pattern
- This should be familiar from many languages
- e.g. `nicerID (Book id _ _) = id`
- Wild cards function similarly to variable but don't bind a new variable

## Exhaustive Patterns and Wild Cards

- When writing patterns for a function (or something) it's important to cover _all_ cases of the certain argument type
- For example, if you only cover the non-empty constructor - `(:)` - then you'll get an error if you pass an empty constructor - `[]`
- You can use a compiler flag to warn about incomplete constructors
- We can also use a wildcard pattern if we don't care about other constructors, e.g.

```haskell
-- file: ch03/BadPattern.hs
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
```

- essentially saying "else zero"
- This will catch `[]` but will it also catch literally anything else?
  - no
- I guess the compiler is smart enough to know that `_` means any other _list_ pattern

## Record Syntax

- Haskell programmers don't like boilerplate
- To avoid having to create accessor functions for each of a data types components, you can use record syntax to define the data type and accessors at the same time

```haskell
-- file: ch03/BookStore.hs
data Customer = Customer
  { customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
  }
  deriving (Show)
```

- this is equivalent to

```haskell
-- file: ch03/AltCustomer.hs
data Customer = Customer Int String [String]
  deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
```

- all hail brevity!
- We can still use the same syntax to create a value of type `Customer` but it also exposes a new, more verbose and readable syntax

```haskell
customer2 =
  Customer
    { customerID = 271828,
      customerAddress =
        [ "1048576 Disk Drive",
          "Milpitas, CA 95134",
          "USA"
        ],
      customerName = "Jane Q. Citizen"
    }
```

- nice!
- note that if we use this syntax, order no longer matters when declaring the value
- Using record syntax also makes the _printed_ values have keys, which is also nice and more readable
  - what's even the downside????
    - other than verbosity I guess

## Parameterized Types

- 
