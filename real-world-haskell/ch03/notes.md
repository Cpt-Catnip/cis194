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
  - we need to find out which value constructor was used
  - we need to extract data if the constructor had data fields
- in comes \*~pattern matching~\*
- "A pattern lets us look inside a value and bind variables to the data it contains"
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

- Here we looked into the list within the pattern
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

- This is kind of freaky
- One thing we can do is add polymorphism into our types by using type variables in our type declarations
- For example, there's a type `Maybe`, which represents that a value (of any type) is either present or missing.

```haskell
-- file: ch03/Nullable.hs
data Maybe a = Just a
             | Nothing
```

- this is the most hilarious code I've ever seen- it's so passive aggressive
- `a` here is a type variable, not a type or a regular variable
- `Maybe` takes a type as a parameter
- Passing a type to `Maybe` makes a new, disticnt type
- `Maybe Int` is its own type!
- You can nest parameterized types like `Just (Just "wrapped")` but I have no idea why or what it does

## Recursive Type

- The list type is recursive
- For example a list is a value followed by a list (linked list)
- our own implementation of a list might look like

```haskell
-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)
```

- here we're replaces `(:)` with `Cons` and `[]` with `Nil`
- The value constructor (`Cons`) contains the type constructor (`List`)
- recursive!
- binary tree example

```haskell
-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
```

- Now they're comparing to Java, which is unfamiliar to me
- Haskell doesn't have an analog to `null` or `nil` or anything like that, so we used the no-argument `Empty` constructor to indicate no value
- we could have used `Maybe` but the author says that this would bloat the pattern matching
  - I guess I don't know enough about pattern matching to recognize that yet

## Reporting Errors

- Haskell provides an error function `error :: String -> a`
- Error is different from other functions since it immediately aborts evaluation and prints an error message
- the `String` argument for `error` is the message we want repoted
- `error` returns the type variable `a` so it can be called anywhere, in any function, without violating type rules
- when a function errors, the result looks like `*** Exception: list too short`
- One drawback of `error` is that it doesn't distinguish fatal errors and recoverable errors; it __always__ terminates evaluation
- example of function that pulls the second element from a list

```haskell
-- file: ch03/MySecond.hs
mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
```

### A More Controlled Approach

- `Maybe` can represent the possibility of an error
- We can use `Nothing` to indicate that an operation has failed
- For example with `mySecond`

```haskell
-- file: ch03/MySecond.hs
safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
```

- still not exactly sure what `Just` is
- maybe it's the happy path when the return type is `Maybe`
  - like if something returns `Maybe a`, then you can't have a branch that returns `a`, it has to return `Just a`
- Something funny about seeing "`Just 2`" hahaha
- one more optimization with pattern matching

```haskell
-- file: ch03/MySecond.hs
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
```

- This just says "if the list as a second element, return it, otherwise return `Nothing`
  - Note that a list always has the empty element `[]` at the end (just like sets)
- also recall that order here matters. If we defined the empty pattern first, `tidySecond` would always return `Nothing`
- One way to think about `Maybe` is like a promise in JS
  - It will either pass or fail
  - `Just x` is like `resolve(x)` in JS
  - `Nothing` is like `reject()` in JS

## Introducing Local Variables

- introducing variables in the body of a function
- use `let` to introduce local variables
- example

```haskell
-- file: ch03/Lending.hs
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
```

- I guess the signature here would be `lend :: Int -> Int -> Maybe Int`
  - hmm the compiler says `lend :: (Ord a, Num a) => a -> a -> Maybe a`
  - what is `Ord` and `Num`???
- `let` actually starts a _block_ of local variables
- then we use `in` to end it
- think "_let_ these variables be present _in_ the following function"
- each line in `let/in` is a new variable
- a name in a `let` block is bound to an _expression_ not a _value_

### Shadowing

- we can nest multiple `let` blocks (but why?)

```haskell
-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 2
          in a + b
```

- trivial exmaple, I'm sure
- it's legal but unwise to repeat variable names in nested `let` expressions

```haskell
-- file: ch03/NestedLets.hs
bar = let x = 1
      in ((let x = "foo" in x), x)
```

- this one hurts my brain a little bit
- does the nested `let` not have access to the outter `x`?
- Oh that's precisely the point; the inner x is "shadowing" the outer one (can't access outer x)
- We can also shadow function parameters
- in general, shadowing can be confusing and should probably be avoided

### The `where` Clause

- the definitions in a `where` clause apply to code that comes before it (weird!!!)

```haskell
-- file: ch03/Lending.hs
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
  where reserve = 100
        newBalance = balance - amount
```

- I mean I guess it brings the business logic to the top
- hmm idk I'm envisioning reading a function and going "wait but what does this other thing represent"
- in all of this, whitespace is important!!!

### Local Functions, Global Variables

- we can also define local functions!
- okay this is a great use-case for `where` since it would really busy up the body

```haskell
-- file: ch03/LocalFunction.hs
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"
```

- local functions can freely use vars from the scopes that enclose them

## The Offsice Rule and Whitespace in an Expression

This is a big chapter jeez

- Ooh big stuff here I think
- Haskell uses indentation as a cue to parse sections of code
- the use of layout to convey structure is called the _offside rule_.
- the fist top-level (global) declaration can start at any column but then all subsequent top-level declarations also need to start at that column
- any empty line or a line indented further to the right is treated as a continuation of the previous line
- I guess that means that a new declaration doesn't start until a character shows up in that initial column, or something
  - or __reduced__ indentation!
- this rule applied to `let`/`where` clauses as well!
- here's a weird fake example to demonstrate some of this

```haskell
-- file: ch03/Indentation.hs
foo = let firstDefinition = blah blah
          -- a comment-only line is treated as empty
                              continuation blah

          -- we reduce the indentation, so this is a new definition
          secondDefinition = yada yada

                             continuation yada
      in whatever
```

- I don't think this would actually compile
- and other weird stuff that I'm sure I'll wrap my head around one day!

### A Note About Tabs Versus Spaces

- use space characters!!
- this is because a tab means something different based on where they're placed; spaces are always the same
- if the format of the code breaks on someone else's machine, then the code can break! Eek!

### The Offside Rule Is Not Mandatory

- We can actually use curly braces to be more explicit in our code's structure
- start a block with `{`, separate items with `;`, and finish the block with `}`
- These two functions are isomorphic

```haskell
-- file: ch03/Braces.hs
bar = let a = 1
          b = 2
          c = 3
      in a + b + b

foo = let { a = 1; b = 2;
        c = 3 }
      in a + b + c
```

- with explicit structuring, indentation doesn't matter
- we can obviously see that the first example is much prettier!

## The `case` Expression

- `case` lets us use pattern matching _within_ an expression
  - as opposed to in defining expressions (functions)
- example from `Data.Maybe` for unwapping a `Maybe`

```haskell
-- file: ch03/Guard.hs
fromMaybe defval wrapped =
    case wrapped of
      Nothing    -> defval
      Just value -> value
```

- `case` is followed by an arbitraty expression
- the pattern matching is performed against the result of this expression
- `of` signifies the end of the expression and the start of the pattern matching
- matches are attempted from top to bottom
- we can use the wildcard pattern (`_`) as a fallback if all other patterns fail

## Common Beginner Mistakes with Patterns

- avoid these, you dummy

### Incorrectly Matching Against a Variable

```haskell
-- file: ch03/BogusPattern.hs
data Fruit = Apple | Orange
apple = "apple"
orange = "orange"
whichFruit :: String -> Fruit
whichFruit f = case f of
                  apple -> Apple
                  orange -> Orange
```

- `apple` and `orange` aren't actually referring to the top-level variables here, they're referring to local pattern variables
- the above is equivalent to

```haskell
-- file: ch03/BogusPattern.hs
equational apple = Apple
equational orange = Orange
```

- we could use a `where` clause to make it better?
- patterns that always succeed, such as the wild card or plain variables, are called _irrefutable_ patterns
- the correct function would just have the strings `"apple"` and `"orange"`

### Incorrectly Trying to Compare for Equality

- say we want to check that the value in two nodes are identical
- you might write the following function

```haskell
-- file: BadTree.hs
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
```

- you actually can't reuse `a` like that!!
- you can't place a variable in multiple locations
  - that's like a function for some `x` having two distinct outputs
- instead we can solve this problem with guards

## Conditional Evaluation with Guards

- oh yeah this was introduced somewhere else
- pattern matching is good for checking a value's shape but guards let us do more quantitative checks

```haskell
-- file: ch03/BadTree.hs
nodesAreSame (Node a _ _) (Node b _ _)
  | a == b       = Just a
nodesAreSame _ _ = Nothing
```

- see we can actually do comparison on the values from the pattern match
- `|` introduces the guard
- guards are tested in the order they're defined and the first one that succeeds gets evaluated
- if no guards pass, we move on to the next pattern match
- all variables mentioned in the pattern are available to the guard
- There's a special guard expression `otherwise` which is bound to `True`
- this is used like a default guard if the others don't pass

```haskell
-- file: ch03/Lending.hs
lend3 amount balance
    | amount <= 0            = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
```

- we can use guards anywhere we can use patterns
- using patterns and guards lets us break up function definitions based on how we expect them to behave instead of all the logic being mixed up in if/else statements
