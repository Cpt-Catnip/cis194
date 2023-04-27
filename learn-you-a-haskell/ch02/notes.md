# Starting Out

* functions that take two arguments can be make infix by surrounding them in backticks like ``92 `div` 10``.
* `[1,2,3]` is syntactic sugar for `1:2:3:[]`
  * neat!
  * recall `:` is the _add to beginning of list_ operator
* To get something at a specific location in a list, use `!!` like `[0,1,2,3,4,5] !! 3`
  * trying to get data at an out of bounds index returns an error
* lists can be compared if they contain comparable stuff
  * when using `<`, `<=`, `>`, or `<=`, the lists are compared in lexicographical order (element-wise)
* use `..` inside of a list to generate a range of numbers of anything that can be enumerated (like characters)
  * `[1..20]` creates all the natural numbers between `1` and `20`
* You can include a step size in your ranges by specifying the first two elements like `[n0,n1...stop]`
  * the step size will be inferred from the first two elems
* for example, to generate the reverse of `[1..20]`, you need to do `[20,19..1]`
* Probably don't use floating point numberse in list ranges...

```shell
ghci> [0.1, 0.3 .. 1]  
[0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
```

* one benefit of haskell's lazy evaluation is taking the first N elements of an infinite list
* for instance, if we want the first 24 multiples of 13 (starting at 1), we can do `take 24 [13,26..]`
* `cycle` infinitely cycles through a list

```shell
ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
```

* `repeat` infinitely repeats a value

```shell
ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]
```

* For the above example, you could more simply do something like `replicate 3 10` to create a list of three `10`s

## List Comprehension
* !!!!!
* list comprehension in haskell tries to follow set comprehension from mathematics
* to get the first 10 natural numbers, we'd do `[x*2 | x <- [1..10]]`
* we can also add predicate (conditions) to the list comprehension after the binding
* the following returns the first 10 natural numbers that are also greater than or equal to 12

```shell
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
```

* probs could have made the predicate `x >= 6` but oh well
* adding a predicate is called __filtering__
* you could could make a large logical predicate, or you could combine multiple predicates with commas

```shell
ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  
[10,11,12,14,16,17,18,20]
```

* nice!
* we can also draw from several lists
* when doing this, the list comprehension will produce all combinations of the given lists
  * that means if we use lists with size `N` and size `M`, the resulting list will be of size `N * M` (w/o filtering)
* `haskell` also uses `_` to ignore ins/outs

```haskell
length' xs = sum [1 | _ <-xs]
```
* this just creates a list of all ones where there's a one for each element in `xs`
  * then sums them
* Note that strings are lists, so we can use comprehension to process and produce strings
* You can also create and operate on nested lists because why wouldn't you be able to
* this removes all odd numbers without flattening the list

```shell
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]  
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

## Tuples
* hmm they're making it seem like a tuple is the best way to represent state, which means there's _probably_ no notion of an opbject or whatever
* `zip` combines two lists into a list of 2-tuples (pairs)
* `zip` will return a list with a length equal to the shorter of the two lists

```shell
zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  
[(5,"im"),(3,"a"),(2,"turtle")] 
```

* because haskell is lazy, we can zip finite lists with infinite lists!
* Here's a super neat application of tuples and list comprehension
* Let's say we want a list of all triangles where...
  * all side lengths are integers
  * no side length is greater than 10
  * the perimeter of the triangle is 24
* we can generate that list by simply doing

```shell
ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  
ghci> rightTriangles'  
[(6,8,10)]
```

* wow!!!
