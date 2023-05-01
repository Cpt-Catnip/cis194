-- file: week01/homework/hanoi.hs
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = [(p1, p2)]