-- file: ch03/BookStore.hs
type CustomerID = Int

type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

type CardHolder = String

type CardNumber = String

type Address = [String]

data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

bookID (Book id title authors) = id

bookTitle (Book id title authors) = title

bookAuthors (Book id title authors) = authors

nicerID (Book id _ _) = id

nicerTitle (Book _ title _) = title

nicerAuthors (Book _ _ authors) = authors

myInfo =
  Book
    9780135072455
    "Algebra of Programming"
    ["Richard Bird", "Oege de Moor"]
