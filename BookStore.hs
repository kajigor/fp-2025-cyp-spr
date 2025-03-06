module BookStore where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE


data Person = Person
  { firstName :: String,
    lastName :: String,
    yearOfBirth :: Int
  }

instance Show Person where
  show p = firstName p ++ " " ++ lastName p ++ ", born " ++ show (yearOfBirth p)

-- A book has at least one author, thus we use `NonEmpty` which is a list with at least one element.
-- See https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List-NonEmpty.html.
-- Use functions which work over `NonEmpty` by prefixing them with `NE.`, for example `NE.map`.
data Book = Book
  { title :: String,
    authors :: NE.NonEmpty Person,
    yearOfPublication :: Int,
    price :: Double
  }

instance Show Book where
  show (Book title authors year price) =
    title ++ "\n"
    ++ "\tAuthors:\n\t\t"
    ++ intercalate "\n\t\t" (NE.toList $ NE.map show authors) ++ "\n"
    ++ "\tPublished: " ++ show year ++ "\n"
    ++ "\tPrice: " ++ show price

-- The `type` keyword introduces a type alias.
-- `[Book]` and `Catalog` can be used interchangeably.
-- In error messages, you will only see `[Book]`
type Catalog = [Book]

-- We use `OVERLAPPING` here to notify GHC that it should use this instance of `Show` even if 
-- it sees other possibilities, including for a polymorphic list `[a]`. 
-- See: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#instance-overlap
instance {-# OVERLAPPING #-} Show Catalog where
  show bs = intercalate "\n" $ map show bs

-- Find all books which have been published before the given year
oldBooks :: Int -> Catalog -> Catalog
oldBooks maxYearOfPublication =
  filter $ \book -> yearOfPublication book < maxYearOfPublication

-- At least one of the authors should satisfy the predicate.
booksByAuthor :: (Person -> Bool) -> Catalog -> Catalog
booksByAuthor p =
  filter $ \book -> any p (NE.toList $ authors book)

-- Apply a given discount to the books which satisfy the predicate
discount :: Double -> (Book -> Bool) -> Catalog -> Catalog
discount rate p = 
  map $ \book -> if p book then book { price = price book * (1 - rate) } else book

sampleCatalog :: Catalog
sampleCatalog =
  [ Book
      { title = "Learn You a Haskell for Great Good!",
        authors = Person "Miran" "Lipovača" 1979 NE.:| [],
        yearOfPublication = 2011,
        price = 30.0
      },
    Book
      { title = "Real World Haskell",
        authors =
          Person "Bryan" "O'Sullivan" 1975
            NE.:| [ Person "John" "Goerzen" 1960,
                    Person "Don" "Stewart" 1950
                  ],
        yearOfPublication = 2008,
        price = 45.0
      },
    Book
      { title = "Haskell Programming from First Principles",
        authors =
          Person "Christopher" "Allen" 1985
            NE.:| [Person "Julie" "Moronuki" 1980],
        yearOfPublication = 2019,
        price = 50.0
      },
    Book
      { title = "Haskell in Depth",
        authors = Person "Vitaly" "Bragilevsky" 1988 NE.:| [],
        yearOfPublication = 2021,
        price = 55.0
      },
    Book
      { title = "Parallel and Concurrent Programming in Haskell",
        authors = Person "Simon" "Marlow" 1960 NE.:| [],
        yearOfPublication = 2013,
        price = 40.0
      }
  ]

main = do
  print sampleCatalog
  putStrLn "Old books:"
  print $ oldBooks 2015 sampleCatalog
  putStrLn "Books with young authors:"
  print $ booksByAuthor (\author -> yearOfBirth author > 1980) sampleCatalog
  putStrLn "40% off!"
  print $ discount 0.4 (const True) sampleCatalog
  print $ discount 0.99 (\book -> price book > 50) sampleCatalog
  print $ discount 0.4 (const True) $ oldBooks 2015 sampleCatalog
  print $ booksByAuthor (\author -> 'n' == last (firstName author) ) sampleCatalog
