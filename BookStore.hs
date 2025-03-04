module BookStore where

import Data.List ( intercalate, isInfixOf )
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
  show b =
    title b
      ++ "\n\tAuthors:\n"
      ++ concatMap (\a -> "\t\t" ++ show (firstName a) ++ " " ++ show (lastName a) ++ ", born " ++ show (yearOfBirth a) ++ "\n") (NE.toList $ authors b)
      ++ "\tPublished: "
      ++ show (yearOfPublication b)
      ++ "\n\tPrice: "
      ++ show (price b)

-- The `type` keyword introduces a type alias.
-- `[Book]` and `Catalog` can be used interchangeably.
-- In error messages, you will only see `[Book]`
type Catalog = [Book]

-- We use `OVERLAPPING` here to notify GHC that it should use this instance of `Show` even if
-- it sees other possibilities, including for a polymorphic list `[a]`.
-- See: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#instance-overlap
instance {-# OVERLAPPING #-} Show Catalog where
  show = concatMap (\book -> show book ++ "\n")

-- Find all books which have been published before the given year
oldBooks :: Int -> Catalog -> Catalog
oldBooks maxYearOfPublication = filter (\book -> yearOfPublication book < maxYearOfPublication)

-- At least one of the authors should satisfy the predicate.
booksByAuthor :: (Person -> Bool) -> Catalog -> Catalog
booksByAuthor p = filter (any p . authors)

-- Apply a given discount to the books which satisfy the predicate
discount :: Double -> (Book -> Bool) -> Catalog -> Catalog
discount rate p = map (\book -> if p book then book {price = price book * (1 - rate)} else book)

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

booksByOldAuthors :: Int -> Int -> Catalog -> Catalog
booksByOldAuthors maxYearOfBirth minYearOfPublication = filter (\book -> any (\author -> yearOfBirth author <= maxYearOfBirth) (authors book) && yearOfPublication book > minYearOfPublication)


-- Filter books that contain a specific string in their title
booksAbout :: String -> Catalog -> Catalog
booksAbout keyword = filter (\book -> keyword `isInfixOf` title book)

discountOldBooks :: Double -> Int -> Catalog -> Catalog
discountOldBooks rate year = discount rate (\book -> yearOfPublication book < year)

findBooksByAuthor :: String -> String -> Catalog -> Catalog
findBooksByAuthor fName lName = booksByAuthor (\author -> firstName author == fName && lastName author == lName)


main = do
  print sampleCatalog
  putStrLn "Old books:"
  print $ oldBooks 2015 sampleCatalog
  putStrLn "Books with young authors:"
  print $ booksByAuthor (\author -> yearOfBirth author > 1980) sampleCatalog
  putStrLn "40% off!"
  print $ discount 0.4 (const True) sampleCatalog
  putStrLn "Books by old authors and published after the year 2015:"
  print $ booksByOldAuthors 1980 2015 sampleCatalog
  putStrLn "Books about Haskell:"
  print $ booksAbout "Haskell" sampleCatalog
  putStrLn "30% discount on books published before 2010:"
  print $ discountOldBooks 0.3 2010 sampleCatalog
  putStrLn "Books by Miran Lipovača:"
  print $ findBooksByAuthor "Miran" "Lipovača" sampleCatalog
