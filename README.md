# HW02 

## Deadline 23:59 26.02.2025

Implement a book store catalog in [BookStore.hs](BookStore.hs). 

1. (2 points) Implement `Show` instances for every datatype. You can, for example, display the sample catalog as shown at the bottom of this document. 
2. (1 point) Implement `oldBooks` to filter books published after a certain date. 
3. (2 points) Implement `booksByAuthor` to filter books based on their authors. 
4. (2 points) Implement `discount` to apply a discount to books which satisfy a predicate. 
5. (1 point) Provide at least 2 other uses of these functions beside those in `main`. 

Try using higher-order functions whenever possible. Consult Hoogle if you don't immediately know which function to use. 

Sample catalog: 

```
Learn You a Haskell for Great Good!
        Authors:
                Miran Lipovača, born 1979
        Published: 2011
        Price: 30.0
Real World Haskell
        Authors:
                Bryan O'Sullivan, born 1975
                John Goerzen, born 1960
                Don Stewart, born 1950
        Published: 2008
        Price: 45.0
Haskell Programming from First Principles
        Authors:
                Christopher Allen, born 1985
                Julie Moronuki, born 1980
        Published: 2019
        Price: 50.0
Haskell in Depth
        Authors:
                Vitaly Bragilevsky, born 1988
        Published: 2021
        Price: 55.0
Parallel and Concurrent Programming in Haskell
        Authors:
                Simon Marlow, born 1960
        Published: 2013
        Price: 40.0
```