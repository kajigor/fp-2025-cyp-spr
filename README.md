# HW05

## Deadline 23:59 2.04.2025

1. (4 points) Implement a parser for the statement language from HW04.
    * I would recommend using [megaparsec](https://hackage.haskell.org/package/megaparsec), read this [tutorial](https://markkarpov.com/tutorial/megaparsec.html) to learn how to. 
2. (2 points) Write tests. 
3. (2 points) Write a console application which asks the user a path to the file with a program in the statement language, parses it, evaluates it, and prints out the result. 


# HW06

## Deadline 23:59 16.04.2025

1. (2 points) Come up with at least 4 different optimisations for our statement language. Describe and implement the optimizations.
     * The optimizations can be at the expression level, such as `0 * x ==> 0`, or at the statement level, such as loop unrolling.
     * Be creative. 
3. (4 points) Using property-based testing demostrate properties of your optimisations. For example, you can show that they do not change the semantics of the program or that they improve code in some way. 
     * [This](https://www.youtube.com/watch?v=G0NUOst-53U) is a great introduction. 
     * I would recommend using the [hedgehog](https://hackage.haskell.org/package/hedgehog) package, but you can also use the OG [quickcheck](https://hackage.haskell.org/package/QuickCheck) package. 
4. (2 points) Implement a console application which allows different operational modes with programs in the statement language. Your application should allow at least two scenarios: running unoptimized or optimized program. Come up with at least two additional modes, implement them.
     * I would recommend using the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) package. The [readme](https://github.com/pcapriotti/optparse-applicative) on their github account contain some great examples. 
