module Primes
    ( isPrime
    , primes
    , primeFactors
    ) where

import Primes.Types

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000]


isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n >= length primes = Left TooLarge
  | otherwise = Right (n `elem` primes)


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                       then next:unsafePrimeFactors
                                            (n `div` next) (next:primes)
                                       else unsafePrimeFactors n primes


primeFactors :: Int -> Either PrimeError [Int]
primeFactors n
  | n < 2 = Left InvalidValue
  | n >= length primes = Left TooLarge
  | otherwise = Right (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes
