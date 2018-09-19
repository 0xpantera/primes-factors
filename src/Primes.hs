module Primes
    ( isPrime
    , primes
    , primeFactors
    ) where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000]


isPrime :: Int -> Either String Bool
isPrime n
  | n < 2 = Left "Numbers less than 2 are not candidates for primes"
  | n >= length primes = Left "Value exceeds limits of prime checker"
  | otherwise = Right (n `elem` primes)


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                       then next:unsafePrimeFactors
                                            (n `div` next) (next:primes)
                                       else unsafePrimeFactors n primes


primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes
  
