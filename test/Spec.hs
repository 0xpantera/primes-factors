import Primes.Types
import Data.Either
import Test.QuickCheck
import Primes


--prop_validPrimesOnlyOUT val = if val < 2 || val >= length primes
--                             then result == PrimeError
--                             else isRight result
--  where result = isPrime val

prop_validPrimesOnly val
  | val < 2 = isLeft result
  | val >= length primes = isLeft result
  | otherwise = isRight result
  where result = isPrime val

prop_primesArePrime val = if fromRight False result
                            then length divisors == 0
                            else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]


prop_nonPrimesAreComposite val = if fromRight True result == False
                                    then length divisors > 0
                                    else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]
        

prop_factorsMakeOriginal val = if isLeft result 
                                  then True
                                  else product (fromRight [0] result) == val
  where result = primeFactors val


prop_allFactorsPrime val = if isLeft result
                             then True
                             else all (isRight) resultsPrime
  where result = primeFactors val
        resultsPrime = map isPrime (fromRight [0] result)
        

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
