module Primes.Types where

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Valuee exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime chcker"

  
