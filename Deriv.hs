module Deriv
       where

import Control.Applicative ( pure, liftA2 )

import Regexp ( Regexp ( Epsilon, Symbol, Alt, Seq, Kleene )
              , RegexpBackend ( compile, matches )
              )

newtype Deriv = Deriv { unDeriv :: Regexp }

nullable :: Regexp -> Bool
nullable Epsilon       = True
nullable (Symbol _)    = False
nullable (Alt re1 re2) = nullable re1 || nullable re2
nullable (Seq re1 re2) = nullable re1 && nullable re2
nullable (Kleene _)    = True

deriv :: Char -> Regexp -> Maybe Regexp
deriv _ Epsilon = Nothing
deriv c (Symbol s)
  | c == s    = Just Epsilon
  | otherwise = Nothing
deriv c (Alt re1 re2) = liftA2 Alt (deriv c re1) (deriv c re2)
deriv c (Seq re1 re2)
  | nullable re1 = deriv c re2
  | otherwise    = liftA2 Seq (deriv c re1) (pure re2)
deriv c (Kleene re) = liftA2 Seq (deriv c re) (pure $ Kleene re)

derivMatches :: String -> Regexp -> Bool
derivMatches "" re = nullable re
derivMatches (c:cs) re | Just re' <- deriv c re = derivMatches cs re'
                       | otherwise = False

instance RegexpBackend Deriv where
  compile = Deriv
  matches str = derivMatches str . unDeriv
