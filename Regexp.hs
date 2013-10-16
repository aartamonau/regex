{-# LANGUAGE PostfixOperators #-}

module Regexp
       ( Regexp ( Epsilon, Symbol, Alt, Seq, Kleene )
       , RegexpBackend ( compile, matches )
       , char, string, oneOf, times, between, betweenStupid, upto
       , (.|), (.&), (.*), (.+), (.?), (~=)

       -- examples
       , float, number, hex, oct
       )
       where

import Control.Exception ( assert )

data Regexp = Epsilon
            | Symbol Char
            | Alt Regexp Regexp
            | Seq Regexp Regexp
            | Kleene Regexp
            deriving Show

char :: Char -> Regexp
char = Symbol

string :: String -> Regexp
string = foldr1 Seq . map Symbol

oneOf :: String -> Regexp
oneOf = foldr1 Alt . map Symbol

times :: Regexp -> Int -> Regexp
times re n = foldr1 Seq $ replicate n re

between :: Regexp -> (Int, Int) -> Regexp
between re (m, n) =
  assert (m <= n) $
    times re m .& (iterate k Epsilon) !! (n - m)
  where k rest = (re .?) .& rest

betweenStupid :: Regexp -> (Int, Int) -> Regexp
betweenStupid re (m, n) =
  assert (m <= n) $
    times re m .& times (re .?) (n - m)

upto :: Regexp -> Int -> Regexp
upto re n = between re (0, n)

infixl 6 .|
infixl 7 .&

(.|) :: Regexp -> Regexp -> Regexp
re1 .| re2 = Alt re1 re2

(.&) :: Regexp -> Regexp -> Regexp
re1 .& re2 = Seq re1 re2

(.*) :: Regexp -> Regexp
(.*) re = Kleene re

(.+) :: Regexp -> Regexp
(.+) re = re .& (re .*)

(.?) :: Regexp -> Regexp
(.?) re = Epsilon .| re

class RegexpBackend b where
  compile :: Regexp -> b
  matches :: String -> b -> Bool

infixr 0 ~=
(~=) :: RegexpBackend b => b -> String -> Bool
backend ~= str = str `matches` backend

-- examples
digit :: Regexp
digit = oneOf ['0' .. '9']

number :: Regexp
number = (digit .+)

float :: Regexp
float =
  (digit .+) .& char '.' .& (digit .*)
  .|
  (digit .*) .& char '.' .& (digit .+)

octDigit :: Regexp
octDigit = oneOf ['0' .. '7']

oct :: Regexp
oct = char '0' .& (octDigit .+)

hexDigit :: Regexp
hexDigit = oneOf ['a' .. 'f'] .| oneOf ['A' .. 'F'] .| digit

hex :: Regexp
hex = string "0x" .& (hexDigit .+)
