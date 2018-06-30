module Example where

import FST
import Combinators
import Regex
import Types

import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import Data.String (fromString)


type FSTRegex = Regex (Input, Output)

digits :: [BS.ByteString]
digits = map (fromString . show) [1..9 :: Int]

zero :: FSTRegex
zero = Word ("", "0")

ones, ones0 :: FSTRegex
ones = fromList $ zip ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] digits
ones0 = zero `Union` ones

tens, tens0 :: FSTRegex
tens = fromList $ zip ["X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] digits
tens0 = zero `Union` tens

hundreds, hundreds0 :: FSTRegex
hundreds = fromList $ zip ["C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"] digits
hundreds0 = zero `Union` hundreds

thousands, thousands0 :: FSTRegex
thousands = fromList $ zip ["M", "MM", "MMM"] digits
thousands0 = zero `Union` thousands

n :: FSTRegex
n = unions
  [ concatenates [thousands, hundreds0, tens0, ones0]
  , concatenates [hundreds, tens0, ones0]
  , concatenates [tens, ones0]
  , ones
  ]

n1 :: FSTRegex
n1 = concatenates [Word (" ", " "), n, Word (".", ".")]

nFST :: FST
nFST = fromJust $ toFSM n

n1FST :: FST
n1FST = fromJust $ toFSM n1

rFST :: FST
rFST = replace alphabet nFST

r1FST :: FST
r1FST = replace alphabet n1FST

text :: IO Input
text = BS.readFile "text"
