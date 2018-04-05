module Monomial
( Monomial
, mmul
, mfromList
) where

import Data.Map (Map, empty, foldrWithKey, fromList, member, insert,
                 insertWith, (!))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC


-- | Monomials are mappings from ZZ to ZZ with finite support
newtype Monomial = Monomial (Map Int Int) deriving (Eq)

instance Show Monomial where
  show (Monomial m)
    | m == empty = "1"
    | otherwise =  foldrWithKey
                     (\x e sh -> "X" ++ show x ++ "^" ++ show e ++ " " ++ sh)
                      "" m

-- | Creates monomials from list of tuples
--
-- Left entry is index of indeterminate, right index is power of the
-- indeterminate
--
-- === Example
-- >>> mfromList [(1, 2), (0, 3), (4, 7)]
-- X0^3 X1^2 X4^7
mfromList :: [(Int, Int)] -> Monomial
mfromList l = Monomial $ fromList l

-- | Multiplies two monomials
--
-- === Example
-- >>> mmul (mfromList [(1, 2), (2, 4)]) (mfromList [(2, 1), (3, 2)])
-- X1^2 X2^5 X3^2
mmul :: Monomial -> Monomial -> Monomial
mmul xx@(Monomial m1) yy@(Monomial m2)
  | m1 == empty = yy
  | m2 == empty = xx
  | otherwise = Monomial $ foldrWithKey
                  (\x e m -> if x `member` m
                             then insertWith (+) x e m
                             else insert x e m)
                  m2 m1

instance Monoid Monomial where
  mempty = Monomial empty
  mappend = mmul

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "left multiplication by identity" $
    \x -> (let m = mfromList (x :: [(Int, Int)])
           in mempty `mappend` m == m)
  , QC.testProperty "right multiplication by identity" $
    \x -> (let m = mfromList (x :: [(Int, Int)])
           in m `mmul` mempty == m)
  , QC.testProperty "associativity" $
    \x y z -> (let m1 = mfromList (x :: [(Int, Int)])
                   m2 = mfromList (y :: [(Int, Int)])
                   m3 = mfromList (z :: [(Int, Int)])
           in (m1 `mmul` m2) `mmul` m3 == m1 `mmul` (m2 `mmul` m3))
  ]

unitTests = testGroup "Unit tests"
  [ testCase "show X0^3 X1^2 X4^7" $
      show (mfromList [(1, 2), (0, 3), (4, 7)]) @?= "X0^3 X1^2 X4^7 "
  , testCase "sample multiplication" $
      show (mmul (mfromList [(1, 2), (2, 4)]) (mfromList [(2, 1), (3, 2)])) @?=
      "X1^2 X2^5 X3^2 "
  ]
