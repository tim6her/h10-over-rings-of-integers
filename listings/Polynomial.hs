{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import NumericPrelude

import qualified Monomial

import qualified Data.Map as Map
import Algebra.Monoid as Monoid
import Algebra.Ring as Ring
import Algebra.Module as Module
import Algebra.Additive as Additive

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- | Polynomials over a ring R are finitely supported functions
-- from the set of monomials to R
newtype Polynomial a = Polynomial (Map.Map Monomial.Monomial a)

-- | Polynomials form an additive (abelian) group
instance (Ring.C a, Eq a) => Additive.C (Polynomial a) where
  zero = Polynomial Map.empty
  (+) = padd
  negate (Polynomial m) = Polynomial $ Map.map negate m

-- | Polynomials from an R-module
instance (Ring.C a, Eq a) => Module.C a (Polynomial a) where
  (*>) 0 _ = zero
  (*>) a (Polynomial m) = Polynomial $ Map.map (a*) m

-- | Polynomials form a ring with unit
instance (Ring.C a, Eq a) => Ring.C (Polynomial a) where
  one = pfromList [(1, [])]
  (*) p@(Polynomial m1) q
    | p == zero = zero
    | q == zero = zero
    | p == one = q
    | q == one = p
    | otherwise = Map.foldrWithKey
                    (\mono coeff poly -> (coeff *> mono `mmul` q) + poly)
                    0 m1

-- | Two polynomials are equal if their difference is zero
instance (Ring.C a, Eq a) => Eq (Polynomial a) where
  (==) p q = let (Polynomial m) = p - q in m == Map.empty

instance (Show a, Eq a) => Show (Polynomial a) where
  show (Polynomial p)
    | p == Map.empty = "0"
    | otherwise = Map.foldrWithKey
                    (\m a sh -> show a ++ " " ++ show m ++ "+ " ++ sh)
                    "" p

-- | Adds two polynomials over the same ring
--
-- If a coefficient of a monoid equals 0 the monoid is dropped out of the map
padd :: (Ring.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
padd p@(Polynomial m1) q@(Polynomial m2)
  | m1 == Map.empty = q
  | m2 == Map.empty = p
  | otherwise = clean $ Polynomial $ Map.foldrWithKey
                  (\mono coeff poly -> if mono `Map.member` poly
                             then Map.insertWith (+) mono coeff poly
                             else Map.insert mono coeff poly)
                  m2 m1

mmul :: (Ring.C a, Eq a) => Monomial.Monomial -> Polynomial a -> Polynomial a
mmul mono poly@(Polynomial mp)
  | mono == Monomial.idt = Polynomial mp
  | poly == zero = zero
  | otherwise = Polynomial $ Map.mapKeys (mono Monomial.<*>) mp


-- | Generate polynomials from lists
pfromList :: (Ring.C a, Eq a) => [(a, [(Integer, Integer)])] -> Polynomial a
pfromList [] = zero
pfromList ((a, m):l) = deepClean . clean  $ (Polynomial $ Map.singleton
                         (Monomial.mfromList m) a) + pfromList l

-- | Comfort function for creating polynomials
--
-- === Example
-- >>> 2 *> ((x 1 + x 2) * (x 1 - x 2)) == 2 *> x 1 ^ 2 - 2 *> x 2 ^ 2
-- True
x :: (Ring.C a, Eq a) => Integer -> Polynomial a
x i = pfromList [(1, [(i, 1)])]

-- | Remove monoids with coefficient zero from support
clean :: (Ring.C a, Eq a) => Polynomial a -> Polynomial a
clean (Polynomial m) = Polynomial $ Map.foldrWithKey
                         (\mono coeff poly -> if coeff == 0
                                              then Map.delete mono poly
                                              else poly)
                          m m

-- | Remove variables with power zero from monomials
--
-- This function runs in O(n log(n)) so use it sparsely
deepClean :: (Ring.C a, Eq a) => Polynomial a -> Polynomial a
deepClean (Polynomial m) = Polynomial $ Map.mapKeys Monomial.clean m

-- * Testing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcAddProps, qcModProps,
               localOption (QuickCheckTests 5) qcRingProps,
               qcAlgebraProps]

qcAddProps = testGroup "Group axioms for addition"
  [ QC.testProperty "addition is commutative" $
    \x y -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
                 q = pfromList (y :: [(Int, [(Integer, Integer)])])
           in p + q == q + p)
  , QC.testProperty "addition is associative" $
    \x y z -> (let p1 = pfromList (x :: [(Int, [(Integer, Integer)])])
                   p2 = pfromList (y :: [(Int, [(Integer, Integer)])])
                   p3 = pfromList (z :: [(Int, [(Integer, Integer)])])
           in (p1 + p2) + p3 == p1 + (p2 + p3))
  , QC.testProperty "addition by zero" $
    \x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in p + zero == p)
  , QC.testProperty "addition with inverse" $
    \x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in p - p == zero)
  ]

qcModProps = testGroup "Module axioms"
  [ QC.testProperty "first distributive law" $
    \a x y -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
                   q = pfromList (y :: [(Int, [(Integer, Integer)])])
           in (a :: Int) *> (p + q) == a *> q + a *> p)
  , QC.testProperty "second distributive law" $
    \a b x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in (a + b :: Int) *> p == a *> p + b *> p)
  , QC.testProperty "multiplications commute" $
    \a b x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in (a * b :: Int) *> p == a *> (b *> p))
  , QC.testProperty "multiplication by one" $
    \x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in (one :: Int) *> p == p)
  ]

qcRingProps = testGroup "Ring axioms"
  [ QC.testProperty "multiplication is associative" $
    \x y z -> (let p1 = pfromList (x :: [(Int, [(Integer, Integer)])])
                   p2 = pfromList (y :: [(Int, [(Integer, Integer)])])
                   p3 = pfromList (z :: [(Int, [(Integer, Integer)])])
           in (p1 * p2) * p3 == p1 * (p2 * p3))
  , QC.testProperty "left multiplication by one" $
    \x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in one * p == p)
  , QC.testProperty "right multiplication by one" $
    \x -> (let p = pfromList (x :: [(Int, [(Integer, Integer)])])
           in p * one == p)
  , QC.testProperty "distributive law" $
    \x y z -> (let p1 = pfromList (x :: [(Int, [(Integer, Integer)])])
                   p2 = pfromList (y :: [(Int, [(Integer, Integer)])])
                   p3 = pfromList (z :: [(Int, [(Integer, Integer)])])
               in p1 * (p2 + p3) == p1 * p2 + p1 * p3)
  ]

qcAlgebraProps = testGroup "Algebra axioms"
  [ QC.testProperty "multiplications commute" $
    \x y a -> (let p1 = pfromList (x :: [(Int, [(Integer, Integer)])])
                   p2 = pfromList (y :: [(Int, [(Integer, Integer)])])
           in (a :: Int) *> (p1 * p2) == (a *> p1) * p2)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "sample polynomial" $
      show (pfromList [(1, [(1, 2), (3, 4)]), (-4, [(1, 4), (2, 3)])]
               :: Polynomial Int) @?= "1 X1^2 X3^4 + -4 X1^4 X2^3 + "
  , testCase "test equality" $
      pfromList [(0, [(1, 1)])] @?= (zero :: Polynomial Int)
  ]
