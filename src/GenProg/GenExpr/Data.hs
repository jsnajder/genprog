-- |
-- Module      :  GenProg.GenExpr.Data
-- Copyright   :  (c) 2010 Jan Snajder
-- License     :  BSD-3 (see the LICENSE file)
--
-- Maintainer  :  Jan Snajder <jan.snajder@fer.hr>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implementation of the @GenProg.GenExpr@ interface for members of
-- the 'Data' typeclass. The implementation is based on SYB and SYZ
-- generic programming frameworks (see
-- <http://hackage.haskell.org/package/syb> and
-- <http://hackage.haskell.org/package/syz> for details).
--
-- NB: Subexpressions that are candidates for crossover points or
-- mutation must be of the same type as the expression itself, and
-- must be reachable from the root node by type-preserving traversal.
-- See below for an example.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, Rank2Types,
    UndecidableInstances, DeriveDataTypeable #-}

module GenProg.GenExpr.Data (
  -- | This module re-exports @GenExpr@ typeclass.
  GenExpr (..)
  -- * Example
  -- $Example
  ) where

import Data.Generics
import Data.Generics.Zipper
import Data.Maybe
import Control.Monad
import GenProg.GenExpr

moduleName = "GenProg.GenExpr.Data"

instance (Data a) => GenExpr a where

  -- | Exchanges two expression nodes. Works by using two generic
  -- zippers and exchanging their holes.
  exchange e1 n1 e2 n2 = (fromZipper y1, fromZipper y2)
    where z1 = typeMoveForUnsafe n1 $ toZipper e1
          z2 = typeMoveForUnsafe n2 $ toZipper e2
          (y1,y2) = exchangeHoles z1 z2

  -- | Adjust an expression node. Works by applying a monadic
  -- tranformation on a zipper hole.
  adjustM f e n = fromZipper `liftM` transM (mkM f) z
    where z = typeMoveForUnsafe n (toZipper e)

  nodeMapM f = gmapM (mkM f)

  nodeMapQ q (x::a) = concat $ gmapQ ([] `mkQ` (\(y::a) -> [q y])) x

  nodeIndices = index 0 [] [] . toZipper

-- Zipper moves

type Move a = Zipper a -> Maybe (Zipper a)

backtrack :: (Typeable a) => Move a
backtrack z = do
  z2 <- up z
  right z2 `mplus` backtrack z2

repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM 0 _ x = return x
repeatM n f x = f x >>= repeatM (n - 1) f

-- Moves zipper to next node in DFS order, but does not move down the
-- zipper if node satisfies query 'q'.
nextDfsQ :: Typeable a => GenericQ Bool -> Move a
nextDfsQ q z = (if query q z then Nothing else down' z)
  `mplus` right z `mplus` backtrack z

-- Moves the zipper to node 'n' from current position in DFS order,
-- skipping nodes not satisfying query 'q2' and descending only down
-- the nodes satisfying query 'q1'.
moveForQ :: (Typeable a) => GenericQ Bool -> GenericQ Bool -> Int -> Move a
moveForQ _  _  0 z = Just z
moveForQ q1 q2 n z = do
  z2 <- nextDfsQ q1 z
  moveForQ q1 q2 (if query q2 z2 then n - 1 else n) z2

-- Moves the zipper to node 'n' from current position in DFS order,
-- counting only nodes of type 'a', and not descending down the nodes
-- of other type.
typeMoveFor :: (Typeable a) => Int -> Move a
typeMoveFor n (z::Zipper a) =
  moveForQ (True `mkQ` (\(_::a) -> False)) (False `mkQ` (\(_::a) -> True)) n z

-- | Same as typeMoveFor, but throws an error if node index is out of
-- bound.
typeMoveForUnsafe :: (Typeable a) => Int -> Zipper a -> Zipper a
typeMoveForUnsafe n z = fromMaybe
  (error $ moduleName ++ ".typeMoveForUnsafe: Nonexisting node.")
  (typeMoveFor n z)

-- | Exchanges two zipper holes.
exchangeHoles :: (Data a) => Zipper a -> Zipper a -> (Zipper a, Zipper a)
exchangeHoles (z1::Zipper a) (z2::Zipper a) = (y1,y2)
  where Just h1 = getHole z1 :: Maybe a
        Just h2 = getHole z2 :: Maybe a
        y1 = setHole h2 z1
        y2 = setHole h1 z2

index :: (Data a) => Int -> [Int] -> [Int] -> Zipper a -> ([Int], [Int])
index i is es (z :: Zipper a) =
  maybe (is2,es2) (index (i + 1) is2 es2) (typeMoveFor 1 z)
  where Just h = getHole z :: Maybe a
        (is2,es2) = if terminalQ h then (is,i:es) else (i:is,es)

terminalQ :: (Data a) => a -> Bool
terminalQ = null . nodeMapQ id

{- $Example

Suppose you have a datatype defined as

@
data E = A E E
       | B String [E]
       | C
 deriving (Eq,Show,Typeable,Data)
@

and an expression defined as

@
e = A (A C C) (B \"abc\" [C,C])
@

The subexpressions of a @e@ are considered to be only the subvalues of
@e@ that are of the same type as @e@.  Thus, the number of nodes of
expression @e@ is

>>> nodes e
5
 
because subvalues of node @B@ are of different type than expression
@e@ and therefore not considered as subexpressions. 

Consequently, during a genetic programming run, subexpressions that
are of a different type than the expression itself, or subexpression
that cannot be reached from the root node by a type-preserving
traversal, cannot be chosen as crossover points nor can they be
mutated.

-}
