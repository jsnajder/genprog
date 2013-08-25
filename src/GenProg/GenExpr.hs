-- |
-- Module      :  GenProg.GenExpr
-- Copyright   :  (c) 2010 Jan Snajder
-- License     :  BSD-3 (see the LICENSE file)
--
-- Maintainer  :  Jan Snajder <jan.snajder@fer.hr>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An interface to genetically programmable expressions.
--
-----------------------------------------------------------------------------

module GenProg.GenExpr (
  GenExpr (..)) where

import Control.Monad

-- | This typeclass defines an interface to expressions
-- that can be genetically programmed.  The operations that must be
-- provided by instances of this class are used for the generation
-- of random individuals as well as crossover and mutation operations.
-- (An instance for members of the @Data@ typeclass is provided in
-- "GenProg.GenExpr.Data".)
--
-- Minimal complete definition: 'exchange', 'nodeMapM', 'nodeMapQ',
-- and 'nodeIndices'.
class GenExpr e where
  -- | Exchanges subtrees of two expressions:
  -- @exchange e1 n1 e2 n2@ replaces the subexpression of @e1@ rooted in node
  -- @n1@ with the subexpression of @e2@ rooted in @n2@, and vice versa.
  exchange :: e -> Int -> e -> Int -> (e, e)
  -- | Maps a monadic transformation function over the immediate
  -- children of the given node.
  nodeMapM :: Monad m => (e -> m e) -> e -> m e
  -- | Maps a query function over the immediate children of the given
  -- node and returns a list of results.
  nodeMapQ :: (e -> a) -> e -> [a]
  -- | A list of indices of internal (functional) and external
  -- (terminal) nodes of an expression.
  nodeIndices :: e -> ([Int], [Int])
  -- | Adjusts a subexpression rooted at the given node by applying a
  -- monadic transformation function.
  adjustM :: (Monad m) => (e -> m e) -> e -> Int -> m e
  -- | Number of nodes an expression has.
  nodes :: e -> Int
  -- | The depth of an expression. Equals 1 for single-node expressions.
  depth :: e -> Int


  -- | Default method (expensive because it calls exchange twice).
  adjustM f e n = replace e n `liftM` f (get e n)
    where get e n = fst $ exchange e 0 e n
          replace e1 n1 e2 = fst $ exchange e1 n1 e2 0

  nodes = (+1) . foldr (+) 0 . nodeMapQ nodes 

  depth = (+1) . foldr max 0 . nodeMapQ depth
 
