-- |
-- Module      :  GenProg
-- Copyright   :  (c) 2010 Jan Snajder
-- License     :  BSD-3 (see the LICENSE file)
--
-- Maintainer  :  Jan Snajder <jan.snajder@fer.hr>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The Genetic Programming Library.
--
-- /Genetic programming/ is an evolutionary optimization technique
-- inspired by biological evolution. It is similar to /genetic algorithms/
-- except that the individual solutions are programs (or, more generally, 
-- /expressions/) representing a solution to a given problem. A genetic 
-- program is represented as an /abstract syntax tree/ and associated 
-- with a custom-defined /fitness/ value indicating the quality of the 
-- solution. Starting from a randomly generated initial population of 
-- genetic programs, the genetic operators of /selection/, /crossover/, 
-- and (occasionally) /mutation/ are used to evolve programs of 
-- increasingly better quality.
--
-- Standard reference is: John Koza. /Genetic programming:/
-- /On the Programming of Computers by Means of Natural Selection/.
-- MIT Press, 1992.
--
-- In GenProg, a genetic program is represented by a value of an
-- algebraic datatype. To use a datatype as a genetic program, it
-- suffices to define it as an instance of the 'GenProg' typeclass.
-- A custom datatype can be made an instance of the 'GenProg'
-- typeclass, provided it is an instance of the 'Data' typeclass (see
-- "GenProg.GenExpr.Data").
--
-- An example of how to use this library is given below.
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    NoMonomorphismRestriction #-}

module GenProg (
  -- * Genetic programs
  GenProg (..),
  -- * Expressions
  generateFullExpr,
  generateGrownExpr,
  depth,
  nodes,
  -- * Individuals
  Ind,
  unInd,
  mkInd,
  aFitness,
  sFitness,
  -- * Population
  Pop,
  unPop,
  mkPop,
  generatePop,
  replenishPop,
  mergePop,
  best,
  avgFitness,
  avgDepth,
  avgNodes,
  -- * Genetic operators
  -- | The following functions are not meant to be used directly.
  -- They are exposed for debugging purposes.
  crossoverInd,
  mutateInd,
  crossoverPop,
  mutatePop,
  -- * Evolution state
  EvolState (..),
  -- * Control parameters
  Fitness,
  Mutate,
  defaultMutation,
  Terminate,
  tSuccess,
  tFitness,
  tGeneration,
  EvolParams (..),
  defaultEvolParams,
  -- * Evolution
  evolve,
  evolveFrom,
  evolveTrace,
  evolveTraceFrom
  -- * Example
  -- $Example
  ) where

import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Control.Monad.Random
import GenProg.GenExpr.Data

-- | A typeclass defining a genetic program interface.  Datatypes @e@
-- that are to be used as genetic programs must be instances of the
-- 'GenExpr' typeclass and must implement this interface. 
class (Eq e, GenExpr e, MonadRandom m) => GenProg m e | e -> m where
  -- | Generates a random terminal @T@.
  terminal :: m e
  -- | Generates a random nonterminal (functional) node @F(T,...,T)@ whose
  -- arguments are again terminals (this condition is not verified).
  nonterminal :: m e

-----------------------------------------------------------------------------
-- Expressions

-- | Generates a random expression of a given maximum depth.
generateExpr :: (GenProg m e) => m e -> Int -> m e
generateExpr g d
  | d < 1     = error "GenProg.generateExpr: Invalid expression depth"
  | otherwise = nonterminal >>= step (d - 1)
  where step 0 _ = terminal
        step d e = nodeMapM (const g >=> step (d - 1)) e

-- | Generates a random expression fully expanded to the specified depth.
generateFullExpr :: (GenProg m e) => Int -> m e
generateFullExpr = generateExpr nonterminal

-- | Generates a random expression of limited depth. The maximum depth of
-- the resulting expression may be less than the specified depth
-- limit, and paths may be of different length.
generateGrownExpr :: (GenProg m e) => Int -> m e
generateGrownExpr d = do
  t <- getRandom
  generateExpr (if t then terminal else nonterminal) d

-----------------------------------------------------------------------------
-- Individuals

-- | A genetically programmed individual, representing a basic unit
-- of evolution. (Basically a wrapper around a genetically programmable
-- expression.)
data Ind e = Ind {
  -- | Returns the expression wrapped by an individual.
  unInd :: e,
  -- | Adjusted fitness of an individual. Adjusted fitness equals
  -- @1/(1+s)@, where @s@ is the standardized fitness as computed by
  -- 'fitness'. To reduce computational costs, this value is computed
  -- only once and then cached.
  aFitness :: Double,
  -- The indices of inner (functional) nodes of an individual's expression.
  iNodes :: [Int],
  -- The indices of external (terminal) nodes of an individual's expression.
  eNodes :: [Int] }
  deriving (Show)

instance (Eq e) => Eq (Ind e) where
  i1 == i2 = unInd i1 == unInd i2

instance (Eq e) => Ord (Ind e) where
  compare = comparing aFitness

-- | Wraps an expression into an individual.
mkInd :: (GenProg m e) => Fitness e -> e -> Ind e
mkInd f e = Ind e (adjust $ f e) fs ts
  where (fs,ts) = nodeIndices e

-- Adjusts fitness.
adjust :: Double -> Double
adjust f = 1 / (1 + max 0 f)

-- Unadjusts fitness (the inverse of adjustFitness).
unadjust :: Double -> Double
unadjust f = 1 / f - 1

-- | Standardized fitness of an individual as computed by 'fitness'
sFitness :: Ind e -> Double
sFitness = unadjust . aFitness

-----------------------------------------------------------------------------
-- Population

-- | A population of individuals. (Basically a wrapper around a list of
-- individuals.)
data Pop e = Pop
  { unPop  :: [Ind e]   -- ^ Unwraps a population.
  , dist_  :: [Double]  -- ^ Fitness distribution.
  } deriving (Show, Eq)

-- | Wraps a list of individuals into a population.
mkPop :: [Ind e] -> Pop e
mkPop is = Pop is ds
  where ds = map snd . distribution $
             map (\i -> (unInd i, aFitness i)) is

-- | Generate population of given size and given depth limit using
-- /ramped half-and-half/ method (Koza, 1992): for each depth value from 0 to
-- the initial depth limit 'iDepth', 50% of individuals are generated using
-- 'generateFullExpr' and 50% are generated using
-- 'generateGrownExpr'. Afterwards, duplicates are removed, thus the
-- size of the resulting population may actually be less than the
-- specified size.
generatePop :: (GenProg m e) => EvolParams m e -> m (Pop e)
generatePop p
  | s < 2 || n==0 = error "GenProg.generatePop: Invalid population size"
  | otherwise = do
    iss <- forM [2..di] $ \i -> do
      is1 <- replicateM n (mkInd (fitness p) `liftM` generateFullExpr di)
      is2 <- replicateM n (mkInd (fitness p) `liftM` generateGrownExpr di)
      return $ is1 ++ is2
    return . mkPop . nub $ concat iss
  where n  = s `div` (2 * (di - 1))
        s  = popSize p
        di = iDepth p

-- | Replenishes a population up to 'popSize' by randomly
-- generating new individuals.
replenishPop :: (GenProg m e) => EvolParams m e -> Pop e -> m (Pop e)
replenishPop p pop1 = do
  pop2 <- generatePop p
  return . mkPop $ unPop pop1 ++ drop s (unPop pop2)
  where s = length $ unPop pop1

-- | Merges two populations by taking 'popSize' best-fitted individuals
-- from the union of the two populations.
mergePop :: (GenProg m e) => EvolParams m e -> Pop e -> Pop e -> Pop e
mergePop p pop1 pop2 = mkPop $ take (popSize p) is
  where is = sortBy (flip $ comparing aFitness) $ unPop pop1 ++ unPop pop2

-- | Population's best-fitted individual.
best :: Pop e -> Ind e
best = maximumBy (comparing aFitness) . unPop

avg :: (Fractional a) => [a] -> a
avg xs = sum xs / realToFrac n
  where n = length xs

-- | Population's average standardized fitness.
avgFitness :: Pop e -> Double
avgFitness = avg . map (unadjust . aFitness) . unPop

-- | Average depth of expressions in the population.
avgDepth :: (GenProg m e) => Pop e -> Double
avgDepth = avg . map (realToFrac . depth . unInd) . unPop

-- | Average number of expression nodes in the population.
avgNodes :: (GenProg m e) => Pop e -> Double
avgNodes = avg . map (realToFrac . nodes . unInd) . unPop

-----------------------------------------------------------------------------
-- Genetic operators

-- Selects at random an index of an expression node. Functional
-- (internal) nodes are selected with probability 'pci', whereas
-- terminal (external) nodes are selecred with probability '1-pi'.
selectNode :: (GenProg m e, MonadRandom m) => Double -> Ind e -> m Int
selectNode pi i
  | null $ iNodes i = oneof $ eNodes i
  | otherwise       = choice pi (oneof $ iNodes i) (oneof $ eNodes i)

-- | Crossover operation of two individuals, resulting in two
-- offsprings. Crossover is performed by choosing at random two nodes
-- in each expressions, and then by exchanging the subexpressions
-- rooted at these nodes between the two individuals. The probability
-- that an internal (functional) node is chosen as crossover point is
-- set by the 'ciProb' parameter in 'EvolParams', whereas the
-- probability that an external (terminal) node is chosen equals
-- @1-ciProb@. Among internal and external nodes, nodes are chosen
-- uniformly at random. If the depth of a created offspring exceeds
-- the depth limit 'cDepth' specified by evolution parameters
-- 'EvolParams', that offspring is discarded and a parent is
-- reproduced (i.e., copied as-is).
crossoverInd :: (GenProg m e) =>
  EvolParams m e -> Ind e -> Ind e -> m (Ind e, Ind e)
crossoverInd p i1 i2 = do
  n1 <- selectNode (ciProb p) i1
  n2 <- selectNode (ciProb p) i2
  let (r1,r2) = exchange (unInd i1) n1 (unInd i2) n2
  return (if depth r1 <= cDepth p then mkInd (fitness p) r1 else i1,
          if depth r2 <= cDepth p then mkInd (fitness p) r2 else i2)

-- | Mutates an individual by applying the mutation function @mutate@
-- to a randomly selected node. The probability that an internal
-- (functional) node is chosen for muration is set by the 'miProb'
-- parameter in 'EvolParams', whereas the probability that an external
-- (terminal) node is chosen equals @1-miProb@. Among internal and
-- external nodes, nodes are chosen uniformly at random. If the depth
-- of the mutated expression exceeds the depth limit 'cDepth'
-- specified by evolution parameters 'EvolParams', the individual is
-- left unaltered.
mutateInd :: (GenProg m e) => EvolParams m e -> Ind e -> m (Ind e)
mutateInd p i = do
  n  <- selectNode (miProb p) i
  e2 <- adjustM (mutate p) e1 n
  return . mkInd (fitness p) $ if depth e2 <= cDepth p then e2 else e1
  where e1 = unInd i

-- Discrete distribution.
type Distribution a = [(a, Double)]

-- Computes distribution from a weighted list.
-- The weights need not sum to 1.
distribution :: [(a, Double)] -> Distribution a
distribution xs = [(x,f i) | ((x,_),i) <- zip xs [1..]]
  where f i = sum . map snd $ take i ys
        s   = sum $ map snd xs
        ys  = map (\(x, w) -> (x, w/s)) xs

-- Samples a value from a discrete distribution.
choose :: (MonadRandom m) => Distribution a -> m a
choose xs = do
  p <- getRandomR (0,1)
  return . fst . fromJust $ find ((>= p) . snd) xs

-- Chose first action with probability 'p' and second with probability
-- 1-p.
choice :: (MonadRandom m) => Double -> m a -> m a -> m a
choice p a1 a2 = do
  r <- getRandomR (0,1)
  if r <= p then a1 else a2

oneof :: (MonadRandom m) => [a] -> m a
oneof xs = (xs!!) `liftM` getRandomR (0,length xs-1)

-- Fitness-proportionate selection of an individual from a population.
selectInd :: (MonadRandom m) => Pop e -> m (Ind e)
selectInd pop = choose (zip (unPop pop) (dist_ pop))

reproducePop :: (MonadRandom m) => Pop e -> m (Ind e)
reproducePop = selectInd

-- | Applies crossover to two randomly chosen individuals from a
-- population. The probability of an individual being chosen as parent
-- is fitness-proportionate (individuals with better fitness have
-- better chanches of being chosen for crossover).
crossoverPop :: (GenProg m e) => EvolParams m e -> Pop e -> m (Ind e,Ind e)
crossoverPop p pop = do
  i1 <- selectInd pop
  i2 <- selectInd pop
  crossoverInd p i1 i2

-- | Applies mutation operation to individuals from a population. The
-- probability of mutating each individual is determined by 'mProb' parameter
-- from 'EvalParams'.
mutatePop :: (GenProg m e) => EvolParams m e -> Pop e -> m (Pop e)
mutatePop p pop
  | mProb p == 0 = return pop
  | otherwise    = liftM mkPop . forM (unPop pop) $ \i ->
                     choice (mProb p) (mutateInd p i) (return i)

-----------------------------------------------------------------------------
-- Evolution state

-- | The state of the evolution.
data EvolState e = EvolState
  { pop        :: Pop e    -- ^ Current population.
  , iter       :: Int      -- ^ Iteration (current generation number).
  , cachedBest :: Ind e    -- ^ Best individual evolved so far.
  } deriving (Show,Eq)

initState :: Pop e -> EvolState e
initState pop =
  EvolState { pop = pop, iter = 0, cachedBest = best pop }

-- | Advances to next evolution state.
nextState :: (GenProg m e ) =>
  EvolParams m e -> EvolState e -> m (EvolState e)
nextState p es1 = do
  pop2 <- evolvePop p pop1
  return $ es1 { pop = pop2, iter = iter es1 + 1,
                 cachedBest = max (cachedBest es1) (best pop1) }
  where pop1 = pop es1

-----------------------------------------------------------------------------
-- Control parameters

-- | Standardized fitness. It takes on values from 0 (best fitness) to
-- +infinity (worst fitness).
type Fitness e = e -> Double

-- | A function to mutate a chosen expression node.
type Mutate m e = e -> m e

-- | Default mutation. Replaces a node, irrespective of its value,
-- with a randomly generated subexpression whose depth is limited to
-- 'iDepth'.
defaultMutation :: (GenProg m e) => EvolParams m e -> Mutate m e
defaultMutation p = const $ generateGrownExpr (iDepth p)

-- | Termination predicate.
type Terminate e = EvolState e -> Bool

-- | Termination predicate: terminate if any individual satisfies the
-- specified predicate.
tSuccess :: (e -> Bool) -> Terminate e
tSuccess c = any (c . unInd) . unPop . pop

-- | Termination predicate: terminate if best individual's
-- standardized fitness is greater than or equal to the specified value.
tFitness :: (GenProg m e) => Double -> Terminate e
tFitness f = (>= f) . unadjust . aFitness . cachedBest

-- | Termination predicate: terminate after running for the specified
-- number of iterations.
tGeneration :: Int -> Terminate e
tGeneration n = (>=n) . iter

-- | Parameters governing the evolution.
--
-- Default evolution parameters,
-- as used in (Koza, 1992), are defined by 'defaultEvolParams'
-- and indicated below. At least the fitness function 'fitness' should
-- be overriden.
data EvolParams m e = EvolParams {
  -- | Population size (number of individuals). Default is @500@.
  popSize   :: Int,
  -- | Depth of expressions in initial population. Default is @6@.
  iDepth    :: Int,
  -- | Maximum depth of expressions created during the evolution.
  -- Default is @17@.
  cDepth    :: Int,
  -- | Probability of crossover. Default is @0.9@. If crossover is not
  -- chosen, an individual is simply reproduced (copied as-is) into
  -- the next generation.
  cProb     :: Double,
  -- | Probability that an internal (functional) node is chosen as a
  -- crossover point. Default is @0.9@. If an internal node is not
  -- chosen, an external (terminal) node is
  -- chosen.
  ciProb    :: Double,
  -- | Probability that an individual gets mutated. Default is @0@
  -- (no mutation).
  mProb     :: Double,
  -- | Probability that an internal (functional) node is chosen for
  -- mutation. Default is @0.1@.
  miProb    :: Double,
  -- | Standardized fitness function. Default value is @undefined@
  -- (must be overriden).
  fitness   :: Fitness e,
  -- | Mutation function. Defines how to change a randomly chosen
  -- node. Default is @defaultMutation defaultEvolParams@
  -- (replacement of a chosen node with a randomly generated subexpression).
  mutate    :: Mutate m e,
  -- | Elitist factor: number of best-fitted individuals that are preserved
  -- from each generation (reproduced as-is into next evolution state).
  -- Default is @0@.
  elitists  :: Int,
  -- | Termination predicate. Default is @50@ (terminate after 50 generations).
  terminate :: Terminate e }

defaultEvolParams = EvolParams
  { popSize   = 500
  , iDepth    = 6
  , cDepth    = 17
  , cProb     = 0.9
  , ciProb    = 0.9
  , mProb     = 0.0
  , miProb    = 0.1
  , terminate = tGeneration 50
  , fitness   = error "GenProg.defaultEvolParams: fitness function is undefined"
  , mutate    = const $ generateGrownExpr (iDepth defaultEvolParams)
  , elitists  = 0 }

-----------------------------------------------------------------------------
-- Evolution

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = f x >>= untilM p f

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateUntilM p f x
  | p x       = return []
  | otherwise = do y  <- f x
                   ys <- iterateUntilM p f y
                   return (y:ys)

-- | Evolves one population from another one by performing a single
-- evolution step.
evolvePop :: (GenProg m e) => EvolParams m e -> Pop e -> m (Pop e)
evolvePop p pop1 = do
     pop2 <- mkPop `liftM` untilM ((>= s) . length) step []
     pop3 <- mutatePop p pop2
     return $ mkPop (elite ++ unPop pop3)
  where s = popSize p - length elite
        elite = take (elitists p) topRanked
        topRanked = sortBy (flip $ comparing aFitness) $ unPop pop1
        step is | length is == s - 1 = (:is) `liftM` reproducePop pop1
                | otherwise = choice (cProb p)
                    (do (i1,i2) <- crossoverPop p pop1; return (i1:i2:is))
                    ((:is) `liftM` reproducePop pop1)

-- | Creates an initial population and evolves it until termination
-- predicate is satisfied, returning the last evolution state.
evolve :: (GenProg m e) => EvolParams m e -> m (EvolState e)
evolve p = -- generatePop p >>= evolveFrom p
  last `liftM` evolveTrace p

-- | Evolves a given initial population until termination
-- predicate is satisfied, returning the last evolution state.
-- If the size of the initial population is less than
-- 'popSize', the population will be replenished (see 'replenishPop').
evolveFrom :: (GenProg m e) => EvolParams m e -> Pop e -> m (EvolState e)
evolveFrom p pop = -- untilM (terminate p) (nextState p) . initState
  last `liftM` evolveTraceFrom p pop

-- | Runs evolution on a given initial population until termination
-- predicate is satisfied and returns a list of successive evolution
-- states. If the size of the initial population is less than
-- 'popSize', the population will be replenished (see 'replenishPop').
evolveTraceFrom :: (GenProg m e) => EvolParams m e -> Pop e -> m [EvolState e]
evolveTraceFrom p pop1 =
  iterateUntilM (terminate p) (nextState p) . initState =<< replenishPop p pop1

-- | Creates an initial population and runs evolution until
-- termination predicate is satisfied. Returns a list of successive
-- evolution states.
evolveTrace :: (GenProg m e) => EvolParams m e -> m [EvolState e]
evolveTrace p = generatePop p >>= evolveTraceFrom p

-----------------------------------------------------------------------------
-- Example

{- $Example

This is a simple, worked through example of how to use the GenProg
library. Given a target number @n@, out aim is to evolve an arithmetic
expression that evaluates to @n@. For example, given @13@ as the
target number, one possible solution is @(3 * 5) - 2@. The constants
allowed to appear in the expression are restricted to integers from 1
to 9. The allowed operations are @+@, @-@, @*@, and integer division
without remainder.

We begin by defining the datatype for the genetically programed
expression:

@
-- The following language extensions need to be enabled:
-- DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses

import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random

data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
       | Const Int
       deriving (Typeable,Data,Eq,Show)
@

In order to evolve arithmetic expressions, we need to be able to
compute their values. To this end we define

@
eval :: E -> Maybe Int
eval (Const c)     = Just c
eval (Plus e1 e2)  = liftM2 (+) (eval e1) (eval e2)
eval (Minus e1 e2) = liftM2 (-) (eval e1) (eval e2)
eval (Times e1 e2) = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2) | ok        = liftM2 div x1 x2
                 | otherwise = Nothing
  where (x1,x2) = (eval e1,eval e2)
        ok = x2 /= Just 0 && liftM2 mod x1 x2 == Just 0
@

Dividing by zero and dividing with a remainder are not allowed and in
such cases we return @Nothing@.

Because we have made @E@ an instance of the 'Data' typeclass, it can
be readily used as a genetically programmable expression. Next step is
to make 'E' an instance of the 'GenProg' typeclass:

@
instance GenProg (Rand StdGen) E where
  terminal    = Const `liftM` getRandomR (1,9)
  nonterminal = do
    r <- getRandomR (0,3)
    [liftM2 Plus terminal terminal,
     liftM2 Minus terminal terminal,
     liftM2 Times terminal terminal,
     liftM2 Div terminal terminal] !! r
@

Thus, a random terminal node contains one of the constants from 1 to
9. A nonterminal node can be one of the four arithmetic operations,
each with terminal nodes as arguments.  Note that computations are run
within the standard random generator monad (@Rand StdGen@).

The fitness function evaluates the accurateness of the arithmetic
expression with respect to the target number. If the value of the
expression is far off from the target number @n@, the standardized
fitness should be high. Moreover, we would like to keep the expression
as simple as possible. To this end, we include a /parsimony factor/
that is proportional to the number of nodes an expression has. We
define the overall standardized fitness as

@
myFitness :: Int -> E -> Double
myFitness n e = error + size
  where error = realToFrac $ maybe maxBound (abs . (n-)) (eval e)
        size  = (realToFrac $ nodes e) / 100
@

The number of nodes is divided by a factor of 100 to make it less
important than the numeric accuracy of the expression.

We now have everything in place to get the evolution going. We will use
default evolution parameters and choose @12345@ as the target number:

>>> let params = defaultEvolParams { fitness = myFitness 12345 }

Let us first create a random number generator: 

>>> let g = mkStdGen 0

We are doing this because we want our results to be reproducible, and
because we want to be able to compare the results of different
evolution runs. Normally, you would use @getStdGen@ to get a random
generator with random seed.

To run the evolution and get the best evolved individual, we type

>>> let i = cachedBest $ evalRand (evolve params) g

To check out its standardized fitness, we type

>>> sFitness i
39.61

Let us see how the actual expression looks like:

>>> unInd i
Times (Minus (Minus (Minus (Plus (Const 4) (Const 4)) (Plus (Const 6) 
(Const 7))) (Minus (Minus (Const 5) (Const 9)) (Plus (Minus (Const 5) 
(Const 9)) (Minus (Const 4) (Const 4))))) (Plus (Times (Plus (Const 5) 
(Const 1)) (Const 6)) (Times (Plus (Const 9) (Const 3)) (Minus (Const 1) 
(Const 8))))) (Div (Times (Plus (Plus (Const 3) (Const 5)) (Times (Const 4) 
(Const 7))) (Plus (Const 4) (Const 4))) (Minus (Minus (Plus (Const 2) 
(Const 8)) (Plus (Const 6) (Const 7))) (Plus (Minus (Const 5) (Const 9)) 
(Minus (Const 4) (Const 4)))))

The number of nodes is

>>> nodes $ unInd i
61

Let us see to what number the expression evaluates:

>>> eval $ unInd i
Just 12384

So in this run we didn't get a perfect match, but we were close. Let
us see if we can do better.

When doing genetic programming, it is always a good idea to experiment
a bit with the parameters. There are no parameters that work best for
any given problem. You can learn a lot about how parameters influence
the evolution by analysing how the evolution progresses in time. This
can be accomplised by evolving an evolution trace:

>>> let trace = evalRand (evolveTrace params) g

We can now analyse how the standardized fitness of the
best individual improves during the evolution:

>>> map (sFitness . best . pop) trace
[9591.35,2343.59,1935.59,2343.59,903.51,903.45,585.59,585.59,327.45,225.41,
225.41,135.43,57.49,39.61,39.61,39.61,39.61,39.61,57.43,57.47,57.43,57.45,
57.33,57.43,57.43,57.45,57.43,57.43,57.35,57.35,57.43,57.27,57.33,57.33,57.43,
57.29,57.33,57.41,57.29,57.43,57.33,57.35,57.35,57.33,57.39,57.39,57.39,57.33,
57.37,57.37]

We see that at some point the fitness decreases and then increases
again. This indicates that the best fitted individual was lost by
evolving from one generation to the other. We can prevent this by
employing the /elitist strategy/. Let us see what happens if we
preserve a best fitted individual in each generation:

>>> let trace = evalRand (evolveTrace params {elitists = 1}) g 
>>> map (sFitness . best . pop) trace
[9591.35,2343.59,711.61,711.61,711.61,711.61,57.55,57.53,57.39,57.39,57.39,
57.39,57.37,57.37,57.37,57.37,57.37,57.37,57.37,57.37,57.35,57.35,57.35,
57.35,57.35,57.35,57.35,57.35,57.35,57.35,57.33,57.33,57.33,57.33,57.33,
57.33,57.33,57.33,57.33,25.31,25.31,25.31,25.31,25.31,25.31,25.296,25.296,
25.296,25.296,25.296]

This gives us better fitness, but still not an exact match:

>>> let i = cachedBest $ last trace
>>> eval $ unInd i
Just 12320

In the previous evolution run fitness converged relatively fast, but then
remained stuck. To stir up things a little, let us allow for some
mutation. Setting mutation probability to 5%, while retaining the
elitist strategy, we get

>>> let trace = evalRand (evolveTrace params {elitists = 1, mProb = 0.05}) g
>>> map (sFitness . best . pop) trace
[9591.35,9591.35,9591.35,9591.35,9591.35,9591.35,9159.35,8403.23,7239.11,
6087.15,6087.15,1479.13,819.21,60.13,51.19,5.19,5.19,5.19,5.19,5.19,1.23,
1.23,1.23,1.23,1.23,1.23,1.21,1.21,1.21,1.21,0.23998,0.23998,0.23998,0.23998,
0.23998,0.23998,0.23998,0.23998,0.23998,0.23998,0.23998,0.23998,0.23998,
0.23998,0.23998,0.23998,0.23998,0.23998,0.23998,0.23998]

This time we've got a perfect match:

>>> let i = cachedBest $ last trace
>>> eval $ unInd i
Just 12345

while at the same time the expression is rather compact:

>>> unInd i
Plus (Times (Const 4) (Plus (Const 9) (Const 4))) (Plus (Plus (Times 
(Plus (Const 4) (Const 3)) (Times (Times (Const 3) (Const 9)) (Times 
(Const 5) (Plus (Const 9) (Const 4))))) (Const 3)) (Const 5))
>>> nodes $ unInd i
23

-}
