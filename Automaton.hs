module Automaton where

import qualified Data.Map.Lazy       as Map
import qualified Data.Set            as Set

import           Combinators
import           Control.Applicative (many)
import           Control.Monad       (when)
import           Data.Maybe          (listToMaybe)

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma      :: Set s
                               , states     :: Set q
                               , initState  :: q
                               , termStates :: Set q
                               , delta      :: Map (q, s) (Maybe q)
                               }

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q

parseAutomaton :: String -> Either String (Automaton String String)
parseAutomaton = fmap snd . runParser automatonP

automatonP :: Parser String (Automaton String String)
automatonP = do
    _ <- char '<' *> many space

    sigma' <- Set.fromList <$> listP manyCharP

    colonP
    states' <- Set.fromList <$> listP manyCharP
    when (Set.null states') $ fail "Set of states is empty."

    colonP
    initStateM <- listToMaybe <$> listP manyCharP
    initState' <- maybe (fail "initState is not provided.") pure initStateM
    when (Set.notMember initState' states') $ fail "initState not in states."

    colonP
    termStates' <- Set.fromList <$> listP manyCharP
    when (not (Set.isSubsetOf termStates' states')) $ fail "terminalStates is not subset of states."

    colonP
    deltaL <- listP $ tripleP manyCharP manyCharP
    delta' <- listOfTriplesToMap states' sigma' deltaL

    _ <- many space <* char '>'

    pure $ Automaton sigma' states' initState' termStates' delta'
  where
    colonP :: Parser String ()
    colonP = const () <$> many space <* char ',' <* many space

    manyCharP :: Parser String String
    manyCharP = takeWhileP (\x -> x /= ',' && x /= ' ' && x /= '>' && x /= ')')

    listP :: Parser String a -> Parser String [a]
    listP p = parseList p (char ',') (char '<') (char '>') 1

    tripleP :: Parser String a -> Parser String b -> Parser String (a, b, a)
    tripleP aP bP = char '(' *> pure (,,)
                 <* many space <*> aP <* many space <* char ','
                 <* many space <*> bP <* many space <* char ','
                 <* many space <*> aP <* many space <* char ')'

    listOfTriplesToMap :: (Ord a, Ord b) => Set a -> Set b -> [(a, b, a)] -> Parser String (Map (a, b) (Maybe a))
    listOfTriplesToMap st sig l = do
        when (any (\(x, y, z) -> x `Set.notMember` st || y `Set.notMember` sig || z `Set.notMember` st) l) $
          fail "Delta function is badly defined."

        let prod = productOfSets st sig
        let triM = triplesToMap l
        pure $ Map.fromList $ fmap (\x -> (,) x $ x `Map.lookup` triM) prod

    productOfSets :: Set a -> Set b -> [(a, b)]
    productOfSets aSet bSet = [(x, y) | x <- Set.toList aSet, y <- Set.toList bSet]

    triplesToMap :: (Ord a, Ord b) => [(a, b, a)] -> Map (a, b) a
    triplesToMap = Map.fromList . fmap (\(x, y, z) -> ((x, y), z))


-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA = undefined

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA = undefined

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool
isComplete = undefined

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined
