module Automaton where

import qualified Data.Map.Lazy       as Map
import qualified Data.Set            as Set

import           Combinators
import           Control.Applicative (many)
import           Control.Monad       (when)
import           Data.Bifunctor      (first)
import           Data.List           (groupBy, nubBy, sortOn)
import           Data.Maybe          (listToMaybe)

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma      :: Set s
                               , states     :: Set q
                               , initState  :: q
                               , termStates :: Set q
                               , delta      :: Map (q, s) [q]
                               }
  deriving (Eq, Show)

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q

parseAutomaton :: String -> Either String (Automaton String String)
parseAutomaton = first (concat . fmap show) . parse automatonP

automatonP :: ParserS (Automaton String String)
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
    colonP :: ParserS ()
    colonP = const () <$> many space <* char ',' <* many space

    manyCharP :: ParserS String
    manyCharP = (++) <$> many (char '\\') <*> takeWhileP (\x -> x /= ',' && x /= ' ' && x /= '>' && x /= ')')

    listP :: ParserS a -> ParserS [a]
    listP p = parseList p (char ',') (char '<') (char '>') 1

    tripleP :: ParserS a -> ParserS b -> ParserS (a, b, a)
    tripleP aP bP = char '(' *> pure (,,)
                 <* many space <*> aP <* many space <* char ','
                 <* many space <*> bP <* many space <* char ','
                 <* many space <*> aP <* many space <* char ')'

    listOfTriplesToMap :: Ord a => Set a -> Set String -> [(a, String, a)] -> ParserS (Map (a, String) [a])
    listOfTriplesToMap st sig l = do
        when (any (\(x, y, z) -> x `Set.notMember` st || (y /= "\\epsilon" && y `Set.notMember` sig) || z `Set.notMember` st) l) $
          fail "Delta function is badly defined."

        let prod = productOfSets st sig
        let triM = triplesToMap l
        pure $ Map.fromList $ fmap (\x -> maybe (x, []) ((,) x) $ x `Map.lookup` triM) prod

    triplesToMap :: (Ord a, Ord b) => [(a, b, a)] -> Map (a, b) [a]
    triplesToMap = Map.fromList . groupByKeys . fmap (\(x, y, z) -> ((x, y), z))

    groupByKeys :: (Eq a, Eq b, Ord a, Ord b) => [((a, b), a)] -> [((a, b), [a])]
    groupByKeys = fmap (\l@(x : _) -> (fst x, fmap snd l)) . groupBy (\x y -> fst x == fst y) . sortOn fst

    productOfSets :: Set a -> Set b -> [(a, b)]
    productOfSets aSet bSet = [(x, y) | x <- Set.toList aSet, y <- Set.toList bSet]


-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Show b => Automaton String b -> Bool
isDFA auto = all ((<= 1) . length . snd) transList && all ((/= "\\epsilon") . snd . fst) transList
  where
    transList = Map.toList . delta $ auto

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton String b -> Bool
isNFA = const True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool
isComplete = all ((== 1) . length . snd) . Map.toList . delta

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton String String -> Bool
isMinimal auto = isDFA auto && null res
  where
    nullAdd       = Map.fromList (fmap (\x -> (("null", x), ["null"])) $ Set.toList $ sigma auto)
    completeDelta = fmap (\x -> if null x then ["null"] else x) (delta auto) `Map.union` nullAdd
    revDelta''    = Map.fromList
                  $ ungroups . sortOn fst
                  $ fmap (\((a, b), [a']) -> (a', (a, b)))
                  $ Map.toList completeDelta
    revDelta'     = Map.mapWithKey (const . maybe [] id . flip Map.lookup revDelta'') graphMap

    reachableStates = dfs (initState auto) []
    revDelta        = fmap (filter ((`elem` reachableStates) . fst)) revDelta'

    stateToInd = Map.fromList $ zip reachableStates [0.. length reachableStates - 1]

    table'   = [[False | _ <- [0.. length reachableStates - 1]] | _ <- [0.. length reachableStates - 1]]
    allPairs = [(x, y) | x <- reachableStates, y <- reachableStates]

    (initTable, initQueue) = initTableQueue allPairs table' []
    resTable               = recurse initQueue initTable

    equalStates = [(i, j) | i <- [0.. length reachableStates - 1], j <- [0.. length reachableStates - 1], not (resTable !! i !! j)]
    res         = filter (\(x, y) -> x /= y) $ nubBy (\(x, y) (x', y') -> x == x' && y == y' || x == y' && y == x') equalStates

    recurse :: [(String, String)] -> [[Bool]] -> [[Bool]]
    recurse [] table            = table
    recurse ((x, y) : xs) table | table !! i !! j || table !! j !! i = recurse (xs ++ prevs) newTable
                                | otherwise                          = recurse xs table
      where
        i = stateToInd Map.! x
        j = stateToInd Map.! y

        prevsX = revDelta Map.! x
        prevsY = revDelta Map.! y

        possiblePrevs = fmap (\(a, b) -> (fst a, fst b)) $ filter (\(a, b) -> snd a == snd b) [(a, b) | a <- prevsX, b <- prevsY]
        prevs         = filter (\(a, b) -> not $ table !! (stateToInd Map.! a) !! (stateToInd Map.! b)) possiblePrevs

        newTable = foldl (\table (a, b) -> setElemTable (setElemTable table a b) b a) table prevs

    initTableQueue :: [(String, String)] -> [[Bool]] -> [(String, String)] -> ([[Bool]], [(String, String)])
    initTableQueue [] t q            = (t, q)
    initTableQueue ((x, y) : xs) t q | diffCond  = initTableQueue xs (setElemTable (setElemTable t x y) y x) newQueue
                                     | otherwise = initTableQueue xs t q
      where
        terms    = termStates auto
        diffCond = x `Set.notMember` terms && y `Set.member` terms || x `Set.member` terms && y `Set.notMember` terms

        newQueue = q ++ [(x, y)]

    setElemTable :: [[Bool]] -> String -> String -> [[Bool]]
    setElemTable table x y = newTable
      where
        i = stateToInd Map.! x
        j = stateToInd Map.! y

        newTable = fmap (\(ind, line) -> if ind == i then take j line ++ [True] ++ drop (j + 1) line else line) $ zip [0..] table

    graphMap = Map.fromList
             $ ungroups
             $ fmap (\((a, _), [a']) -> (a, a'))
             $ Map.toList completeDelta

    dfs :: String -> [String] -> [String]
    dfs cur taken | cur `elem` taken = taken
                  | otherwise        = foldl (flip dfs) (cur : taken) $ graphMap Map.! cur

    ungroups :: Eq a => [(a, b)] -> [(a, [b])]
    ungroups = fmap (\l@(x : _) -> (fst x, fmap snd l)) . groupBy (\x y -> fst x == fst y)
