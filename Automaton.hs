module Automaton where

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           Combinators         hiding (find)
import           Control.Applicative (many)
import           Control.Monad       (forM_, when)
import           Control.Monad.State (State, execState, get, modify)
import           Data.Bifunctor      (bimap, first, second)
import           Data.List           (find, groupBy, intercalate, nub, nubBy,
                                      sort, sortOn)
import           Data.Maybe          (catMaybes, listToMaybe)

import           Debug.Trace         (trace)

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

        let prod   = productOfSets st $ Set.insert "\\epsilon" sig
        let triM   = triplesToMap l
        let potMap = fmap (\x -> maybe (x, []) ((,) x) $ x `Map.lookup` triM) prod

        pure $ Map.fromList $ filter (\((a, b), c) -> not (b == "\\epsilon" && null c)) potMap

    triplesToMap :: (Ord a, Ord b) => [(a, b, a)] -> Map (a, b) [a]
    triplesToMap = Map.fromList . groupByKeys . fmap (\(x, y, z) -> ((x, y), z))

    groupByKeys :: (Eq a, Eq b, Ord a, Ord b) => [((a, b), a)] -> [((a, b), [a])]
    groupByKeys = fmap (\l@(x : _) -> (fst x, fmap snd l)) . groupBy (\x y -> fst x == fst y) . sortOn fst

    productOfSets :: Set a -> Set b -> [(a, b)]
    productOfSets aSet bSet = [(x, y) | x <- Set.toList aSet, y <- Set.toList bSet]


-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton String b -> Bool
isDFA auto = all ((<= 1) . length . snd) transList && all ((/= "\\epsilon") . snd . fst) transList
  where
    transList = Map.toList . delta $ auto

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton String b -> Bool
isNFA = const True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton String b -> Bool
isComplete auto = isDFA auto && (all ((== 1) . length . snd) . Map.toList . delta $ auto)

complete :: Automaton String String -> Automaton String String
complete auto | isDFA auto = auto { states=newStates, delta=completeDelta }
              | otherwise  = Prelude.error "Automaton is not DFA."
  where
    newStates     = "null" `Set.insert` states auto
    nullAdd       = Map.fromList (fmap (\x -> (("null", x), ["null"])) $ Set.toList $ sigma auto)
    completeDelta = fmap (\x -> if null x then ["null"] else x) (delta auto) `Map.union` nullAdd

determinize :: Automaton String String -> Automaton String String
determinize auto | isDFA auto                                               = auto
                 | any ((== "\\epsilon") . snd . fst) $ Map.toList oldDelta = Prelude.error "Automaton contains \\epsilon transitions."
                 | otherwise                                                = res
  where
    alphabet = sigma auto
    oldDelta = delta auto
    terms    = termStates auto
    initSt   = initState auto

    (_, manyAutomaton) = execState getDFAByNFA $ ([[initSt]], Automaton alphabet Set.empty [initSt] Set.empty Map.empty)

    newTermStates = foldl (\set x -> if any (`elem` terms) x then x `Set.insert` set else set) Set.empty (states manyAutomaton)
    newDelta      = Map.fromList $ fmap (bimap (first unionString) (fmap unionString)) $ Map.toList $ delta manyAutomaton

    res = Automaton alphabet (Set.map unionString $ states manyAutomaton) initSt (Set.map unionString newTermStates) newDelta

    getDFAByNFA :: State ([[String]], Automaton String [String]) ()
    getDFAByNFA = do
        (queue, _) <- get

        if not (null queue)
          then do
              modify (first tail)
              let pD = head queue

              forM_ alphabet $ \sym -> do
                  (_, auto') <- get
                  let qD       = foldl (\set -> (`Set.union` set) . Set.fromList . (oldDelta Map.!) . (flip (,) sym)) Set.empty pD
                  let qDListed = sort $ Set.toList qD

                  modify (second (\x -> x { delta=Map.insert (pD, sym) [qDListed] $ delta x }))

                  when (qDListed `Set.notMember` (states auto')) $ modify (bimap (++ [qDListed]) (\x -> x { states=Set.insert qDListed $ states x }))

              getDFAByNFA
          else pure ()

unionString :: [String] -> String
unionString = intercalate "&" . sort

epsClojure :: Automaton String String -> Automaton String String
epsClojure auto = res
  where
    alphabet = Set.filter (/= "\\epsilon") $ sigma auto
    delt     = delta auto
    terms    = termStates auto

    manyNewStates   = fmap (Set.fromList . flip dfs []) $ Set.toList $ states auto
    newStates       = fmap Set.toList $ nub $ filter (\x -> all (not . (x `Set.isProperSubsetOf`)) manyNewStates) manyNewStates

    newDelta      = getTransitions alphabet newStates delt
    newTermStates = foldl (\set x -> if any (`elem` terms) x then x `Set.insert` set else set) Set.empty newStates
    newInitState  = maybe (Prelude.error "Now init state in epsClojure") unionString $ find (initState auto `elem`) newStates

    res = Automaton alphabet (Set.fromList $ fmap unionString newStates) newInitState (Set.map unionString newTermStates) newDelta

    dfs :: String -> [String] -> [String]
    dfs cur taken | cur `elem` taken = taken
                  | otherwise        = foldl (flip dfs) (cur : taken) $ maybe [] id $ (cur, "\\epsilon") `Map.lookup` delt

getTransitions :: Set String -> [[String]] -> Map (String, String) [String] -> Map (String, String) [String]
getTransitions alphabet sts delt = Map.fromList $ do
    a  <- Set.toList alphabet
    st <- sts

    let oldTrans = nub $ concatMap ((delt Map.!) . (flip (,) a)) st
    let newTrans = nub $ concatMap (\x -> filter (x `elem`) sts) oldTrans

    pure $ ((unionString st, a), fmap unionString newTrans)

minimize :: Automaton String String -> Automaton String String
minimize auto | not (isDFA auto) = Prelude.error "Automaton is not DFA."
              | otherwise        = res
  where
    completeDelta = delta $ complete auto
    revDelta''    = Map.fromList
                  $ ungroups . sortOn fst
                  $ fmap (\((a, b), [a']) -> (a', (a, b)))
                  $ Map.toList completeDelta
    revDelta'     = Map.mapWithKey (const . maybe [] id . flip Map.lookup revDelta'') graphMap

    reachableStates = dfs graphMap (initState auto) []
    revDelta        = fmap (filter ((`elem` reachableStates) . fst)) revDelta'

    stateToInd = Map.fromList $ zip reachableStates [0.. length reachableStates - 1]

    table'   = [[False | _ <- [0.. length reachableStates - 1]] | _ <- [0.. length reachableStates - 1]]
    allPairs = [(x, y) | x <- reachableStates, y <- reachableStates]

    (initTable, initQueue) = initTableQueue allPairs table' []
    resTable               = recurse initQueue initTable

    equalStates   = [ (reachableStates !! i, reachableStates !! j)
                    | i <- [0.. length reachableStates - 1]
                    , j <- [0.. length reachableStates - 1]
                    , not (resTable !! i !! j)
                    , reachableStates !! i /= "null"
                    , reachableStates !! j /= "null"
                    ]

    equalityGraph = Map.fromList $ fmap (second pure) equalStates
    uniStates     = fmap (Set.fromList . flip (dfs equalityGraph) []) $ fmap fst equalStates
    newStates     = fmap Set.toList $ nub $ filter (\x -> all (not . (x `Set.isProperSubsetOf`)) uniStates) uniStates

    newDelta      = getTransitions (sigma auto) newStates (delta auto)
    newTermStates = foldl (\set x -> if any (`elem` termStates auto) x then x `Set.insert` set else set) Set.empty newStates
    newInitState  = maybe (Prelude.error "Now init state in minimize") unionString $ find (initState auto `elem`) newStates

    res = Automaton (sigma auto) (Set.fromList $ fmap unionString newStates) newInitState (Set.map unionString newTermStates) newDelta

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

    dfs :: Map String [String] -> String -> [String] -> [String]
    dfs m cur taken | cur `elem` taken = taken
                    | otherwise        = foldl (flip (dfs m)) (cur : taken) $ m Map.! cur

    ungroups :: Eq a => [(a, b)] -> [(a, [b])]
    ungroups = fmap (\l@(x : _) -> (fst x, fmap snd l)) . groupBy (\x y -> fst x == fst y)

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton String String -> Bool
isMinimal auto = isDFA auto && Set.size (states auto) == Set.size (states $ minimize auto)

