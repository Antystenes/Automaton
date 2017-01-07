module Lib  where

import qualified Data.HashMap as HM
import qualified Data.Set as S
import System.IO
import System.Environment
import Data.List
import Data.List.Split (splitOn)

data Automaton = Automaton { transitions :: HM.Map (String, Char) (S.Set String),
                             accept :: (S.Set String),
                             starting :: String,
                             state :: (S.Set String)} deriving Eq
instance Show Automaton where
  show a = unlines $ (sort trans)++accep
    where trans  = HM.foldWithKey (\(st, val) states acc -> S.foldr (\x ac -> unwords [st, x, if val == '∊' then "<eps>" else [val]]:ac) acc states) [] $ transitions a
          accep = S.toList $ accept a

readWords :: IO [String]
readWords = lines <$> (((!!0) <$>getArgs ) >>= readFile)

addTransition :: String -> String -> Char -> Automaton -> Automaton
addTransition beginning ending value aut = aut {transitions = HM.insertWith S.union (beginning, value) (S.singleton ending) $ transitions aut}

addAcceptState :: String -> Automaton -> Automaton
addAcceptState ch aut = aut {accept = S.insert ch $ accept aut}

emptyAutomaton :: Automaton
emptyAutomaton = Automaton { transitions = HM.empty,
                             accept = S.empty,
                             starting = "0",
                             state = S.empty}

initAutomaton :: Automaton -> Automaton
initAutomaton aut = aut {state = S.singleton "0"}

addEpsilon :: String -> String -> Automaton -> Automaton
addEpsilon x y = addTransition x y '∊'

toChar :: String -> Char
toChar [x] = x
toChar "<eps>" = '∊'
toChar _ = ' '

addTransFromList :: [String] -> Automaton -> Automaton
addTransFromList (x:y:z:[]) = addTransition x y $ toChar z
addTransFromList _ = id

parseString :: String -> Automaton -> Automaton
parseString x
  | (length . words $ x) > 1 = addTransFromList . words $ x
  | otherwise = addAcceptState x

parseLines :: [String] -> Automaton -> Automaton
parseLines x = foldr (.) id $ map parseString x

tranState :: Char -> String -> HM.Map (String, Char) (S.Set String) -> S.Set String
tranState ch st tr
  | HM.member (st, ch) tr = tr HM.! (st, ch)
  | otherwise = S.empty
--  where epsneighbour = if HM.member (st, '∊') tr then tr HM.! (st, '∊') else S.empty
--        reachable = S.foldr (\x acc -> (tranState '∊' x tr) `S.union` acc) S.empty epsneighbour

processChar :: Char -> Automaton -> Automaton
processChar ch aut = aut{state = newstate . reachable (state aut) $ transitions aut}
  where newstate = S.foldr (\st acc -> acc `S.union` (tranState ch st $ transitions aut)) S.empty

reachable :: S.Set String -> HM.Map (String, Char) (S.Set String) -> S.Set String
reachable stat tr = if new == stat then stat else reachable new tr
  where new = S.foldr (\st acc -> acc `S.union` if HM.member (st, '∊') tr then st `S.insert` (tr HM.! (st, '∊')) else S.singleton st ) S.empty stat
-- TODO - fix epsilon transitions
processString :: String -> Automaton -> Automaton
processString = flip $ foldl (\a x -> processChar x a )

isAccepted :: Automaton -> Bool
isAccepted aut = if acceptance aut then True else acceptance $ processChar '∊' aut
  where acceptance = not . null . (<*> id)((flip (.) state).(S.intersection . accept))
--  where acceptance = not . null . (<*> id)((flip (.) state).(S.intersection . accept))


readAutomaton :: IO Automaton
readAutomaton = isEOF >>= loop [] >>= (return . flip parseLines emptyAutomaton)
  where loop xs b = if b
          then return xs
          else ((:xs) <$> getLine) >>= (\new -> isEOF >>= loop new)

acceptingStates :: Automaton -> [String]
acceptingStates = S.toList . accept

mergeTrans :: Automaton -> Automaton -> Automaton
mergeTrans a1 a2 = a1 { transitions = HM.union (transitions a1) (transitions a2)}

-- DETERMINIZATION

extractAlphabet :: Automaton -> S.Set Char
extractAlphabet = HM.foldWithKey (\(_,c) _ acc -> S.insert c acc) S.empty . transitions

determinizeTransitions :: Automaton -> Automaton -> Automaton
determinizeTransitions aut acc = foldr determinizer acc states
  where states = state acc
        determinizer st ac = HM.foldWithKey (\(s, v) s2 a ->
                                             if s `S.member` (toIndet st)
                                             then a {transitions = HM.insertWith S.union (st, v) s2 $ transitions a}
                                             else a) ac $ transitions aut
        toIndet = S.fromList . splitOn ","
        --toDet = unwords . S.toList

setDetStates :: Automaton -> Automaton
setDetStates aut = aut { state = HM.fold (\v acc -> S.insert ( intercalate "," . S.toList $ v) acc ) S.empty (transitions aut)}

setAcceptingStates :: Automaton -> Automaton -> Automaton
setAcceptingStates aut acc = acc{ accept = HM.foldWithKey adder S.empty $ transitions acc}
  where adder (s,_) v ac = ac `S.union` (addV v) `S.union` (addS s)
        addS s = if S.null $ (S.fromList . splitOn "," $ s) `S.intersection` (accept aut)
          then S.empty
          else S.singleton s
        addV v = if S.null $ v `S.intersection` (accept aut)
          then S.empty
          else S.singleton . intercalate "," . S.toList $ v

determinization :: Automaton -> Automaton -> Automaton
determinization aut acc = if (state acc) == (state newAut) then setAcceptingStates aut newAut else determinization aut newAut
  where newAut = setDetStates . determinizeTransitions aut $ acc

flattenSet :: Automaton -> Automaton
flattenSet aut = aut{transitions = HM.map (\v -> S.singleton . intercalate "," . S.toList $ v) $ transitions aut}

determinize :: Automaton -> Automaton
determinize = setUniqueStateID . flattenSet . flip determinization (initAutomaton emptyAutomaton)

extractStates :: Automaton -> S.Set String
extractStates = HM.foldWithKey (\(s,_) s2 acc -> S.insert s . S.union s2 $ acc) S.empty . transitions

generateNamesMap :: Automaton -> HM.Map String String
generateNamesMap = HM.fromList . flip (zipWith (\x y -> (x,show y))) [0..] . S.elems . extractStates

setUniqueStateID :: Automaton -> Automaton
setUniqueStateID aut = aut { transitions = HM.foldWithKey (\(s, c) v acc -> HM.insert (names HM.! s, c) ( S.map (\a -> names HM.! a) v) acc) HM.empty $ transitions aut,
                                  accept = S.map (\a -> names HM.! a) $ accept aut,
                                starting = names HM.! (starting aut)}
  where names = generateNamesMap aut

prependToState :: Char -> Automaton -> Automaton
prependToState a aut = aut { transitions = HM.foldWithKey (\(s,c) v acc -> HM.insert (a:s,c) (S.map (a:) v) acc) HM.empty $ transitions aut,
                                  accept = S.map (a:) $ accept aut,
                                starting = a:(starting aut)}
