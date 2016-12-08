module Lib  where

import qualified Data.HashMap as HM
import qualified Data.Set as S
import System.IO
import System.Environment


data Automaton = Automaton { transitions :: HM.Map (String, Char) (S.Set String),
                             accept :: (S.Set String),
                             state :: (S.Set String)} deriving Show

readWords :: IO [String]
readWords = lines <$> (((!!0) <$>getArgs ) >>= readFile)


addTransition :: String -> String -> Char -> Automaton -> Automaton
addTransition beginning ending value aut = aut {transitions = HM.insert  (beginning, value) (S.singleton ending) $ transitions aut}

addAcceptState :: String -> Automaton -> Automaton
addAcceptState ch aut = aut {accept = S.insert ch $ accept aut}

emptyAutomaton :: Automaton
emptyAutomaton = Automaton { transitions = HM.empty,
                             accept = S.empty,
                             state = S.empty}

initAutomaton :: Automaton -> Automaton
initAutomaton aut = aut {state = S.singleton "0"}

toChar :: String -> Char
toChar [x] = x
toChar "<eps>" = '∊'
toChar _ = ' '

addTransFromList :: [String] -> Automaton -> Automaton
addTransFromList (x:y:z:[]) = addTransition x y $ toChar z
addTransFromList _ = id

parseString :: String -> Automaton -> Automaton
parseString x
  | length yolo > 1 = addTransFromList yolo
  | otherwise = addAcceptState x
  where yolo = words x

parseLines :: [String] -> Automaton -> Automaton
parseLines x = foldr (.) id $ map parseString x

tranState :: Char -> String -> HM.Map (String, Char) (S.Set String) -> S.Set String
tranState ch st tr
  | HM.member (st, ch) tr = tr HM.! (st, ch)
  | HM.member (st, '∊') tr = tr HM.! (st, '∊')
  | otherwise = S.empty

processChar :: Char -> Automaton -> Automaton
processChar ch aut = aut{state = newstate}
  where newstate = S.foldr (\st acc -> acc `S.union` (tranState ch st $ transitions aut)) S.empty $ state aut

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
