--GENERATING AUTOMATON FROM REGEXP
module RegexpParser where
import Lib
import qualified Data.Set as S

data Regexp = Sym Char --symbol
            | Plus Regexp -- 1-n times
            | Star Regexp -- 0-n times
            | Ques Regexp -- 0-1 times
            | Alt Regexp Regexp --alternative
            | Conc Regexp Regexp--concatenation
            | Eps deriving (Show, Eq)

data ParsingState = ParsingState { result :: Regexp,
                                   nesting :: Int,
                                   remaining :: String
                                 } deriving Show

parser :: ParsingState -> ParsingState
parser (ParsingState st n []) = (ParsingState st n [])
parser (ParsingState st n (x:xs)) = case x of
    '|' -> (if (n > (nesting subparse)) then id else parser) $ ParsingState (Alt st (result subparse)) (nesting subparse) (remaining subparse)
      where subparse = parser $ ParsingState Eps n xs
    '\\'-> parser $ ParsingState (if st /= Eps then Conc st (Sym (head xs)) else Sym (head xs)) n (tail xs)
    '(' -> parser $ ParsingState ((if st /= Eps then Conc st else id) (parserHelper1 ss $ result subparse)) (n) (parserHelper2 ss)
      where subparse = parser $ ParsingState Eps (n+1) xs
            ss = remaining subparse
    ')' -> ParsingState (st) (n-1) (xs)
    _   -> parser $ ParsingState ((if st /= Eps then Conc st else id) (parserHelper1 xs $ Sym x)) n  (parserHelper2 xs)

parserHelper1 :: String -> Regexp -> Regexp
parserHelper1 []     = id
parserHelper1 (x: _) = case x of
  '+' -> Plus
  '*' -> Star
  '?' -> Ques
  _   -> id

parserHelper2 :: String -> String
parserHelper2 a@(x:xs) = if x `elem` "+*?" then xs else a
parserHelper2 [] = []

parse :: String -> Regexp
parse = result . parser . ParsingState Eps 0
{-

Sym a =  (Q0)-a->(Q1_)

         (Q0)-eps->[r]-eps->(Q1_)
Plus r =   ^                 |
           |-eps-------------|

           |-eps-------------|
           |                 V
Star r = (Q0)-eps->[r]-eps->(Q1_)
           ^                 |
           |-eps-------------|

           |-eps-------------|
Ques r =   |                 V
         (Q0)-eps->[r]-eps->(Q1_)

          |-eps--->[r]-eps---|
          |                  V
Alt r q =(Q0)               (Q1_)
          |                  ^
          |-eps--->[q]-eps---|

Conc r q=(Q0)-eps->[r]-eps->[q]->eps->(Q1_)

-}

regexpToAutomaton :: Regexp -> Automaton
regexpToAutomaton x = case x of    -- by convention beginning state of result is 0
  Sym a    -> addAcceptState "1" . -- and accepting state is 1
              addTransition "0" "1" a $
              emptyAutomaton { starting = "0" }
  Plus r   -> addEpsilon end beg $
              aut
    where aut = regexpToAutomaton r
          beg = starting aut
          end = head . acceptingStates $ aut
  Star r    -> addEpsilon end beg .
               addEpsilon beg end $
               aut
    where aut = regexpToAutomaton r
          beg = starting aut
          end = head . acceptingStates $ aut
  Ques r    -> addEpsilon beg end $
               aut
    where aut = regexpToAutomaton r
          beg = starting aut
          end = head . acceptingStates $ aut
  Alt r s   -> addEpsilon en2 "1" .
               addEpsilon en1 "1" .
               addEpsilon "0" be2 .
               addEpsilon "0" be1 $
               aut
    where aut = mergeTrans au1 au2
          (au1, be1, en1) = automatizationHelper 'r' r
          (au2, be2, en2) = automatizationHelper 's' s
  Conc r s  -> addEpsilon en1 be2 $
               aut { accept = S.singleton en2, starting = be1}
    where aut = mergeTrans au1 au2
          (au1, be1, en1) = automatizationHelper 'r' r
          (au2, be2, en2) = automatizationHelper 's' s
  Eps       -> emptyAutomaton

automatizationHelper :: Char -> Regexp -> (Automaton, String, String)
automatizationHelper c r = ( addAcceptState "1" $ a {accept = (accept emptyAutomaton), starting = "0"}, beg, end)
  where a = prependToState c . regexpToAutomaton $ r
        beg = starting a
        end = head . acceptingStates $ a

automatonFromRegexp :: String -> Automaton
automatonFromRegexp = (setUniqueStateID) . regexpToAutomaton . parse
