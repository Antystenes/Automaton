module Main where


import Lib
import RegexpParser
import Data.Char(toUpper)

main :: IO ()
main = do
  aut <- automatonFromRegexp <$> getLine --readAutomaton
  print aut
  print $ (determinize aut)
  args <- readWords
  let result = (map ((toUpper <$>).show . isAccepted . flip processString (initAutomaton aut)) args)
  sequence_ (putStrLn <$> (zipWith (\x y -> x++" "++y) result args)++(lines $ show aut))
