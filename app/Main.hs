module Main where


import Lib
import Data.Char(toUpper)

main :: IO ()
main = do
  aut <- readAutomaton
  args <- readWords
  let result = (map ((toUpper <$>).show . isAccepted . flip processString (initAutomaton aut)) args)
  sequence_ (putStrLn <$> (zipWith (\x y -> x++" "++y) result args))
