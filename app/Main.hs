module Main where

import Combinators
import Example
import FST


main :: IO ()
main = do
  putStrLn "Transducing with R"
  print . head . transduce rFST =<< text

  putStrLn "Transducing with R1"
  print . head . transduce r1FST =<< text

  putStrLn "Conflict freeness of N"
  print $ conflictFree nFST

  putStrLn "Conflict freeness of N1"
  print $ conflictFree n1FST

  putStrLn "Conflict freeness of R"
  print $ conflictFree rFST

  putStrLn "Conflict freeness of R1"
  print $ conflictFree r1FST
