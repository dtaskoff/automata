module FSA where

import Prelude hiding (Word)
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type Alphabet = [Char]
type Word = String
type State = Int

type Transition = (State, Word, State)

data FSA = FSA
  { alphabet :: Alphabet
  , states :: Int
  -- ^ the number of states
  -- I'll assume that FSAs have states 0..states-1
  , initial :: [State]
  , terminal :: [State]
  , delta :: [Transition]
  } deriving Show


-- | An automaton which accepts a single word
word :: Word -> FSA
word w = FSA w 2 [0] [1] [(0, w, 1)]

-- | Returns True iff the automaton accepts the given word
accepts :: FSA -> Word -> Bool
accepts fsa w = any (`elem` terminal fsa) $ concatMap (go w) $ initial fsa
  where go "" p | p `elem` terminal fsa = [p]
        go w' p = concatMap (uncurry go) $
          [(fromJust mv, q) |
            (p', u, q) <- delta fsa, p' == p,
            let mv = stripPrefix w' u, isJust mv
          ]
