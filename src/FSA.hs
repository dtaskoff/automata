module FSA where

import Prelude hiding (Word)
import Data.List (nub, stripPrefix)
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
            let mv = stripPrefix u w', isJust mv
          ]

union :: FSA -> FSA -> FSA
union fsa fsa' =
  let fsa'' = rename fsa' (states fsa)
  in  FSA { alphabet = nub $ alphabet fsa ++ alphabet fsa''
          , states = states fsa + states fsa''
          , initial = initial fsa ++ initial fsa''
          , terminal = terminal fsa ++ terminal fsa''
          , delta = delta fsa ++ delta fsa''
          }

concatenate :: FSA -> FSA -> FSA
concatenate fsa fsa' =
  let fsa'' = rename fsa' (states fsa)
  in  FSA { alphabet = nub $ alphabet fsa ++ alphabet fsa''
          , states = states fsa + states fsa'
          , initial = initial fsa
          , terminal = terminal fsa''
          , delta = delta fsa ++ delta fsa'' ++
              [(t, "", i) | t <- terminal fsa, i <- initial fsa'']
          }

star :: FSA -> FSA
star fsa =
  let q = states fsa
  in  fsa { states = states fsa + 1
          , initial = [q]
          , terminal = [q]
          , delta = delta fsa ++
              [(q, "", i) | i <- initial fsa] ++
              [(t, "", q) | t <- terminal fsa]
          }

-- | Rename the states in a given FSA (increase them with n)
rename :: FSA -> Int -> FSA
rename fsa n = fsa { states = states fsa + n
                   , initial = map (+ n) $ initial fsa
                   , terminal = map (+ n) $ terminal fsa
                   , delta = map (\(p, w, q) -> (p+n, w, q+n)) $ delta fsa
                   }
