module Combinators where

import FSM
import FSA (FSA)
import FST (FST)
import Types

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


transform :: (Eq a, Hashable a, Eq b, Hashable b) => (a -> b) -> FSM a -> FSM b
transform f fsm = fsm
  { delta = M.map (M.fromList . map (\(a, q) -> (f a, q)) . M.toList) $ delta fsm
  }

domain :: FST -> FSA
domain = transform fst

range :: FST -> FSA
range = transform snd

inverse :: FST -> FST
inverse = transform (\(u, v) -> (v, u))

identity :: FSA -> FST
identity = transform (\u -> (u, u))

allOver :: Alphabet -> FSA
allOver = star . unions . map (word . (:[]))

optionalReplace :: Alphabet -> FST -> FST
optionalReplace = replace' . identity . allOver
  where replace' ide fst = concatenate ide $ star $ concatenate fst ide
