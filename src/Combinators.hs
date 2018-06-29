module Combinators where

import FSM
import FSA
import FST
import Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


intersect :: FSA -> FSA -> FSA
intersect = combine False

compose :: FST -> FST -> FST
compose = combine True

product :: FSA -> FSA -> FST
product = combine True

transform :: (Hash a, Hash b) => (a -> b) -> FSM a -> FSM b
transform f fsm = fsm
  { delta = M.map (M.fromList . map (\(a, b) -> (f a, b)) . M.toList) $ delta fsm
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
allOver = star . unions . map (word . BS.singleton) . S.toList

optionalReplace :: Alphabet -> FST -> FST
optionalReplace = replace' . identity . allOver

replace :: Alphabet -> FST -> FST
replace alphabet fst = replace' ide fst
  where ide = union (word mempty) $ identity $ complement' $
          concatenates [all', domain fst, all']
        complement' = complement . total alphabet . determinise
        all' = allOver alphabet

replace' :: FST -> FST -> FST
replace' ide fst = concatenate ide $ star $ concatenate fst ide
