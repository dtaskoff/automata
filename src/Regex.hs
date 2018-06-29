module Regex where

import qualified FSM
import Types


data Regex a = Word a
             | Union (Regex a) (Regex a)
             | Concatenate (Regex a) (Regex a)
             | Star (Regex a)
  deriving Show

unions :: [Regex a] -> Regex a
unions = foldr1 Union

concatenates :: [Regex a] -> Regex a
concatenates = foldr1 Concatenate

fromList :: [a] -> Regex a
fromList = unions . map Word

toFSM :: (Hash a, Monoid a) => Regex a -> Maybe (FSM.FSM a)
toFSM (Word w) = Just $ FSM.word w
toFSM (Union regex regex') = do
  fsm <- toFSM regex
  fsm' <- toFSM regex'
  pure $ FSM.union fsm fsm'
toFSM (Concatenate regex regex') = do
  fsm <- toFSM regex
  fsm' <- toFSM regex'
  pure $ FSM.concatenate fsm fsm'
toFSM (Star regex) = FSM.star <$> toFSM regex
