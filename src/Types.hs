module Types where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashSet as S
import Data.Word (Word8)


type Alphabet = S.HashSet Word8
type Input = BS.ByteString
type Output = BS.ByteString

class Expandable a where
  shouldExpand :: a -> Bool
  size :: a -> Int
  expandLabel :: a -> [a]

instance Expandable Input where
  shouldExpand = (> 1) . BS.length
  size = fromIntegral . BS.length
  expandLabel = map BS.singleton . BS.unpack

instance Expandable (Input, Output) where
  shouldExpand (a, b) = shouldExpand a || shouldExpand b
  size (a, b) = max (size a) $ size b
  expandLabel (a, b) = take (size (a, b)) $
    zip (expandLabel a ++ repeat "") $ expandLabel b ++ repeat ""
