module DPC.Util
  ( oneOfP
  , oneOf
  , snoc
  , groupsOf
  , pickRandom
  )
where

import           Control.Applicative
import           System.Random

-- return one element from an alternative and the remaining elements as well
oneOfP :: Alternative m => (a -> Bool) -> [a] -> m (a, [a])
oneOfP _ [] = empty
oneOfP p (x : xs) =
  (if p x then pure (x, xs) else empty) <|> (replace <$> oneOfP p xs)
  where replace (a, ys) = (a, x : ys)

-- apply oneOf with an always true predicate
oneOf :: Alternative m => [a] -> m (a, [a])
oneOf = oneOfP (const True)

-- add element to tail of list
snoc :: [a] -> a -> [a]
snoc xs a = foldr (:) [a] xs

-- create groups of elements frmo a given list
groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = case splitAt n xs of
  (grp, [] ) -> [grp]
  (grp, xs') -> grp : groupsOf n xs'

-- pick a random element from a list
pickRandom :: [a] -> IO a
pickRandom xs = (xs !!) <$> randomRIO (0, length xs - 1)
