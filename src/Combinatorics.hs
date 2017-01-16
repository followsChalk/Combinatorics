module Combinatorics
 (c, c_, v, v_ )
 where

import Data.Monoid

data Type = C | C_ | V | V_

f :: (Eq a) => Type ->  Int -> [a] -> [[a]]
f _ 0 xs     = [[]]
f _ i []     = []
f C  i (x:xs) = ( (x:) <$> f C  (i-1) xs     )  <>  f C  i xs
f C_ i (x:xs) = ( (x:) <$> f C_ (i-1) (x:xs) )  <>  f C_ i xs
f V  i xs     = concatMap (fmap . (:) <$> id <*> f V (i-1) . (flip filter xs . (/=))) xs
f V_ i xs     = concatMap (flip fmap (f V_ (i-1) xs) . (:) ) xs


c, c_, v, v_ :: (Eq a) =>  Int -> [a] -> [[a]]
c = f C;      c_= f C_;      v = f V;      v_ = f V_


