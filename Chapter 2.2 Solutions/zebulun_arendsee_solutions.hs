module Chapter2
  ( accumulate
  , map'
  , append'
  , length'
  , horner
  , accumulateN
  , transpose
  , matrixVectorProd
  , matrixMatrixProd
  ) where

-- accumulate is a special case of foldr
accumulate :: (a -> b -> b) -> b -> [a] -> b
accumulate _ b [] = b
accumulate f b (x:rs) = f x (accumulate f b rs)

-- 2.33
map' p sequence = foldr (\x y -> p x : y) [] sequence

append' seq1 seq2 = foldr (:) seq2 seq1

length' sequence = foldr (\_ y -> y + 1) 0 sequence

-- 2.34
-- example:
--   given: 5 + 2x + x^3 evaluated at x=3
--   horner 3 [5, 2, 0, 1] = 38
horner x coefs = accumulate (\a b -> a + x * b) 0 coefs

-- 2.36
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xss =
  if any (\x -> length x == 0) xss
  then []
  else map head xss : (transpose (map tail xss)) 

accumulateN :: (a -> b -> b) -> b -> [[a]] -> [b]
accumulateN f b xss = map (accumulate f b) (transpose xss)

-- 2.37
matrixVectorProd :: Num a => [[a]] -> [a] -> [a]
matrixVectorProd xss xs = map (sum . zipWith (*) xs) xss

matrixMatrixProd :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMatrixProd mss nss = map (matrixVectorProd (transpose nss)) mss 
