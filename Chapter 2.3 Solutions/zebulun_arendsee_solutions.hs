{-# LANGUAGE ViewPatterns #-}

module Chapter2 where

import Data.List (sort)

-- 2.62 -----------------------------------------------------------------------
unionOrdered :: Ord a => [a] -> [a] -> [a]
unionOrdered xs [] = xs
unionOrdered [] ys = ys
unionOrdered (x:xs) (y:ys)
 | x == y = x : unionOrdered xs ys
 | x < y = x : unionOrdered xs (y:ys)
 | x > y = y : unionOrdered (x:xs) ys

-- 2.65 -----------------------------------------------------------------------

-- NOTE: this is NOT an ordered tree
data Tree a = Tree a (Tree a) (Tree a) | Nil 
  deriving (Show, Ord, Eq)

instance Functor Tree where
  -- Mapping of the tree may cause loss of order. I can't reorder the tree here
  -- without a sorting step. I can't sort since Tree's parameter is not
  -- necessarily in Ord. Also, reordering would violate the functor laws.
  fmap f (Tree x lhs rhs) = Tree (f x) (fmap f lhs) (fmap f rhs)
  fmap _ Nil = Nil

instance Foldable Tree where
  foldMap _ Nil = mempty
  foldMap f (Tree x lhs rhs) = f x <> foldMap f lhs <> foldMap f rhs

singleton :: a -> Tree a
singleton x = Tree x Nil Nil

-- assumes the tree is ordered
elementOfSet :: (Ord a) => a -> Tree a -> Bool
elementOfSet x Nil = False
elementOfSet x (Tree y lhs rhs)
  | x == y = True
  | x < y = elementOfSet x lhs
  | x > y = elementOfSet x rhs

-- assumes the tree is ordered
adjoinSet :: (Ord a) => a -> Tree a -> Tree a
adjoinSet x Nil = singleton x
adjoinSet x t@(Tree y lhs rhs)
  | x == y = t -- The element is already in the tree, so return the original tree 
  | x < y = Tree y (adjoinSet x lhs) rhs
  | x > y = Tree y lhs (adjoinSet x rhs)

-- preserves input tree order, so an ordered tree will produce an ordered list
toList :: Tree a -> [a]
toList Nil = []
toList (Tree x lhs rhs) = toList lhs ++ (x : toList rhs)

-- preserves input list order, an ordered list produces an ordered tree
fromList :: [a] -> Tree a
fromList [] = Nil
fromList xs = Tree middle (fromList left) (fromList right) where
  k = div (length xs) 2   -- FIXME: I shouldn't recalculate this
  middle = xs !! k        -- the middle element in the list
  left = take k xs        -- all elements on the left
  right = drop (k + 1) xs -- all elements on the right

balance :: Tree a -> Tree a
balance = fromList . toList

unionSet :: (Ord a) => Tree a -> Tree a -> Tree a
unionSet x = adjoinMany (toList x) where
  adjoinMany [] tree = tree
  adjoinMany (x:rs) tree = adjoinMany rs (adjoinSet x tree)

intersectSet :: (Ord a) => Tree a -> Tree a -> Tree a
intersectSet t1 t2 = fromList [x | x <- toList t1, elementOfSet x t2]



-- HUFFMAN TREES -----------------------------------------------------------

-- This is a bad representation, but it will look pretty ...
type Binary = String
type HuffmanTree w v = Tree (w, Tree v)


-- remove the leftmost element from a tree
pop :: Tree a -> (a, Tree a)
pop Nil = error "You shouldn't have done that"
pop (Tree x Nil rhs) = (x, rhs) 
pop (Tree x (pop -> (y, lhs)) rhs) = (y, Tree x lhs rhs)


-- remove the N leftmost elements from a tree
-- return the elements and the pruned tree
popN :: Int -> Tree a -> ([a], Tree a)
popN 0 t = ([], t)
popN _ Nil = ([], Nil)
popN i t = case pop t of 
  (x, t2) -> case popN (i-1) t2 of
    (xs, t3) -> (x:xs, t3)


-- w := Element weight, can be any numeric type
-- v := Element type
-- The Ord constraint on element type could be removed. This would technically
-- make the data structure more general, however, almost everything is
-- orderable, so the generallity would probably not be worth the loss in
-- efficiency.
makeHuffmanTree :: (Num w, Ord w, Ord v) => [(w, v)] -> HuffmanTree w v
makeHuffmanTree
  = makeTree
  . fromList
  . map (\(w,v) -> singleton (w, singleton v))
  . sort
  where
  -- combine a tree of singleton Huffman trees into a single Huffman tree
  makeTree :: (Num w, Ord w, Ord v) => Tree (HuffmanTree w v) -> HuffmanTree w v
  makeTree t = case popN 2 t of
    ([], Nil) -> Nil
    -- all done, all characters have been combined into one
    ([final], Nil) -> final
    -- combine two characters and push them into the tree
    ([lhs, rhs], t') -> makeTree (adjoinSet (combine lhs rhs) t')

  -- combine two Huffman trees
  combine :: (Num w, Ord w, Ord v) => HuffmanTree w v -> HuffmanTree w v -> HuffmanTree w v
  combine Nil x = x
  combine x Nil = x
  combine lhs@(Tree (w1, v1) _ _) rhs@(Tree (w2, v2) _ _) = Tree (w1+w2, unionSet v1 v2) rhs lhs


encodeHuffman :: (Ord v) => HuffmanTree w v -> [v] -> Binary
encodeHuffman t = concat . map (encodeHuffmanChar t)


encodeHuffmanChar :: (Ord v) => HuffmanTree w v -> v -> Binary
encodeHuffmanChar (Tree (_, Tree v' Nil Nil) _ _) v
 | v == v' = ""
 | otherwise = error "Illegal character"
encodeHuffmanChar (Tree (_, vs) lhs rhs) v
  | isIn lhs v = '0' : (encodeHuffmanChar lhs v)
  | isIn rhs v = '1' : (encodeHuffmanChar rhs v)
  | otherwise = error "Illegal character"
  where
    isIn :: (Ord v) => HuffmanTree w v -> v -> Bool
    isIn (Tree (_, vs) _ _) v = elementOfSet v vs
encodeHuffmanChar _ _ = error "Illegal character"


decodeHuffman :: HuffmanTree w v -> Binary -> [v]
decodeHuffman _ [] = []
decodeHuffman tree bits  = case decodeChar tree bits of
  (v, bits2) -> v : decodeHuffman tree bits2
  where
    decodeChar :: HuffmanTree w v -> Binary -> (v, Binary)
    decodeChar (Tree (_, Tree v Nil Nil) Nil Nil) bits = (v, bits)
    decodeChar (Tree _ lhs _) ('0':bits) = decodeChar lhs bits
    decodeChar (Tree _ _ rhs) ('1':bits) = decodeChar rhs bits
    decodeChar _ _ = error "Bad tree"

{- Example:
  t = makeHuffmanTree [(8, 'A'), (3, 'B'), (1, 'C'), (1, 'D'), (1, 'E'), (1, 'F'), (1, 'G'), (1, 'H')]
  decodeHuffman t (encodeHuffman t "ABCDEFGH")
-}
