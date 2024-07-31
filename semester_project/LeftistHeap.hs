module LeftistHeap (LeftistHeap, empty, insert, extractMin, isEmpty) where

data LeftistHeap a = Empty | Node Int a (LeftistHeap a) (LeftistHeap a)
  deriving (Show)

rank :: LeftistHeap a -> Int
rank Empty = 0
rank (Node r _ _ _) = r

makeNode :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeNode x a b
  | rank a >= rank b = Node (rank b + 1) x a b
  | otherwise        = Node (rank a + 1) x b a

merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge h Empty = h
merge Empty h = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2)
  | x <= y    = makeNode x a1 (merge b1 h2)
  | otherwise = makeNode y a2 (merge h1 b2)

empty :: LeftistHeap a
empty = Empty

isEmpty :: LeftistHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert x h = merge (Node 1 x Empty Empty) h

extractMin :: Ord a => LeftistHeap a -> (a, LeftistHeap a)
extractMin Empty = error "extractMin: empty heap"
extractMin (Node _ x a b) = (x, merge a b)
