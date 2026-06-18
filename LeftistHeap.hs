module LeftistHeap where

data Heap a = Nil | Node Int a (Heap a) (Heap a)

rank :: Heap a -> Int
rank Nil = 0
rank (Node r _ _ _) = r

makeNode :: Ord a => a -> Heap a -> Heap a -> Heap a
makeNode x a b
  | rank a >= rank b = Node (rank b + 1) x a b
  | otherwise = Node (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2)
  | x <= y = makeNode x a1 (merge b1 h2)
  | otherwise = makeNode y a2 (merge h1 b2)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Node 1 x Nil Nil)

findMin :: Heap a -> Maybe a
findMin Nil = Nothing
findMin (Node _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Nil = Nil
deleteMin (Node _ _ a b) = merge a b

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _ = False