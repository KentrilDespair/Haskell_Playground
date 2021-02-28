module Tree
( Tree(..)
, singleton
, treeInsert
, treeElem
, fromList
, inorder
, preorder
, postorder
) where

-- An empty tree or has a value and two subtrees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- A singleton tree is a tree with just one node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- newTree = treeInsert 5 root
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- Check whether an element is in a tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- Create a tree from a list of values
fromList :: (Ord a) => [a] -> Tree a
fromList = foldr treeInsert EmptyTree

-- Tree traversals
-- TODO using more general function
inorder :: (Ord a) => Tree a -> [a]
inorder EmptyTree = []
inorder (Node a left right) = inorder left ++ a : inorder right

preorder :: (Ord a) => Tree a -> [a]
preorder EmptyTree = []
preorder (Node a left right) = a : preorder left ++ preorder right

postorder :: (Ord a) => Tree a -> [a]
postorder EmptyTree = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

-- Returns a node with that value
find :: (Ord a) => a -> Tree a -> Maybe (Tree a)
find x EmptyTree = Nothing
find x tree@(Node a left right)
    | x == a = Just tree
    | x < a = search x left
    | x > a = search x right

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)



