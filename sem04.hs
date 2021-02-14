data List a = Nil | Cons a (List a)
  deriving (Show)

-- error :: String -> any

-- Голова списка
head' :: List a -> a
head' Nil        = error "No head."
head' (Cons h t) = h

listOf123 :: List Int
listOf123 = Cons 1 (Cons 2 (Cons 3 Nil))

-- Хвост списка
tail' :: List a -> List a
tail' Nil        = error "No tail."
tail' (Cons h t) = t

-- Третий элемент списка
third :: List a -> a
third (Cons x (Cons y (Cons z t))) = z
third _                            = error "No third."

-- Добавляем элемент в конец списка
append :: List a -> a -> List a
append Nil a        = Cons a Nil
append (Cons x t) a = Cons x (append t a)

listOf123' :: [Int]
listOf123' = [1, 2, 3]

-- class Node:
--   def __init__(self, value, left, right):
--     self.value = value
--     self.left = left
--     self.right = right
--
data BinaryTree a 
  = Leaf
  | Node 
    { nodeValue  :: a
    , leftChild  :: BinaryTree a
    , rightChild :: BinaryTree a
    }
  deriving (Show, Eq)

-- Node(5, Node(30, None, None), Node(239, Node(7, None, None), Node(8, None, None))))
--
myBinaryTree :: BinaryTree Int
myBinaryTree = Node 5 (Node 30 Leaf Leaf) (Node 239 (Node 7 Leaf Leaf) (Node 8 Leaf Leaf))

-- | Собирает все значения из листьев дерева.
--
collectAllLeafValues :: BinaryTree Int -> [Int]
collectAllLeafValues Leaf               = []
collectAllLeafValues (Node v Leaf Leaf) = [v]
collectAllLeafValues (Node v l r)       = leftVals <> rightVals
  where
    leftVals  = collectAllLeafValues l
    rightVals = collectAllLeafValues r

-- Построение бинарного дерева поиска.
--
buildBST :: [Int] -> BinaryTree Int
buildBST l = helper Leaf l
  where
    helper :: BinaryTree Int -> [Int] -> BinaryTree Int
    helper tree []       = tree
    helper tree (x : xs) = helper (insert tree x) xs

    insert :: BinaryTree Int -> Int -> BinaryTree Int
    insert Leaf val            = Node val Leaf Leaf
    insert (Node val' l r) val = 
      if val < val' 
        then Node val' (insert l val) r 
        else Node val' l (insert r val)