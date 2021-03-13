{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- В Хаскеле есть тип данных 'Map k v', сопостовляющий каждому ключу
-- типа @k@ какое-то конкретное значение типа @v@.
import           Data.Array (Array, listArray, (!), (//))
import           Data.List (foldl')
import           Data.Map.Strict (Map, empty, insertWith, unionWith)
import           Data.Foldable (all, maximumBy, find)
import           Data.Ord (comparing)

-- 1. Реализуйте исключительно с помощью свёрток:
-- | 1.1. Функция, суммирующая квадраты элементов списка (0.1 б)
sumOfSquares :: Num a => [a] -> a
sumOfSquares = foldl' (\x y -> x + y * y) 0

-- >>> sumOfSquares [1, 2, 3]
-- 14
-- >>> sumOfSquares []
-- 0
-- >>> sumOfSquares [1.5]
-- 2.25
--
-- | 1.2. Функция разворачивания списка (0.25 б)
reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

-- >>> reverse' [1, 2, 3]
-- [3,2,1]
--
-- | 1.3. Функция, которая достаёт из списка элемент по индексу (0.25 б)
getByIndex :: [a] -> Int -> Maybe a
getByIndex a idx = fst $ foldl' helper (Nothing, 0) a
  where
    helper :: (Maybe a, Int) -> a -> (Maybe a, Int)
    helper (x, i) a
      | i == idx = (Just a, i + 1)
      | otherwise = (x, i + 1)

-- >>> getByIndex [1, 2, 3] 1
-- Just 2
-- >>> getByIndex [1, 2, 3] 3
-- Nothing
--
-- | Тип данных "Студент"
data Student =
  Student { name :: String  -- имя студента
          , grade :: Int -- оценка студента по нашему предмету
          }
  deriving (Show)

-- | Тип данных "Информация о студентах курса"
data StudentsLog =
  StudentsLog { studentNames :: [String]  -- список имён студентов
              , worstGrade :: Maybe Int  -- наименьшапя оценка по курсу
              , bestGrade :: Maybe Int -- наибольшая оценка по курсу
              }
  deriving (Show)

-- | 1.4. Функция, которая по списку студентов курса рассчитывает информацию по курсу (0.5 б)
maybeOp :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeOp op Nothing y = y
maybeOp op x Nothing = x
maybeOp op (Just x) (Just y) = Just $ op x y

maybeMin :: Maybe Int -> Maybe Int -> Maybe Int
maybeMin = maybeOp min

maybeMax :: Maybe Int -> Maybe Int -> Maybe Int
maybeMax = maybeOp max

calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog = foldr helper (StudentsLog [] Nothing Nothing)
  where
    helper :: Student -> StudentsLog -> StudentsLog
    helper student log = StudentsLog
      (name student:studentNames log)
      (maybeMin (worstGrade log) (Just $ grade student))
      (maybeMax (bestGrade log) (Just $ grade student))

-- >>> calculateStudentsLog [Student "A" 2, Student "B" 3, Student "C" 5]
-- StudentsLog {studentNames = ["A","B","C"], worstGrade = Just 2, bestGrade = Just 5}
-- >>> calculateStudentsLog []
-- StudentsLog {studentNames = [], worstGrade = Nothing, bestGrade = Nothing}
--
-- | 1.5. Функция, которая вставляет в уже отсортированный список элементов
--        новый элемент на такую позицию, что все элементы левее будут меньше или
--        равны нового элемента, а все элементы справа будут строго больше (1 б)
insert :: forall a. Ord a => [a] -> a -> [a]
insert xs y = case res of
  (True, x)  -> x
  (False, x) -> y:x
  where
    helper :: Ord a => a -> (Bool, [a]) -> (Bool, [a])
    helper z (was, xs)
      | not was && z <= y = (True, z:y:xs)
      | otherwise = (was, z:xs)

    res = foldr helper (False, []) xs

-- >>> insert [1, 2, 3] 100
-- >>> insert [1, 2, 3] 3
-- >>> insert [1, 2, 3] (-2)
-- >>> insert [] 5
-- >>> insert [1, 100, 101] 3
-- [1,2,3,100]
-- [1,2,3,3]
-- [-2,1,2,3]
-- [5]
-- [1,3,100,101]
--
-- | 1.6. Сортировка вставками. В реализации можно использовать функцию @insert@,
--        если вы её реализовали (0.5 б)
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' insert []

-- >>> insertionSort [3, 5, 1, 2, -3]
-- [-3,1,2,3,5]
-- >>> insertionSort [0, 1, 0, 1, 0, 1]
-- [0,0,0,1,1,1]
--
-- | 1.7. Функция zip. В секции where дана подсказка — вид инициального значения (1 б)
zip' :: forall a b. [a] -> [b] -> [(a, b)]
zip' = foldr helper ini
  where
    ini :: [b] -> [(a, b)]
    ini _ = []

    helper :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
    helper x fn [] = []
    helper x fn (y:ys) = (x, y):fn ys

-- >>> zip' [1, 2, 3, 2] [3, 2, 1]
-- [(1,3),(2,2),(3,1)]
-- >>> zip' [1, 2, 3] [3, 2, 1, 0]
-- [(1,3),(2,2),(3,1)]
--
-- | 2. Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--      @calculateStudentsLog'@, которая делает то же самое, что и @calculateStudentsLog@.
--      В реализации нужно использовать то, что 'StudentsLog' — моноид. (0.5 б)
instance Semigroup StudentsLog where
  log1 <> log2 = StudentsLog
    (studentNames log1 ++ studentNames log2)
    (maybeMax (bestGrade log1) (bestGrade log2))
    (maybeMin (worstGrade log1) (worstGrade log2))

instance Monoid StudentsLog where
  mempty = StudentsLog [] Nothing Nothing

studentToLog :: Student -> StudentsLog
studentToLog student =
  StudentsLog [name student] (Just $ grade student) (Just $ grade student)

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' xs = mconcat $ map studentToLog xs

-- >>> calculateStudentsLog [Student "A" 2, Student "B" 3, Student "C" 5]
-- >>> calculateStudentsLog []
-- StudentsLog {studentNames = ["A","B","C"], worstGrade = Just 2, bestGrade = Just 5}
-- StudentsLog {studentNames = [], worstGrade = Nothing, bestGrade = Nothing}
--
-- | Хорошо знакомый нам тип данных "Дерево"
data Tree a = Node a [Tree a]
            | Leaf
  deriving (Eq, Show)

-- 3. Сделайте 'Tree' представителем класса типов 'Foldable' (1 б)
--
instance Foldable Tree where
  -- foldr f z Leaf = z
  -- foldr f z (Node a []) = f a z
  -- foldr f z (Node a neighbours) = helper
  --   where
  --     helper = f a $ Prelude.foldr (flip $ foldr f) z neighbours
  --
  foldMap f Leaf = mempty
  foldMap f (Node a []) = f a
  foldMap f (Node a neighbours) = mconcat $ f a:map (foldMap f) neighbours

-- | Тип данных "Яблоко"
--
data Apple = Apple { color :: String  -- цвет яблока
                   , weight :: Float -- вес яблока
                   }
  deriving (Eq, Show)

-- | 4. С помощью функйций из 'Data.Foldable' реализуйте следующие функции:
--
appleTree :: Tree Apple
appleTree = Node
  (Apple "A" 2)
  [ Node (Apple "B" 2) [Node (Apple "C" 2) [], Node (Apple "A" 1) []]
  , Node (Apple "C" 3) []
  , Node (Apple "D" 5) []]

-- | 4.1. Проверка, что все яблоки в дереве имеют вес, который находится
--        в заданном диапазоне весов (0.1 б)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (low, high) =
  all (\Apple { weight = w } -> low <= w && w <= high) tree

--
-- applesInRange tree (low, high) = getAll $ foldMap helper tree
--   where
--     helper :: Apple -> All
--     helper Apple { weight = w } = All $ low <= w && w <= high
--
-- >>> applesInRange appleTree (1, 5)
-- >>> applesInRange appleTree (2, 3)
-- >>> applesInRange appleTree (3, 100)
-- True
-- False
-- False
--
-- | 4.2. Находит яблоко с наибольшим весом (0.1 б)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple tree = Just $ maximumBy (comparing weight) tree

--
-- heaviestApple = foldr helper Nothing
--   where
--     helper :: Apple -> Maybe Apple -> Maybe Apple
--     helper apple Nothing = Just apple
--     helper apple (Just acc)
--       | weight apple > weight acc = Just apple
--       | otherwise = Just acc
--
-- >>> heaviestApple appleTree
-- Just (Apple {color = "D", weight = 5.0})
-- >>> heaviestApple Leaf
-- Nothing
--
-- | 4.3 Находит яблоко с цветом из заданного списка цветов и весом,
--       находящимся в заданном диапазоне весов (0.1 б)
--
thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple
thisApple tree colors (low, high) =
  find (\(Apple c w) -> c `elem` colors && low <= w && w <= high) tree

--
-- thisApple tree colors (low, high) = foldr helper Nothing tree
--   where
--     helper :: Apple -> Maybe Apple -> Maybe Apple
--     helper apple@(Apple c w) Nothing
--       | c `elem` colors && low <= w && w <= high = Just apple
--       | otherwise = Nothing
--     helper apple (Just acc) = Just acc
--
-- >>> thisApple appleTree ["A"] (2, 3)
-- >>> thisApple appleTree ["A", "D"] (3, 5)
-- >>> thisApple appleTree [] (-1, 1)
-- >>> thisApple Leaf ["A"] (-1, 1)
-- >>> thisApple appleTree ["A", "B", "C", "D"] (-1, 0)
-- Just (Apple {color = "A", weight = 2.0})
-- Just (Apple {color = "D", weight = 5.0})
-- Nothing
-- Nothing
-- Nothing
--
-- | 4.4 Считает сумму весов всех яблок в дереве.
--       В реализации нужно использовать 'Data.Foldable.sum' (0.25 б)
--
-- sum :: Num a => t a -> a
-- but Apple is not instance of Num
-- so we can't apply sum directly
--
sumOfApples :: Tree Apple -> Float
sumOfApples = foldr (\apple x -> weight apple + x) 0

--
-- sumOfApples tree = getSum $ foldMap (Sum . weight) tree
-- sumOfApples tree = sum $ concatMap (\Apple { weight = w } -> [w]) tree
--
-- >>> sumOfApples appleTree
-- >>> sumOfApples Leaf
-- 15.0
-- 0.0
--
-- | Корзинка с яблоками.
--   Важно, что яблоки в корзинке расфасованы по цветам.
--   Для каждого цвета яблоки упорядочены по весу
--
newtype Basket = Basket { apples :: Map String [Apple] }
  deriving (Eq, Show)

-- | 5. Реализуйте с помощью свёртки дерева функцию, которая соберёт
--      по дереву яблок корзинку с яблоками.
--      В 'Data.Map.Strict' вы найдёте функции, которые помогут вам
--      инициализировать и модифицировать мапу (0.5 б)
--
collectBasket :: Tree Apple -> Basket
collectBasket Leaf = Basket empty
collectBasket (Node apple neighbours) = Basket
  $ insertWith merge (color apple) [apple]
  $ foldl' (flip $ unionWith merge . apples . collectBasket) empty neighbours
  where
    merge :: [Apple] -> [Apple] -> [Apple]
    merge [] ys = ys
    merge xs [] = xs
    merge allx@(x:xs) ally@(y:ys)
      | weight x < weight y = x:merge xs ally
      | otherwise = y:merge allx ys

-- >>> collectBasket appleTree
-- Basket {apples = fromList [("A",[Apple {color = "A", weight = 1.0},Apple {color = "A", weight = 2.0}]),("B",[Apple {color = "B", weight = 2.0}]),("C",[Apple {color = "C", weight = 2.0},Apple {color = "C", weight = 3.0}]),("D",[Apple {color = "D", weight = 5.0}])]}
--
-- | Двоичная куча (https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча)
--
data BinaryHeap a =
    BinNode { val :: a, left :: BinaryHeap a, right :: BinaryHeap a }
  | BinLeaf
  deriving (Eq, Show)

-- | 6.1. Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0.5 б)
--
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf
siftDown node@(BinNode a leftNode rightNode) = case (leftNode, rightNode) of
  (BinLeaf, BinLeaf) -> node
  (BinNode leftVal leftLeft leftRight, BinLeaf)
    | a < leftVal -> BinNode leftVal newLeft BinLeaf
    | otherwise -> node
    where
      newLeft = siftDown $ BinNode a leftLeft leftRight
  (BinLeaf, BinNode rightVal rightLeft rightRight)
    | a < rightVal -> BinNode rightVal BinLeaf newRight
    | otherwise -> node
    where
      newRight = siftDown $ BinNode a rightLeft rightRight
  (BinNode leftVal leftLeft leftRight, BinNode rightVal rightLeft rightRight)
    | leftVal > a && leftVal > rightVal -> BinNode leftVal newLeft rightNode
    | rightVal > a && rightVal > leftVal -> BinNode rightVal leftNode newRight
    | otherwise -> node
    where
      newRight = siftDown $ BinNode a rightLeft rightRight

      newLeft = siftDown $ BinNode a leftLeft leftRight

heap :: BinaryHeap Int
heap = BinNode
  1
  (BinNode 2 BinLeaf BinLeaf)
  (BinNode 3 (BinNode 4 BinLeaf BinLeaf) BinLeaf)

-- >>> siftDown heap
-- BinNode {val = 3, left = BinNode {val = 2, left = BinLeaf, right = BinLeaf}, right = BinNode {val = 4, left = BinNode {val = 1, left = BinLeaf, right = BinLeaf}, right = BinLeaf}}
--
-- | 6.2. Реализуйте с помощью свёртки (которая уже написана в коде)
--        функцию buildHeap, которая за __линейное время__ конструирует
--        на основе спиcка элементов бинарную кучу.
--        Соответствующий алогритм описан в статье на вики (ссылка выше).
--        Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!) (1 б)
--
makeHeap :: a -> BinaryHeap a
makeHeap a = BinNode a BinLeaf BinLeaf

-- >>> makeHeap 128
-- >>> makeHeap "ABC"
-- BinNode {val = 128, left = BinLeaf, right = BinLeaf}
-- BinNode {val = "ABC", left = BinLeaf, right = BinLeaf}
--
myBuildHeap :: forall a. Ord a => [a] -> BinaryHeap a
myBuildHeap lst = build 1
  where
    len = length lst

    halfLen = len `div` 2

    arr :: Array Int a
    arr = listArray (1, len) lst

    build :: Int -> BinaryHeap a
    build i
      | i <= halfLen = siftDown
        $ BinNode (arr ! i) (build $ 2 * i) (build $ 2 * i + 1)
      | i <= len = makeHeap (arr ! i)
      | otherwise = BinLeaf

buildHeap :: forall a. Ord a => [a] -> BinaryHeap a
buildHeap lst = heapifyed ! 1
  where
    len = length lst

    arr :: Array Int (BinaryHeap a)
    arr = listArray (1, len) $ map makeHeap lst

    heapify :: Int -> Array Int (BinaryHeap a) -> Array Int (BinaryHeap a)
    heapify i arr = arr // updates
      where
        iLeft = 2 * i

        iRight = 2 * i + 1

        curVal = val $ arr ! i

        -- left is always in arr
        leftHeap :: BinaryHeap a
        leftHeap = arr ! iLeft

        rightHeap :: BinaryHeap a
        rightHeap
          | iRight <= len = arr ! iRight
          | otherwise = BinLeaf

        newHeap :: BinaryHeap a
        newHeap = siftDown $ BinNode curVal leftHeap rightHeap

        -- remove merged nodes, left is always in arr
        updates :: [(Int, BinaryHeap a)]
        updates
          | iRight <= len = [(i, newHeap), (iLeft, BinLeaf), (iRight, BinLeaf)]
          | otherwise = [(i, newHeap), (iLeft, BinLeaf)]

    heapifyed :: Array Int (BinaryHeap a)
    heapifyed = foldr heapify arr [1 .. len `div` 2]

-- >>> buildHeap [1 .. 6]
-- >>> myBuildHeap [1 .. 6]
-- >>> buildHeap [1 .. 6] == myBuildHeap [1 .. 6]
-- >>> buildHeap [6, 5 .. 1]
-- >>> myBuildHeap [6, 5 .. 1]
-- >>> buildHeap [6, 5 .. 1] == myBuildHeap [6, 5 .. 1]
-- BinNode {val = 6, left = BinNode {val = 5, left = BinNode {val = 4, left = BinLeaf, right = BinLeaf}, right = BinNode {val = 2, left = BinLeaf, right = BinLeaf}}, right = BinNode {val = 3, left = BinNode {val = 1, left = BinLeaf, right = BinLeaf}, right = BinLeaf}}
-- BinNode {val = 6, left = BinNode {val = 5, left = BinNode {val = 4, left = BinLeaf, right = BinLeaf}, right = BinNode {val = 2, left = BinLeaf, right = BinLeaf}}, right = BinNode {val = 3, left = BinNode {val = 1, left = BinLeaf, right = BinLeaf}, right = BinLeaf}}
-- True
-- BinNode {val = 6, left = BinNode {val = 5, left = BinNode {val = 3, left = BinLeaf, right = BinLeaf}, right = BinNode {val = 2, left = BinLeaf, right = BinLeaf}}, right = BinNode {val = 4, left = BinNode {val = 1, left = BinLeaf, right = BinLeaf}, right = BinLeaf}}
-- BinNode {val = 6, left = BinNode {val = 5, left = BinNode {val = 3, left = BinLeaf, right = BinLeaf}, right = BinNode {val = 2, left = BinLeaf, right = BinLeaf}}, right = BinNode {val = 4, left = BinNode {val = 1, left = BinLeaf, right = BinLeaf}, right = BinLeaf}}
-- True
--
main :: IO Int
main = return 0
