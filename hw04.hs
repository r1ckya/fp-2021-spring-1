-- | Тип 'ChurchNumber' является представлением чисел Чёрча в Хаскеле.
-- 
data ChurchNumber = Zero | Succ ChurchNumber

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция!)
--   к 'Zero'. Это ровно то же самое, что было у нас в лямбда-исчислении.
--
churchThree :: ChurchNumber
churchThree = Succ (Succ (Succ Zero))

-- 1. Реализуйте функции succ, add, mult и pow для типа 'ChurchNumber'.
--    Также явно укажите типы этих функций. (0.5 б)

succ = undefined -- undefined — конструкция, которая имеет любой тип, но упадёт с ошибкой при попытки её вычислить.
                 -- Её удобно использовать тогда, когда вы пока не знаете, что написать в реализации функции,
                 -- но хотите скомпилировать файл.

add = undefined

mult = undefined

pow = undefined

-- 2. Реализуйте функцию prev (вычитание единицы) для типа 'ChurchNumber'.
--    Также явно укажите тип функции. (0.5 б)

prev = undefined

-- 3. Реализуйте функции (как функции в Хаскеле, а не как лямбда-абстракции!), имеющие заданный тип.

-- (0.1 б)
--
combB :: (a -> b) -> (b -> c) -> a -> c
combB = undefined

-- (0.25 б)
--
myEither :: (a -> c) -> (b -> c) -> Either a b -> c
myEither = undefined

-- (0.25 б)
--
mapList :: (a -> b) -> [a] -> [b]
mapList = undefined

-- (0.1 б)
--
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

-- (0.1 б)
--
mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither = undefined

-- (0.5 б)
-- В этом задании полезно будет вспомнить, что паттерн-матчиться можно по нескольким аргументам.
--
up :: [a -> b] -> [a] -> [b]
up = undefined

-- 4. Реализуйте функцию reverseLinear, которая будет разворачивать список.
--    Функция должна разворачивать список за один проход по нему.
--    Функция должна быть хвосторекурсивной.
--    Также явно укажите тип функции. (1 б)

reverseLinear = undefined

-- 5. Реализуйте функцию chunksOf, которая будет разбивать список
--    на подсписки заданного размера.
--    Функция должна быть хвосторекурсивной.
--    Также явно укажите тип функции. (1 б)

chunksOf = undefined

-- | Вспомним наш тип 'BinaryTree', который мы завели на лекции.
--
data BinaryTree a 
  = Leaf
  | Node 
    { nodeValue  :: a
    , leftChild  :: BinaryTree a
    , rightChild :: BinaryTree a
    }
  deriving (Show, Eq)

-- | Для примера заведём дерево строк.
--
binTreeOfStrings :: BinaryTree String
binTreeOfStrings = 
  Node "This" 
    (Node "is" 
      (Node "Tree" Leaf Leaf) 
      (Node "too" Leaf Leaf)
    ) 
    (Node "and" 
      (Node "don't" 
        (Node "forget" Leaf Leaf) 
        (Node "me!" Leaf Leaf)
      ) 
      Leaf
    )

-- | В Хаскеле можно создавать "синонимы типов". То есть некоторые сокращения
--   для уже сущсетвующих типов.
--   Например, мы можем сказать, что тип 'BinaryTree String', можно будет называть как 'StringTree'.
--
type StringTree = BinaryTree String

-- | Доказательство того, что 'StringTree' и 'BinaryTree String' — один и тот же тип:
--   вроде бы, binTreeOfStringsCopy должна вернуть что-то типа 'StringTree', а мы вернули
--   binTreeOfStrings, которая имеет тип 'BinaryTree String'. И компилятор на это не ругается!
--
binTreeOfStringsCopy :: StringTree
binTreeOfStringsCopy = binTreeOfStrings

-- В Хаскеле тип 'String' определён как синоним типа ['Char'].
-- Сооответственно, для строк работают все те же функции, что и для списков.
-- Например, length (длина списка), оператор (!!) (взятие элемента списка) и другие.

-- 6. Реализуйте функцию removeOdd, которая будет удалять из переданного ей 'StringTree'
--    все узлы, в которых хранятся строки чётной длины.
--    Не забудьте явно указать тип функции. Все вспомогательные функции пишите 
--    в секции where с явным указанием их типов. (1.5 б)

removeOdd = undefined

-- 7. Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--    число потомков. (0.25 б)

data Tree a = YourTree -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- 8. Реализуйте функции bfs и dfs, которые производят обход в глубину и в ширину дерева 'Tree'. (2 б)
--

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину.
--
bfs :: Tree a -> [a]
bfs = undefined

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину.
--
dfs :: Tree a -> [a]
dfs = undefined