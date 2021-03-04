{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

-- С помощью таких конструкций можно включать разные расширения языка.
-- Поподробнее с тем, что это такое, мы познакомимся чуть позже.
-- Но, как вы могли догадаться, для выполнения домашки нам понадобились какие-то расширения!
import           Data.Array (Array, Ix)
import qualified Data.Array as A ((!), (//), array, bounds, indices, listArray)
import qualified Data.Ix as Ix (inRange)

-- | Класс типов 'Indexable' характеризует структуры данных,
--   к элементам которых можно обращаться по индексу.
--
--   Важно, что характеризует он именно какую-то __структуру__ данных @c@.
--   @c@ может быть 'List', 'Matrix' или 'Array ind' (у 'Array' два типовых
--   параметра — тип индекса, т.е. @ind@, и тип значений в массиве).
--
class Indexable c where
  -- | Специальный синтаксис для определения каких-то типов,
  --   связанных с @c@ и необходимых для того, чтобы он мог быть представителем данного класса типов.
  --   Тут мы говорим, что c @c@ связан какой-то тип, являющийся в нём индексом.
  --
  --   Это нужно для того, чтобы зафиксировать разные индексы для разных структур данных.
  --   Понятно, что для 'List' индексом является 'Int'.
  --   При этом для 'Matrix' индексом является уже пара интов: (Int, Int).
  --   Для 'Array ind' индексом неожиданным образом является @ind@!
  --
  --   Тип @Index c@ называется __ассоциированным__ типом для типа @c@.
  --
  type Index c

  -- | Достаёт из структуры @c a@ значение по индексу @Index c@.
  --   Если индекс находится за границами структуры, то возвращает Nothing.
  --
  --   Смотрите, как круто! Запись 'Index c' подразумевает, что вторым аргументом
  --   в @get@ был передан тип, соответствующий индексу @c@.
  --   То есть при задании класса типов у нас есть возможность абстрагироваться от того,
  --   что это за конкретный индекс: 'Int', (Int, Int) или что-то другое...
  --
  get :: c a -> Index c -> Maybe a

  -- | То же самое, что и @get@. Но если индекс находится за границами,
  --   то падает с ошибкой.
  --
  getUnsafe :: c a -> Index c -> a
  getUnsafe x i = helper $ get x i
    where
      helper Nothing = error "Index out of range"
      helper (Just x) = x

  -- | Заменяет в структуре @c a@ значение по индексу @Index c@ на @a@.
  --   Возвращает изменённую структуру.
  --   Если индекс находится за границами структуры, то возвращает Nothing.
  --
  --   ВАЖНО: функции, которые каким-либо образом меняют объекты в Хаскеле,
  --          возвращают __новые__ версии этих объектов, содержащие изменения.
  --          При этом те объекты, которые мы меняли изначально, никак не
  --          поменяются, потому что у нас в языке всё неизменяемое.
  --
  update :: c a -> Index c -> a -> Maybe (c a)

  -- | То же самое, что и @update@. Но если индекс находится за границами,
  --   то падает с ошибкой.
  --
  updateUnsafe :: c a -> Index c -> a -> c a
  updateUnsafe x i v = helper $ update x i v
    where
      helper Nothing = error "Index out of range"
      helper (Just x) = x

  -- | Возвращает список всех индексов в структуре @c a@.
  --   Порядок — от первого индекса до последнего.
  --
  indices :: c a -> [Index c]

-- 1. Добавьте в класс типов 'Indexable' "реализации по умолчанию" (см. лекцию)
--    для функций @getUnsafe@ и @updateUnsafe@. (0.1 б)
-- 2. Сделайте 'List' (который []), 'Array' и 'Matrix' представителями
--    класса типов 'Indexable'. (0.5 б)
-- | Пример того, как задать ассоциированный тип 'Index'
--   для конкретного типа 'List'.
--
instance Indexable [] where
  type Index [] = Int

  get xs i
    | length xs > i = Just $ xs !! i
    | otherwise = Nothing

  update xs i v
    | length xs > i = Just $ take i xs ++ v:drop (i + 1) xs
    | otherwise = Nothing

  indices xs = [0 .. length xs - 1]

-- >>> get [1, 2, 3] 2
-- >>> x = [1, 3 .. 5]
-- >>> x
-- >>> indices x
-- >>> x = [1, 3 .. 5]
-- >>> update x 1 100
-- Just 3
-- [1,3,5]
-- [0,1,2]
-- Just [1,100,5]
instance Ix ind => Indexable (Array ind) where
  type Index (Array ind) = ind

  get arr ix
    | Ix.inRange (A.bounds arr) ix = Just $ arr A.! ix
    | otherwise = Nothing

  update arr ix v
    | Ix.inRange (A.bounds arr) ix = Just $ arr A.// [(ix, v)]
    | otherwise = Nothing

  indices = A.indices

-- >>> x = A.array ((0, 0), (2, 2)) [((i, j), 2) | i <- [0..2], j <- [0..2]]
-- >>> x
-- >>> y = updateUnsafe x (0, 0) 1000
-- >>> get x (0, 0)
-- >>> get y (0, 0)
-- array ((0,0),(2,2)) [((0,0),2),((0,1),2),((0,2),2),((1,0),2),((1,1),2),((1,2),2),((2,0),2),((2,1),2),((2,2),2)]
-- Just 2
-- Just 1000
  -- 3. Определите новый тип 'Seq a', который является обёрткой над 'Array Int a'.
  --    Сделайте его представителем класса типов 'Indexable'. (0.25 б)
type Seq a = Array Int a

charSeq :: Seq Char
charSeq = A.array (0, 5) $ zip [0 ..] ['a', 'b', 'c', 'd', 'e', 'f']

-- >>> charSeq
-- >>> indices charSeq
-- >>> get charSeq 3
-- array (0,5) [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f')]
-- [0,1,2,3,4,5]
-- Just 'd'
  -- 4. Перепешите функцию @align@ с практики так, чтобы она выравнивала
  --    друг на друга две любые 'Indexable'-структуры данных. (0.25 б)
  ------------------------------------------------------------------------------------
  -- Ура! Теперь мы можем выравнивать [DNA] на [DNA], Seq RNA на Seq RNA и так далее!
  -- И всё это без переписывания функции @align@: она по умолчанию работает для всех
  -- 'Indexable'-структур данных, наполненных значениями 'Scorable'-типа.
  --
  -- Мы можем даже выравнивать Matrix DNA на Matrix DNA, но зачем...
  ------------------------------------------------------------------------------------
  -- 5. Перепишите функцию @align@ так, чтобы она выдавала не только скор выравнивания,
  --    но и сам результат выравнивания. (2 б)
data DNA = A
         | T
         | G
         | C
  deriving (Eq, Show)

-- | Создадим тип, соответствующий РНК-нуклеотидам.
--
data RNA = A'
         | U'
         | G'
         | C'
  deriving (Eq, Show)

-- | Класс типов 'Scorable' выражает свойство объектов какого-то
--   типа @a@ быть сравниваемыми. Причём результат сравнения — какой-то скор.
--
class Scorable a where
  score :: a -> a -> Float

-- | Делаем тип 'DNA' представителем класса типов 'Scorable'.
--
instance Scorable DNA where
  -- скоры для DNA взяты из матрицы NUC44
  score A A = 5
  score T T = 5
  score G G = 5
  score C C = 5
  score _ _ = -4

-- | Делаем тип 'RNA' представителем класса типов 'Scorable'.
--
instance Scorable RNA where
  -- скоры для RNA взяты из матрицы NUC44
  score A' A' = 5
  score U' U' = 5
  score G' G' = 5
  score C' C' = 5
  score _ _ = -4

instance Scorable Char where
  score x y
    | x == y = 5
    | otherwise = -4

align
  :: (Scorable a, Indexable c) => c a -> c a -> (Float, ([Maybe a], [Maybe a]))
align seq1 seq2 = (getUnsafe dp (n, m), backward (n, m) ([], []))
  where
    flattenSeq1 = map (getUnsafe seq1) (indices seq1)

    flattenSeq2 = map (getUnsafe seq2) (indices seq2)

    n = length flattenSeq1

    m = length flattenSeq2

    dp_init =
      A.array ((0, 0), (n, m)) [((i, j), 0) | i <- [0 .. n], j <- [0 .. m]]

    dp = forward dp_init (indices dp_init)

    -- build score matrix with dp
    forward dp [] = dp
    forward dp ((i, j):xs) = forward dp' xs
      where
        dp' = updateUnsafe dp (i, j) curCost

        curCost
          | i == 0 && j == 0 = 0
          | i == 0 = fromIntegral j * gapCost
          | j == 0 = fromIntegral i * gapCost
          | otherwise = maximum [insCost, delCost, takeCost]

        insCost = getUnsafe dp (i - 1, j) + gapCost

        delCost = getUnsafe dp (i, j - 1) + gapCost

        takeCost = getUnsafe dp (i - 1, j - 1)
          + score (flattenSeq1 !! (i - 1)) (flattenSeq2 !! (j - 1))

    -- trace back, resrotre alignment
    backward (i, 0) (acc1, acc2) = (acc1', acc2')
      where
        acc1' = map Just (take i flattenSeq1) ++ acc1

        acc2' = replicate i Nothing ++ acc2
    backward (0, j) (acc1, acc2) = (acc1', acc2')
      where
        acc1' = replicate j Nothing ++ acc1

        acc2' = map Just (take j flattenSeq2) ++ acc2
    backward (i, j) (acc1, acc2)
      | curCost == insCost = backward (i - 1, j) (accTake1, Nothing:acc2)
      | curCost == delCost = backward (i, j - 1) (Nothing:acc1, accTake2)
      | otherwise = backward (i - 1, j - 1) (accTake1, accTake2)
      where
        curCost = getUnsafe dp (i, j)

        insCost = getUnsafe dp (i - 1, j) + gapCost

        delCost = getUnsafe dp (i, j - 1) + gapCost

        takeCost = getUnsafe dp (i - 1, j - 1)
          + score (flattenSeq1 !! (i - 1)) (flattenSeq2 !! (j - 1))

        accTake1 = Just (flattenSeq1 !! (i - 1)):acc1

        accTake2 = Just (flattenSeq2 !! (j - 1)):acc2

    gapCost :: Float
    gapCost = -1

-- >>> align "ABA" "CABA"
-- >>> align "TTGA" "ATTG"
-- (14.0,([Nothing,Just 'A',Just 'B',Just 'A'],[Just 'C',Just 'A',Just 'B',Just 'A']))
-- (13.0,([Nothing,Just 'T',Just 'T',Just 'G',Just 'A'],[Just 'A',Just 'T',Just 'T',Just 'G',Nothing]))
class SymbolReadable a where
  toSymbol :: Char -> a

instance SymbolReadable DNA where
  toSymbol 'A' = A
  toSymbol 'G' = G
  toSymbol 'T' = T
  toSymbol 'C' = C
  toSymbol _ = error "Can't parse DNA"

instance SymbolReadable RNA where
  toSymbol 'A' = A'
  toSymbol 'G' = G'
  toSymbol 'U' = U'
  toSymbol 'C' = C'
  toSymbol _ = error "Can't parse RNA"

-- 6.2 Реализуйте функцию 'toSeq' прнимающую строку, а возвращающую 'Seq' элементов типа,
--     являющегося представителем класса типов 'SymbolReadable'. (0.1 б)
createSeq :: [a] -> Seq a
createSeq xs = A.array (0, length xs - 1) (zip [0 ..] xs)

toSeq :: SymbolReadable a => String -> Seq a
toSeq s = createSeq $ map toSymbol s

dnaSeq :: Seq DNA
dnaSeq = toSeq "ATGAG"

-- >>> dnaSeq
-- array (0,4) [(0,A),(1,T),(2,G),(3,A),(4,G)]
dnaSeq2 :: Seq DNA
dnaSeq2 = toSeq "AUGUG"

-- >>> dnaSeq2
-- Can't parse DNA
rnaSeq :: Seq RNA
rnaSeq = toSeq "AUGUG"

-- >>> rnaSeq
-- array (0,4) [(0,A'),(1,U'),(2,G'),(3,U'),(4,G')]
rnaSeq2 :: Seq RNA
rnaSeq2 = toSeq "ATGUG"

-- >>> rnaSeq2
-- Can't parse RNA
type Point = (Float, Float, Float)

class WithCoords a where
  getCoords :: a -> [Point]
  setCoords :: a -> [Point] -> a

-- 8. Определите тип данных 'Atom'. У него должны быть поля, соотвестствующие его координатам
--    в пространстве, типу химического элемента и названию атома.
--    Сделайте 'Atom' представителем класса типов 'WithCoords'. (0.25 б)
data Atom =
  Atom { atomCoords :: Point, atomType :: String, atomName :: String }
  deriving (Show)

instance WithCoords Atom where
  getCoords a = [atomCoords a]

  setCoords a [coords] = Atom coords (atomType a) (atomName a)
  setCoords a _ = error "Wrong coords"

a :: Atom
a = Atom (1, 2, 3) "A" "B"

-- >>> getCoords a
-- >>> setCoords a [(3, 2, 1)]
-- [(1.0,2.0,3.0)]
-- Atom {atomCoords = (3.0,2.0,1.0), atomType = "A", atomName = "B"}
data AminoAcid = AminoAcid { aminoAcidAtoms :: [Atom], aminoAcidName :: Char }
  deriving (Show)

instance WithCoords AminoAcid where
  getCoords a = concatMap getCoords (aminoAcidAtoms a)

  setCoords a coords = AminoAcid res (aminoAcidName a)
    where
      helper (a:as) (c:cs) acc = helper as cs (setCoords a [c]:acc)
      helper [] [] acc = acc
      helper [] _ _ = error "Wrong coords"
      helper _ [] _ = error "Wrong coords"

      res = reverse $ helper (aminoAcidAtoms a) coords []

instance Scorable AminoAcid where
  score a b
    | aminoAcidName a == aminoAcidName b = 5
    | otherwise = -4

aa :: AminoAcid
aa = AminoAcid [a, setCoords a [(3, 2, 1)]] 'M'

-- >>> getCoords aa
-- >>> getCoords $ setCoords aa $ reverse $ getCoords aa
-- [(1.0,2.0,3.0),(3.0,2.0,1.0)]
-- [(3.0,2.0,1.0),(1.0,2.0,3.0)]
------------------------------------------------------------------------------------
-- Обратите внимание! Теперь мы получили возможность выранивать аминокислотные
-- последовательности друг на друга, ничего не меняя в функции @align@.
------------------------------------------------------------------------------------
-- 10. Сделайте так, чтобы (Indexable c => c AminoAcid) был представителем класса типов 'WithCoords'.
--     Явно писать инстанс для 'Indexable c => c AminoAcid' нельзя! (1 б)
instance (Indexable c, WithCoords a) => WithCoords (c a) where
  getCoords a = concatMap (getCoords . getUnsafe a) (indices a)

  setCoords a coords = helper a (indices a) coords
    where
      helper a (i:is) cs = helper a' is cs'
        where
          e = getUnsafe a i

          nCoords = length $ getCoords e

          c = take nCoords cs

          cs' = drop nCoords cs

          e' = setCoords e c

          a' = updateUnsafe a i e'
      helper a [] [] = a
      helper a [] _ = error "Wrong coords"

acidList :: [AminoAcid]
acidList = [aa, setCoords aa $ reverse $ getCoords aa]

-- >>> getCoords acidList
-- >>> getCoords $ setCoords acidList $ map (\(x, y, z) -> (-z, y, -x)) $ getCoords acidList
-- [(1.0,2.0,3.0),(3.0,2.0,1.0),(3.0,2.0,1.0),(1.0,2.0,3.0)]
-- [(-3.0,2.0,-1.0),(-1.0,2.0,-3.0),(-1.0,2.0,-3.0),(-3.0,2.0,-1.0)]
-- >>> cc = getCoords acidList ++ [(0, 0, 0)]
-- >>> setCoords acidList cc
-- Wrong coords
-- >>> setCoords acidList $ drop 1 $ getCoords acidList
-- Wrong coords
type RotationMatrix =
  ((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))

rotate :: WithCoords a => RotationMatrix -> a -> a
rotate mat a = res
  where
    rotatePoint :: RotationMatrix -> Point -> Point
    rotatePoint ((a00, a01, a02), (a10, a11, a12), (a20, a21, a22)) (x, y, z) =
      ( a00 * x + a01 * y + a02 * z
      , a10 * x + a11 * y + a12 * z
      , a20 * x + a21 * y + a22 * z)

    coords = getCoords a

    newCoords = map (rotatePoint mat) coords

    res = setCoords a newCoords

mat :: RotationMatrix
mat = ((0, 1, 0), (1, 0, 0), (0, 0, 1))

-- >>> getCoords a
-- >>> rotate mat a
-- [(1.0,2.0,3.0)]
-- Atom {atomCoords = (2.0,1.0,3.0), atomType = "A", atomName = "B"}
-- >>> getCoords aa
-- >>> getCoords $ rotate mat aa
-- [(1.0,2.0,3.0),(3.0,2.0,1.0)]
-- [(2.0,1.0,3.0),(2.0,3.0,1.0)]
------------------------------------------------------------------------------------
-- Обратите внимание! Теперь мы получили возможность вращать в пространстве:
-- атомы, аминокислоты, любые последовательности аминокислот.
------------------------------------------------------------------------------------
main :: IO Int
main = return 0
