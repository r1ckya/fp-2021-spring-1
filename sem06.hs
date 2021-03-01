-- В Хаскеле импорты из других пакетов/библиотек производятся так:
import           Data.Array      (Array, Ix)
-- В Хаскеле есть qualified-импорты:
import qualified Data.Array as A (array, bounds, indices, listArray, (!), (//))

-- Клёвый пакет для дебага хаскельных программ.
-- Например, функция traceShow имеет тип Show a => a -> b -> b.
-- Первым аргументом в неё нужно передать то, что хочется вывести на экран.
-- В остальном она работет так же, как и id. 
-- Поэтому её можно воткнуть в любое место в программе.
import Debug.Trace (traceShow)

-- | Функция, которая заменяет значение в массиве по заданному индексу.
--
updateIndex :: Ix ind => Array ind a -> ind -> a -> Array ind a
updateIndex arr ind val = arr A.// [(ind, val)]

-- | Матрица — массив, индексами которого являются пары интов.
--
newtype Matrix a = Matrix { mat :: Array (Int, Int) a }

-- | Можем сделать представителем класса типов 'Show' любую матрицу,
--   тип элеменов которой сам является представителем класса типов 'Show'.
--
instance Show a => Show (Matrix a) where
    show (Matrix mat) = toString "\n" rowStrings
      where
        (_, (rowsN', colsN')) = A.bounds mat

        rowStrings = map outputRow [0 .. rowsN']

        -- | Склеивает список строк, добавляя между каждыми двумя
        --   из них разделитель @sep@.
        --
        toString :: String -> [String] -> String
        toString _   []       = []
        toString _   [x]      = x
        toString sep (x : xs) = x <> sep <> toString sep xs

        outputRow :: Int -> String
        outputRow rowInd = toString " " rowElemsStr
          where
            rowIndices = zip (repeat rowInd) [0 .. colsN']
            rowElems   = map (mat A.!) rowIndices

            rowElemsStr = map show rowElems

-- | Функция, которая заменяет значение по индексу в матрице.
--
updateIndexM :: Matrix a -> (Int, Int) -> a -> Matrix a
updateIndexM (Matrix m) ind val = Matrix $ updateIndex m ind val

-- | Функция, которая создаёт матрицу размера @rowsN@ на @colsN@ и заполняет
--   все её ячейки значением @val@.
--
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix rowsN colsN val = Matrix 
                             $ A.array (head allIndices, last allIndices) indicesToVals
  where
    allIndices = [(i, j) | i <- [0 .. rowsN - 1], j <- [0 .. colsN - 1]]

    -- По типу @zip@понятно, что он делает: zip :: [a] -> [b] -> [(a, b)]
    -- @repeat@ принимает какое-то значение и делает из него бесконечный список.
    -- Конструкция ниже работает за конечное время потому, что @zip@ ленивый.
    indicesToVals = zip allIndices (repeat val)

-- | Создадим тип, соответствующий ДНК-нуклеотидам.
--
data DNA = A | T | G | C
  deriving (Eq, Show)

-- | Создадим тип, соответствующий РНК-нуклеотидам.
--
data RNA = A' | U' | G' | C'
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
    score _ _   = -4 

-- | Функция, которая считает скор выравнивания одной последовательности 'Scorable'-объектов
--   на другую последовательность 'Scorable'-объектов такого же типа.
--
align :: Scorable a => Array Int a -> Array Int a -> Float
align seq1 seq2 = mat scoreMatrix A.! (lenSeq1, lenSeq2)
  where
    (_, lenSeq1') = A.bounds seq1
    (_, lenSeq2') = A.bounds seq2

    lenSeq1 = lenSeq1' + 1
    lenSeq2 = lenSeq2' + 1

    initScoreMatrix = createMatrix (lenSeq1 + 1) (lenSeq2 + 1) 0

    matIndices  = A.indices (mat initScoreMatrix)
    scoreMatrix = recursiveUpdate initScoreMatrix matIndices

    -- | По очереди вычисляем скор для каждой ячейки матрицы ДП.
    --   Индексы в @matIndices@ идут в порядке слева направо сверху вниз,
    --   потому что мы ровно так определили создание матрицы в @createMatrix@.
    --
    recursiveUpdate :: Matrix Float -> [(Int, Int)] -> Matrix Float
    recursiveUpdate acc []       = acc
    recursiveUpdate acc (x : xs) = recursiveUpdate updatedAcc xs
      where
        updatedAcc :: Matrix Float
        updatedAcc = scoreForIndex seq1 seq2 acc x

-- | Функция, которая вычислет скор выравнивания префикса длины @i@ последовательности 
--   @seq1@ на префикс длины @j@ последовательности @seq2@ и записывает этот скор
--   в ячейку @ind@ матрицы @scoreMat@.
--
scoreForIndex :: Scorable a 
              => Array Int a 
              -> Array Int a 
              -> Matrix Float 
              -> (Int, Int) 
              -> Matrix Float
scoreForIndex seq1 seq2 scoreMat@(Matrix m) ind@(i, j)
  | i == 0 && j == 0 = updateIndexM scoreMat ind 0
  | i == 0           = updateIndexM scoreMat ind (fromIntegral j * gapCost)
  | j == 0           = updateIndexM scoreMat ind (fromIntegral i * gapCost)
  | otherwise        = updateIndexM scoreMat ind (maximum [insCost, delCost, subCost])
  where
    insCost = m A.! (i - 1, j) + gapCost
    delCost = m A.! (i, j - 1) + gapCost
    subCost = m A.! (i - 1, j - 1) + score (seq1 A.! (i - 1)) (seq2 A.! (j - 1))

    gapCost :: Float
    gapCost = -1

-- | Функция, которая превращает строку в последовательность нуклеотидов.
--
stringToDNASeq :: String -> Array Int DNA
stringToDNASeq s = A.listArray bounds $ map charToDNA s
  where
    bounds = (0, length s - 1)

    charToDNA :: Char -> DNA
    charToDNA 'A' = A
    charToDNA 'T' = T
    charToDNA 'G' = G
    charToDNA 'C' = C
    charToDNA _   = error "Can't parse DNA"