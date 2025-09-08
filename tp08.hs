module Main(main) where

import qualified Data.Map as M
import           Data.Map           (Map, (!))
import           System.Environment (getArgs)

import System.IO.Unsafe (unsafePerformIO)

class PriorityQueue pq where
        pqempty :: pq a
        pqisEmpty :: pq a -> Bool
        pqenqueue :: Ord a => a -> pq a -> pq a
        pqfront :: Ord a => pq a -> a
        pqdequeue :: Ord a => pq a -> pq a

extractPQueue :: (PriorityQueue pq, Ord a) => pq a -> (a, pq a)
extractPQueue pque = (pqfront pque, pqdequeue pque)

data Heap a = HEmpty | HNode a Int (Heap a) (Heap a) deriving (Show, Eq)

heapFrom :: a -> Heap a
heapFrom x = HNode x 1 HEmpty HEmpty

instance PriorityQueue Heap where
        pqempty = HEmpty
        pqisEmpty HEmpty = True
        pqisEmpty _ = False
        pqenqueue = insertHeap
        pqfront = minHeap
        pqdequeue = deleteHeap

minHeap :: Heap a -> a
minHeap HEmpty = undefined
minHeap (HNode x _ _ _) = x

hsize :: Heap a -> Int
hsize HEmpty = 0
hsize (HNode _ s _ _) = s

calcSize :: Heap a -> Heap a
calcSize HEmpty = HEmpty
calcSize (HNode x _ ls rs) = HNode x s sl sr
        where sl = calcSize ls
              sr = calcSize rs
              s = 1 + hsize sl + hsize sr

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x HEmpty = HNode x 1 HEmpty HEmpty
insertHeap x (HNode r s ls rs)
        | x <  r && hsize ls <= hsize rs = calcSize $ HNode x s (insertHeap r ls) rs
        | x <  r && hsize ls  > hsize rs = calcSize $ HNode x s ls (insertHeap r rs)
        | x >= r && hsize ls <= hsize rs = calcSize $ HNode r s (insertHeap x ls) rs
        | x >= r && hsize ls  > hsize rs = calcSize $ HNode r s ls (insertHeap x rs)

deleteHeap :: Ord a => Heap a -> Heap a
deleteHeap HEmpty = undefined
deleteHeap (HNode _ _ ls HEmpty) = ls
deleteHeap (HNode _ _ HEmpty rs) = rs
deleteHeap (HNode t s a@(HNode l szl lls lrs) b@(HNode r szr rls rrs))
        | l <= r = calcSize $ HNode l s (deleteHeap $ HNode t szl lls lrs) b
        | l  > r = calcSize $ HNode r s a (deleteHeap $ HNode t szr rls rrs)

leerArchivo :: FilePath -> IO (Int, Int, Int, [[Int]])
leerArchivo archivo = do
  contenido <- readFile archivo
  let (primeraLinea:resto) = lines contenido
      [n, m, k] = map read $ words primeraLinea
      listas = map (map read . words) $ take n resto
  return (n, m, k, listas)

escribirArchivo :: FilePath -> [Int] -> IO ()
escribirArchivo archivo costos = do
  writeFile archivo $ unwords (map show costos)

listasAGrilla :: Int -> Int -> [[Int]] -> Map (Int, Int) Int
listasAGrilla n m lista = M.fromList [((x, y), val)
                                     | (y, fila) <- zip [0..n - 1] lista
                                     , (x, val) <- zip [0..m - 1] fila]

--Hola Luchito :P si esto no molesta lo hizo felipe si esta mal lo hicieron matias con mateo

costosMinimos :: Int -> Int -> Int -> Map (Int, Int) Int -> [Int]
costosMinimos n m k grilla = reverse (dijkstra grilla k n m)

data Paso = Paso (Int, Int) Int deriving (Eq, Show)

instance Ord Paso where
        (Paso _ c1) <= (Paso _ c2) = c1 <= c2

dijkstra :: Map (Int, Int) Int -> Int -> Int -> Int -> [Int]
dijkstra grilla kinicial m n = go kinicial [] visitasInicial heapInicial
        where heapInicial = heapFrom (Paso (0, 0) (grilla ! (0, 0)))
              visitasInicial = listasAGrilla n m (replicate n (replicate m 0))
              go 0 acc _ _ = acc
              go k acc visitas pq
                | pqisEmpty pq = acc
                | x >= n || y >= m = go k acc visitas pq'
                | x == (n - 1) && y == (m - 1) = go (k - 1) (c : acc) visitas' pq'
                | visitas ! (x, y) >= k = go k acc visitas pq'
                | otherwise = go k acc visitas' (pqenqueue abajo (pqenqueue derecha pq'))
               where (p, pq') = extractPQueue pq
                     (Paso (x, y) c) = p
                     visitas' = M.adjust (+1) (x, y) visitas
                     abajo   = Paso (x, y + 1) (c + M.findWithDefault 999 (x, y + 1) grilla)
                     derecha = Paso (x + 1, y) (c + M.findWithDefault 999 (x + 1, y) grilla)

run :: FilePath -> IO ()
run archivo = do
  (n, m, k, listas) <- leerArchivo archivo
  print listas
  let grilla = listasAGrilla n m listas
  let kMejoresCostos = costosMinimos n m k grilla
  escribirArchivo "salida.out" kMejoresCostos

main :: IO ()
main = do
  (archivo : _) <- getArgs
  run archivo
