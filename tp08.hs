{-# LANGUAGE BangPatterns #-}
module Main(main) where

import qualified Data.Map as M
import           Data.Map           (Map, (!))
import           System.Environment (getArgs)

-- Interface de cola de prioridad.
class PriorityQueue pq where
        pqempty :: pq a
        pqisEmpty :: pq a -> Bool
        pqenqueue :: Ord a => a -> pq a -> pq a
        pqfront :: Ord a => pq a -> a
        pqdequeue :: Ord a => pq a -> pq a

extractPQueue :: (PriorityQueue pq, Ord a) => pq a -> (a, pq a)
extractPQueue pque = (pqfront pque, pqdequeue pque)

-- Heap para tener una instancia eficiente de cola de prioridad.
-- Se mantiene el tamaño del heap como miembro para no perder tiempo volviendo a
-- contar cuántos nodos hay en total.
data Heap a = HEmpty | HNode !a !Int !(Heap a) !(Heap a) deriving (Show, Eq)

heapFrom :: a -> Heap a
heapFrom x = HNode x 1 HEmpty HEmpty

instance PriorityQueue Heap where
        pqempty = HEmpty
        pqisEmpty HEmpty = True
        pqisEmpty _ = False
        pqenqueue = insertHeap
        pqfront = minHeap
        pqdequeue = deleteHeap

-- Implementaciones heap; del apunte.
minHeap :: Heap a -> a
minHeap HEmpty = undefined
minHeap (HNode x _ _ _) = x

hsize :: Heap a -> Int
hsize HEmpty = 0
hsize (HNode _ !s _ _) = s

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x HEmpty = HNode x 1 HEmpty HEmpty
insertHeap x (HNode r s ls rs)
        | x <  r && hsize ls <= hsize rs = HNode x (s + 1) (insertHeap r ls) rs
        | x <  r && hsize ls  > hsize rs = HNode x (s + 1) ls (insertHeap r rs)
        | x >= r && hsize ls <= hsize rs = HNode r (s + 1) (insertHeap x ls) rs
        | x >= r && hsize ls  > hsize rs = HNode r (s + 1) ls (insertHeap x rs)

deleteHeap :: Ord a => Heap a -> Heap a
deleteHeap HEmpty = undefined
deleteHeap (HNode _ _ ls HEmpty) = ls
deleteHeap (HNode _ _ HEmpty rs) = rs
deleteHeap (HNode t s a@(HNode l _ _ _) b@(HNode r _ _ _))
        | l <= r = HNode l (s - 1) (deleteHeap a) b
        | l  > r = HNode r (s - 1) a (deleteHeap b)

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

-- Representa un camino que estamos explorando actualmente.
-- Estamos en la posición (x, y) con el costo acumulado c.
data Paso = Paso (Int, Int) Int deriving (Eq, Show)

instance Ord Paso where
        (Paso _ c1) <= (Paso _ c2) = c1 <= c2

-- Usamos el algoritmo de Dijkstra para encontrar los K caminos minimos.
-- El algoritmo en sí no es muy distinto de un recorrido iterativo de un grafo.
-- Cuando el grafo tiene costos, si lo exploramos con una cola de prioridad, y vamos
-- acumulando el costo de recorrer cada nodo, el resultado es que exploramos el
-- grafo por los caminos más cortos.
-- Las primeras K veces que lleguemos a la esquina inferior-derecha, está garantizado
-- entonces que habrán sido los K caminos mínimos.
dijkstra :: Map (Int, Int) Int -> Int -> Int -> Int -> [Int]
dijkstra grilla kinicial m n = go kinicial [] visitasInicial heapInicial
        where !heapInicial = heapFrom (Paso (0, 0) (grilla ! (0, 0)))
              !visitasInicial = listasAGrilla n m (replicate n (replicate m 0))
              go 0 acc _ _ = acc
              go !k acc !visitas !pq
                -- Casos base:
                | pqisEmpty pq = acc
                | x >= n || y >= m = go k acc visitas pq'
                | x == (n - 1) && y == (m - 1) = go (k - 1) (c : acc) visitas' pq'
                -- Casos recursivos:
                | visitas ! (x, y) >= k = go k acc visitas pq'
                | otherwise = go k acc visitas' (pqenqueue abajo (pqenqueue derecha pq'))
               where (p, pq') = extractPQueue pq
                     (Paso (!x, !y) c) = p
                     visitas' = M.adjust (+1) (x, y) visitas
                     abajo   = Paso (x, y + 1) (c + M.findWithDefault 0 (x, y + 1) grilla)
                     derecha = Paso (x + 1, y) (c + M.findWithDefault 0 (x + 1, y) grilla)

-- Para poder probar el programa desde GHCi.
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
