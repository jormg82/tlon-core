-------------------------------------------------------------
-- Heap.hs
-- Heap implementation
-------------------------------------------------------------

module Machine.Heap
  ( Addr,
    Heap (),
    initial,
    alloc,
    allocn,
    update,
    free,
    lookup,
    addresses,
    size,
    null,
    nullAddr,
  )
where

import qualified Data.Map as M
import Prelude hiding (lookup, null)

type Addr = Int

data Heap a = Heap {unused :: [Addr], maps :: M.Map Addr a}
  deriving (Show)

initial :: Heap a
initial = Heap {unused = [1 ..], maps = M.empty}

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap (a:as) m) x = (Heap as $ M.insert a x m, a)
alloc _ _ = error "alloc: no adressess!!"

allocn :: Int -> Heap a -> a -> (Heap a, [Addr])
allocn 0 h _ = (h, [])
allocn n h x = (h'', a:as)
  where
    (h', as) = allocn (n - 1) h x
    (h'', a) = alloc h' x

update :: Heap a -> Addr -> a -> Heap a
update (Heap as m) a x
  | M.member a m = Heap as (M.insert a x m)
  | otherwise = error "Update: ilegal address!!"

free :: Heap a -> Addr -> Heap a
free h@(Heap as m) a
  | M.member a m = Heap (a:as) (M.delete a m)
  | otherwise = h

lookup :: Heap a -> Addr -> Maybe a
lookup (Heap _ m) a = M.lookup a m

addresses :: Heap a -> [Addr]
addresses = M.keys . maps

size :: Heap a -> Int
size = M.size . maps

null :: Addr
null = 0

nullAddr :: Addr -> Bool
nullAddr = (== 0)

