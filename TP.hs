import Data.List (partition)

data NdTree p = 
    Node (NdTree p) p (NdTree p) Int 
  | Empty 
  deriving (Eq, Ord, Show)

class Punto p where
    dimension   :: p -> Int
    coord       :: Int -> p -> Double
    dist        :: p -> p -> Double
    
    dist a b    = sum [(c a - c b)^2 | d <- [1..dimension a], c <- [coord d]]

newtype Punto2d = P2d (Double, Double)          deriving Show
newtype Punto3d = P3d (Double, Double, Double)  deriving Show

instance Punto Punto2d where
    dimension _           = 2
    coord 0 (P2d(x, _))   = x 
    coord 1 (P2d(_, y))   = y
    
instance Punto Punto3d where
    dimension _              = 3
    coord 0 (P3d(x, _, _))   = x 
    coord 1 (P3d(_, y, _))   = y 
    coord 2 (P3d(_, _, z))   = z

-- ========================================================================= --
-- from this point onward, code is ugly
-- maybe write some **determinitically linear** Selection Algorithm ? 
quickSelect :: Punto p => Int -> Int -> [p] -> p
quickSelect d k (x:xs) | k < l     = quickSelect d k ys
                       | k > l     = quickSelect d (k-l-1) zs
                       | otherwise = x
    where (ys, zs)  = partition (\y->c y < c x) xs where c = coord d
          l         = length ys
          
milista = [P2d (x, y) | x <- [1..3], y <- [1..3]]

{- draft
fromList :: Punto p => [p] -> NdTree p
fromList xp = build 0 xp 
    where build         :: Int -> [p] -> NdTree p
          build _ []    = Empty
          build l x     = Node (build newL left) middle (build newL right) l
                          where newL    = (l + 1) `mod` (dimension(x !! 0)) 
                                (left, middle:right) = partition (\y->(c y < c median)) x
                                                       where c      = coord l
                                                             median = quickSelect l half x where half = (length x) `div` 2 
-}
