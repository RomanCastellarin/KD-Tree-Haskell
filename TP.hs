{-# LANGUAGE UnicodeSyntax #-}

import Data.List (partition)

-- |NdTree represents a tree containing points in an N-dimensional metric space
data NdTree p = 
    Node (NdTree p) p (NdTree p) Int 
  | Empty 
  deriving (Eq, Ord, Show)

-- |Punto represents a point in an N-demensional metric space
class Punto p where
    dimension   :: p → Int
    coord       :: Int → p → Double
    dist        :: p → p → Double
    
    dist a b    = sum [(c a - c b)^2 | d ← [1..dimension a], c ← [coord d]]

-- |Punto2d will represent a point in a 2-dimensional metric space 
newtype Punto2d = P2d (Double, Double)          deriving Show

-- |Punto3d will represent a point in a 3-dimensional metric space
newtype Punto3d = P3d (Double, Double, Double)  deriving Show

-- |Declare Punto2d as an instance of Punto
instance Punto Punto2d where
    dimension _           = 2
    coord 0 (P2d(x, _))   = x 
    coord 1 (P2d(_, y))   = y

-- |Declare Punto3d as an instance of Punto    
instance Punto Punto3d where
    dimension _              = 3
    coord 0 (P3d(x, _, _))   = x 
    coord 1 (P3d(_, y, _))   = y 
    coord 2 (P3d(_, _, z))   = z

-- TODO: MAKE QUICKSELECT AT MOST LINEAR IN EXPECTATION (when given many elements with same coordinate it degenerates to O(N²))

-- |quickSelect: given a rank k, returns a triple with a list of elements less than the k-th element, the k-th element, and the remaiming elements
quickSelect :: Punto p ⇒ Int → Int → [p] → ([p],p,[p])
quickSelect d k (x:xs) | k < l     = let (a,b,c) = quickSelect d k ys       in (a, b, c++(x:zs))
                       | k > l     = let (a,b,c) = quickSelect d (k-l-1) zs in (ys++(x:a), b, c)
                       | otherwise = (ys,x,zs)
    where (ys, zs)  = partition (\y → comp y <= comp x) xs where comp = coord d -- DANGEROUS: ORIGINALLY < INSTEAD OF <= see ejercicio 2, item II and IV
          l         = length ys
                
milista =  [P2d x | x ←[(2,3), (5,4), (9,6), (4,7), (8,1), (7,2)]]
miarbol = fromList milista

-- |fromList: builds an NdTree from a list of Punto
fromList :: Punto p ⇒ [p] → NdTree p
fromList xp = build 0 xp 
    where build :: Punto p ⇒ Int → [p] → NdTree p
          build _ [] = Empty
          build l x  = Node (build newL left) median (build newL right) l
                          where newL                  = (l + 1) `mod` (dimension(x !! 0))
                                half                  = (length x) `div` 2  
                                (left, median, right) = quickSelect l half x 

insertar :: Punto p ⇒ p → NdTree p → NdTree p
insertar p Empty = Node Empty p Empty 0 -- TODO: USE AUXILIAR FUNCTION IN ORDER TO ASSIGN CORRECT AXIS
insertar p (Node a b c d) | coord d p <= coord d b = Node (insertar p a) b c d
                          | otherwise              = Node a b (insertar p c) d

{--
Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) 
(P2d (7.0,2.0))
(Node (Node (Node Empty (P2d (8.0,2.0)) Empty 0) (P2d (8.0,1.0)) Empty 0) (P2d (9.0,6.0)) Empty 1) 0
--}
