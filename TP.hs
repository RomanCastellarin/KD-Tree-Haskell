{-# LANGUAGE UnicodeSyntax #-}
{- Román Castellarín, Juan Ignacio Suarez -}

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
newtype Punto2d = P2d (Double, Double)          deriving (Eq, Show)

-- |Punto3d will represent a point in a 3-dimensional metric space
newtype Punto3d = P3d (Double, Double, Double)  deriving (Eq, Show)

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

-- |quickSelect: given a rank k, returns a triple with a list of elements less than the k-th element, the k-th element, and the remaiming elements
quickSelect :: Punto p ⇒ Int → Int → [p] → ([p],p,[p])
quickSelect d k (x:xs) | k < l     = let (a,b,c) = quickSelect d k ys       in (a, b, c++(x:zs))
                       | k > l     = let (a,b,c) = quickSelect d (k-l-1) zs in (ys++(x:a), b, c)
                       | otherwise = (ys,x,zs)
    where (ys, zs)  = partition (\y → comp y <= comp x) xs where comp = coord d -- DANGEROUS: ORIGINALLY < INSTEAD OF <= see ejercicio 2, item II and IV
          l         = length ys

-- |fromList: builds an NdTree from a list of Punto
fromList :: Punto p ⇒ [p] → NdTree p
fromList xp = build 0 xp 
    where build :: Punto p ⇒ Int → [p] → NdTree p
          build _ [] = Empty
          build l x  = Node (build newL left) median (build newL right) l
                          where newL                  = (l + 1) `mod` (dimension(x !! 0))
                                half                  = (length x) `div` 2  
                                (left, median, right) = quickSelect l half x 

-- |comparePoints: Given two points, returns True if all the coordinates are the same or False otherwise, so we don't insert the same point twice into the tree
comparePoints :: Punto p ⇒ p → p → Bool
comparePoints a b = foldl (&&) True [ (coord l a) == (coord l b) | l <- [0..(dimension a)-1] ]

-- |insertar_aux: Auxiliary function for 'insertar' to set the new level correctly
insertar_aux :: Punto p ⇒ p → NdTree p → Int → NdTree p
insertar_aux p Empty l = Node Empty p Empty l
insertar_aux p (Node a b c lvl) _ | comparePoints p b == True  = Node a b c lvl
                                  | coord lvl p <= coord lvl b = let newL = (lvl+1) `mod` (dimension p) in Node (insertar_aux p a newL) b c lvl
                                  | otherwise                  = let newL = (lvl+1) `mod` (dimension p) in Node a b (insertar_aux p c newL) lvl

-- |insertar: Given a Punto and a NdTree, inserts this Punto into the NdTree
insertar :: Punto p ⇒ p → NdTree p → NdTree p
insertar p Empty = Node Empty p Empty 0
insertar p t = insertar_aux p t 0


-- |maxCoord: Given two points and an int, returns the point with the maximum coordinate on the int-th position
maxCoord :: Punto p ⇒ p → p → Int → p
maxCoord a b l | (coord l a > coord l b) = a 
               | otherwise               = b

-- |minCoord: Given two points and an int, returns the point with the minimum coordinate on the int-th position
minCoord :: Punto p ⇒ p → p → Int → p
minCoord a b l | (coord l a < coord l b) = a 
               | otherwise               = b

-- |maxPointNdTree: Given an int and a NdTree, returns the point with the maximum coordinate on the int-th position in the NdTree
maxPointNdTree :: Punto p ⇒ Int → NdTree p → p
maxPointNdTree l (Node Empty p Empty lvl) = p
maxPointNdTree l (Node left p Empty lvl)  = if(l == lvl) then p else maxCoord p (maxPointNdTree l left) l
maxPointNdTree l (Node Empty p right lvl) = if(l == lvl) then maxPointNdTree l right else maxCoord p (maxPointNdTree l right) l
maxPointNdTree l (Node left p right lvl)  = if(l == lvl) then maxPointNdTree l right
                                                         else maxCoord p (maxCoord (maxPointNdTree l left) (maxPointNdTree l right) l) l

-- |minPointNdTree: Given an int and a NdTree, returns the point with the minimum coordinate on the int-th position in the NdTree
minPointNdTree :: Punto p ⇒ Int → NdTree p → p
minPointNdTree l (Node Empty p Empty lvl) = p
minPointNdTree l (Node Empty p right lvl) = if(l == lvl) then p else minCoord p (minPointNdTree l right) l
minPointNdTree l (Node left p Empty lvl)  = if(l == lvl) then minPointNdTree l left else minCoord p (minPointNdTree l left) l
minPointNdTree l (Node left p right lvl)  = if(l == lvl) then minPointNdTree l left
                                                         else minCoord p (minCoord (minPointNdTree l left) (minPointNdTree l right) l) l

-- |eliminar: Given a point and a NdTree, returns this NdTree with the point removed (if found)
eliminar :: (Eq p, Punto p) ⇒ p → NdTree p → NdTree p
eliminar _ Empty = Empty
eliminar x (Node Empty p Empty lvl) | (x == p) = Empty
eliminar x (Node left p Empty lvl)  | (x == p) = let maxI = maxPointNdTree lvl left  in Node (eliminar maxI left) maxI Empty lvl
eliminar x (Node left p right lvl)  | (x == p) = let minD = minPointNdTree lvl right in Node left minD (eliminar minD right) lvl
                                    | coord lvl x <= coord lvl p = (Node (eliminar x left) p right lvl)
                                    | coord lvl x > coord lvl p  = (Node left p (eliminar x right) lvl)
                                           
-- |Rect will represent a rectangle in a 2-dimensional metric space, defined by two (opposed) points
type Rect = (Punto2d, Punto2d)

-- |getMinCoord: Given a Rect and an Int (which represents the axis), returns the minimum coordinate of both points using that axis
getMinCoord :: Rect → Int → Double
getMinCoord (a,b) l = (coord l a) `min` (coord l b)

-- |getMaxCoord: Given a Rect and an Int (which represents the axis), returns the maximum coordinate of both points using that axis
getMaxCoord :: Rect → Int → Double
getMaxCoord (a,b) l = (coord l a) `max` (coord l b)

-- |isInRect: Given a Point2d, a Rect and an Int, returns True if the point is STRICTLY inside the rectangle, or False otherwise
    -- To make this non-strict, one should simply replace '>' with '>=' and '<' with '<=' 
isInRect :: Punto2d → Rect → Int → Bool
isInRect p (a,b) l = let nextl = (l+1) `mod` (dimension p) in 
                        if ((coord l p) > (getMinCoord (a,b) l)) && ((coord l p) < (getMaxCoord (a,b) l)) &&
                           ((coord nextl p) > (getMinCoord (a,b) nextl)) && ((coord nextl p) < (getMaxCoord (a,b) nextl))
                        then True else False

-- |ortogonalSearch: Given a NdTree of Punto2d and a Rect, returns a list of each Punto2d that is STRICTLY inside Rect
ortogonalSearch :: NdTree Punto2d → Rect → [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node Empty p Empty lvl) (a,b) = if (isInRect p (a,b) lvl) then [p] else []
ortogonalSearch (Node left p right lvl) (a,b)  | (coord lvl p) < (getMinCoord (a,b) lvl) = ortogonalSearch right (a,b)
                                               | (coord lvl p) > (getMaxCoord (a,b) lvl) = ortogonalSearch left (a,b)
                                               | otherwise = elem ++ (ortogonalSearch right (a,b)) ++ (ortogonalSearch left (a,b))
                                                    where elem = if (isInRect p (a,b) lvl) then [p] else []


-- EXAMPLES
milista  = [P2d x | x ←[(2,3), (5,4), (9,6), (4,7), (8,1), (7,2)]]
miarbol = fromList milista

{- 

miarbol:

Node 
    (Node 
        (Node Empty (P2d (2.0,3.0)) Empty 0) 
    (P2d (5.0,4.0)) 
        (Node Empty (P2d (4.0,7.0)) Empty 0) 
    1) 
(P2d (7.0,2.0)) 
    (Node 
        (Node Empty (P2d (8.0,1.0)) Empty 0) 
    (P2d (9.0,6.0)) 
        Empty 
    1) 
0

-}

miarbolcon36 = insertar (P2d(3,6)) miarbol

{-

miarbolcon36:

Node 
    (Node 
        (Node Empty (P2d (2.0,3.0)) Empty 0) 
    (P2d (5.0,4.0)) 
        (Node 
            (Node Empty (P2d (3.0,6.0)) Empty 1) 
        (P2d (4.0,7.0)) 
            Empty 
        0) 
    1) 
(P2d (7.0,2.0)) 
    (Node 
        (Node Empty (P2d (8.0,1.0)) Empty 0) 
    (P2d (9.0,6.0)) 
        Empty 
    1) 
0

-}

miarbolsin72 = eliminar (P2d(7,2)) miarbol

{-

miarbolsin72:

Node 
    (Node 
        (Node Empty (P2d (2.0,3.0)) Empty 0) 
    (P2d (5.0,4.0)) 
        (Node Empty (P2d (4.0,7.0)) Empty 0) 
    1) 
(P2d (8.0,1.0)) 
    (Node Empty (P2d (9.0,6.0)) Empty 1)
0

-}

rect1 = ( (P2d(7.5,0.9)) , (P2d(3,5)) )
busq1 = ortogonalSearch miarbol rect1

-- busq1: [P2d (7.0,2.0),P2d (5.0,4.0)]

rect2 = ( (P2d(1.5,3.5)) , (P2d(9.5,9.5)) )
busq2 = ortogonalSearch miarbol rect2

-- busq2: [P2d (9.0,6.0),P2d (5.0,4.0),P2d (4.0,7.0)]
