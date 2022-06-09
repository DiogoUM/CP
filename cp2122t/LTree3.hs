module LTree3 where

import Cp

arvt= Nodo (Nodo (Tri ((2,3),4)) (Tri ((1,2),3)) (Tri ((3,2),2))) (Tri ((3,9),7)) (Nodo (Tri ((2,3),4)) (Tri ((1,2),3)) (Tri ((3,2),2)))


-- (1) Datatype definition -----------------------------------------------------

type Side = Int
type Point = (Int,Int)

type Tri = (Point, Side)

data LTree3 a = Tri a | Nodo (LTree3 a) (LTree3 a) (LTree3 a) deriving (Eq,Show)

inLTree3 :: Either a ((LTree3 a, LTree3 a), LTree3 a) -> LTree3 a
inLTree3 = either Tri ((uncurry . uncurry) Nodo)

outLTree3 :: LTree3 a -> Either a ((LTree3 a, LTree3 a), LTree3 a)
outLTree3 (Tri a) = i1 a
outLTree3 (Nodo a b c) = i2 ((a,b),c)

baseLTree3 f g = f -|- (g >< g) >< g


-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = id -|- (f >< f) >< f

cataLTree3 g = g . (recLTree3 (cataLTree3 g)) . outLTree3

anaLTree3 g = inLTree3 . (recLTree3 (anaLTree3 g)) . g

hyloLTree3 f g = cataLTree3 f . anaLTree3 g

