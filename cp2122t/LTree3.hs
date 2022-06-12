module LTree3 where

import Cp
import Svg

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

-- (3) Código fornecido para desenha ---------------------------------------------

--Triângulo de base:
base = ((0,0), 32)

--Desenho de triângulos em \svg:
desenha x = picd'' [ scale 0.44 (0,0) (x >>= tri2svg) ]

--Função que representa cada triângulo em \svg:
tri2svg :: Tri -> Svg
tri2svg (p,c) = (red . polyg) [ p, p .+ (0,c), p .+ (c,0) ]

--NB: o tipo Svg é sinónimo de String:
type Svg = String

-- (4) Sierpinski + gera + folhas -----------------------------------------------

sierpinski :: (Tri, Int) -> [Tri]
sierpinski = folhasSierp . geraSierp

geraSierp :: (Tri, Int) -> LTree3 Tri
geraSierp = anaLTree3 g2 where
    g2 (t,0) = i2 t
    g2 (((x,y),s), n) = i2 ((t1,t2),t3) where
        t1 = split (split ((uncurry (++)) . (p1 >< (/2))) (p2 . p1)) ((/2) . p2)
        t2 = split (split (p1 . p1) ((uncurry (++)) . (p2 >< (/2)))) ((/2) . p2)
        t3 = (id >< id) >< (/2)

folhasSierp :: LTree3 Tri -> [Tri]
folhasSierp = cataLTree3 (either singl ((uncurry (++)) . ((uncurry (++)) >< id)))