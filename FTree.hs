{-# OPTIONS_GHC -XNPlusKPatterns #-}

-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2017/8)

module FTree where

import Cp

-- (1) Datatype definition -----------------------------------------------------

data FTree a c = Unit c | Comp a (FTree a c, FTree a c) deriving Show

inFTree = either Unit (uncurry Comp)

outFTree (Unit c)         = Left c
outFTree (Comp a (t1,t2)) = Right(a,(t1,t2))

-- (2) Ana + cata + hylo -------------------------------------------------------

cataFTree a = a . (recFTree (cataFTree a)) . outFTree

anaFTree f = inFTree . (recFTree (anaFTree f) ) . f

hyloFTree a c = cataFTree a . anaFTree c

baseFTree f g h  = f -|- (g  >< (h >< h))

recFTree f = baseFTree id id f

-- (3) Map ---------------------------------------------------------------------

instance BiFunctor FTree
         where bmap f g = cataFTree ( inFTree . baseFTree g f id )

-- (4) Examples ----------------------------------------------------------------

-- (4.1) Inversion (mirror) ----------------------------------------------------

invFTree = cataFTree (inFTree . (id -|- id >< swap))

-- (4.2) Counting --------------------------------------------------------------

countFTree = cataFTree (either (const 1) (succ . (uncurry (+)) . p2))

-- etc
