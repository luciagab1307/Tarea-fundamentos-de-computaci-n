-- Lucia Noria
-- 370992

-- Nahuel Ramírez
-- 335448

{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Racionales where
import Naturales

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show

data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show

instance Eq Signo where
    Pos == Pos = True
    Neg == Neg = True
    Pos == Neg = False
    Neg == Pos = False

instance Eq Racional where
    (Q Pos (n1,d1)) == (Q Pos (n2,d2)) = (n1 * d2 == n2 * d1)
    (Q Neg (n1,d1)) == (Q Neg (n2,d2)) = (n1 * d2 == n2 * d1)
    (Q Pos (n1,d1)) == (Q Neg (n2,d2)) = False
    (Q Neg (n1,d1)) == (Q Pos (n2,d2)) = False


instance Ord Signo where
    Neg <= Pos = True
    Neg <= Neg = True
    Pos <= Pos = True
    Pos <= Neg = False

instance Ord Racional where
    (Q Pos (n1,d1)) <= (Q Pos (n2,d2)) = n1*d2 <= n2*d1
    (Q Neg (n1,d1)) <= (Q Neg (n2,d2)) = n2*d1 <= n1*d2
    (Q Neg (n1,d1)) <= (Q Pos (n2,d2)) = True
    (Q Pos (n1,d1)) <= (Q Neg (n2,d2)) = False

instance Num Racional where

    (+) (Q Pos (n1,d1)) (Q Pos (n2,d2)) = Q Pos (n1*d2 + n2*d1,d1*d2)
    (+) (Q Neg (n1,d1)) (Q Neg (n2,d2)) = Q Neg (n1*d2 + n2*d1,d1*d2)
    (+) (Q Pos (n1,d1)) (Q Neg (n2,d2)) =
        case n1*d2 >= n2*d1 of
            True -> Q Pos (n1*d2 - n2*d1,d1*d2)
            False -> Q Neg (n2*d1 - n1*d2,d1*d2)

    (+) (Q Neg (n1,d1)) (Q Pos (n2,d2)) =
        case n1*d2 >= n2*d1 of
            True -> Q Neg (n1*d2 - n2*d1,d1*d2)
            False -> Q Pos (n2*d1 - n1*d2,d1*d2)
   
    (Q Pos (n1,d1)) * (Q Pos (n2,d2)) = Q Pos (n1*n2,d1*d2)
    (Q Neg (n1,d1)) * (Q Neg (n2,d2)) = Q Pos (n1*n2,d1*d2)
    (Q Pos (n1,d1)) * (Q Neg (n2,d2)) = Q Neg (n1*n2,d1*d2)
    (Q Neg (n1,d1)) * (Q Pos (n2,d2)) = Q Neg (n1*n2,d1*d2)

    (Q Pos (n1,d1)) - (Q Neg (n2,d2)) =
        Q Pos (n1*d2 + n2*d1,d1*d2)

    (Q Neg (n1,d1)) - (Q Pos (n2,d2)) =
        Q Neg (n1*d2 + n2*d1,d1*d2)

    (Q Pos (n1,d1)) - (Q Pos (n2,d2)) =
        case n2*d1 <= n1*d2 of
            True -> Q Pos (n1*d2 - n2*d1,d1*d2)
            False -> Q Neg (n2*d1 - n1*d2,d1*d2)

    (Q Neg (n1,d1)) - (Q Neg (n2,d2)) =
        case n2*d1 <= n1*d2 of
            True -> Q Neg (n1*d2 - n2*d1,d1*d2)
            False -> Q Pos (n2*d1 - n1*d2,d1*d2)