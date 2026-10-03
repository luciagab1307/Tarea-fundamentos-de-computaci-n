-- Lucia Noria
-- 370992

-- NOMBRE ESTUDIANTE  2
-- NRO ESTUDIANTE 2

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
    (<=) = undefined

instance Ord Racional where
    
    

instance Num Racional where
    (+) = undefined
    (*) = undefined 
    
