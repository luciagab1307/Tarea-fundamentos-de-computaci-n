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
    (==) = undefined

instance Eq Racional where
    (==) = undefined

instance Ord Signo where
    (<=) = undefined

instance Ord Racional where
    (<=) = undefined

instance Num Racional where
    (+) = undefined
    (*) = undefined
    (-) = undefined  
