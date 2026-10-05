{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Naturales where

data N where
    O :: N
    S :: N -> N
    deriving Show

uno :: N
uno = S O

dos :: N
dos = S uno

tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

seis :: N
seis = S cinco

instance Eq N where
    (==) = \a -> \b -> case a of {
        O -> case b of {
            O -> True;
            S y -> False
        };
        S x -> case b of {
            O -> False;
            S y -> x == y
        }
    }

instance Ord N where
    (<=) = \a -> \b -> case a of {
        O -> True;
        S x -> case b of {
            O -> False;
            S y -> x <= y
        }
    }

instance Num N where
    (+) = \a -> \b -> case a of {
        O -> b;
        S x -> S (x + b)
    }

    (*) = \a -> \b -> case a of {
        O -> O;
        S x -> b + (x * b)
    }

    (-) = \a -> \b -> case a of {
        O -> O;
        S x -> case b of {
            O -> S x;
            S y -> x - y
        }
    }