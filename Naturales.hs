{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Naturales where
data N where { O :: N ; S :: N -> N } 
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

predecesor :: N -> N
predecesor = \n -> case n of {O -> O; S x -> x}

class Eq a where
    (==) :: a -> a -> Bool 
    (==) a b = case a of {
        O -> case b of 
            {O -> True;
             S _ -> False};
              S x -> case b of 
                {O -> False; 
                S y -> x == y}}
instance Eq N where
(==) = \a -> \b -> case a of {
    O -> case b of {
        O -> True; 
        S x-> False
        }; 
        S x -> case b of {
            O -> False;
             S y -> x == y}}

instance Ord N where
    (<=) = \a -> \b -> case a of {
        O -> True; 
        S x -> case b of {
            O -> False; 
            S y -> x <= y}}
    minimo::N -> N -> N
    minimo = \a -> \b -> case a of {
        O -> O;
        S x -> case b of {
            O -> O;
            S y -> S (minimo x y)
        }
    }
    maximo::N -> N -> N
    maximo = \a -> \b -> case a of {
        O -> b;
        S x -> case b of {
            O -> a;
            S y -> S (maximo x y)
        }
    }
    min3::N -> N -> N -> N 
    min3 = \a -> \b -> \c -> minimo a (minimo b c)

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
    (%)::N->N->N.
    (%) = \a -> \b -> case a of {
        O -> O;
        S x -> case b of {
            O -> error "Division by zero";
            S y -> if x < y then O else S ((x - y) % b)
        }
    }
    (*) = \a -> \b -> S x -> b + (x * b)

fact::N->N
fact = \n -> case n of {
    O -> S O;
    S x -> n * fact x
}

 sumi::N->N
sumi = \n -> case n of {
    O -> O;
    S x -> n + sumi x  
}
sumdobles::N->N
sumdobles = \n -> case n of {
    O -> O;
    S x -> (S (S x)) + sumdobles x
}
sumfacts::N->N
sumfacts = \n -> case n of {
    O -> O;
    S x -> fact n + sumfacts x
}
sumfi::(N->N)->N->N
sumfi = \f -> \n -> case n of {
    O -> f 0;
    S x -> f n + sumfi f x
}
sumpares::N->N
sumpares = \n -> case n of {
    O -> O;
    S x -> case even n of {
        True -> n + sumpares x;
        False -> sumpares x
    }
even::N->Bool
even = \n -> case n of {
    O -> True;
    S x -> not (even x)
}
sumimpares::N->N
sumimpares = \n -> case n of {
    O -> O;
    S x -> case even n of {
        True -> sumimpares x;
        False -> n + sumimpares x
    }
}
sumpi::(N->Bool)->N->N
sumpi = \p -> \n -> case n of {
    O -> O;
    S x -> case p n of {
        True -> n + sumpi p x;
        False -> sumpi p x
    }
}
sumcuadimp::N->N
sumcuadimp = \n -> case n of {
    O -> O;
    S x -> case even n of {
        True -> sumcuadimp x;
        False -> n * n + sumcuadimp x
    }
}
doble::N->N
doble = \n -> case n of {
    O -> O;
    S x -> S (S (doble x))
}

sumdobles2::N->N
sumdobles2 = \n -> sumfi doble n

sumfact::N->N
sumfact = \n -> sumfi fact n