-- =============================================
-- Tests para Naturales.hs (Eq, Ord, Num)
-- Uso: para cada instancia hay una lista de  casos
--      y el test correspondiente es un booleano que
--      se llama ok<Clase><Tipo> (okEqN, okOrdN y okNumN).
-- Si un test no da True, probar caso por caso.
-- =============================================

import Naturales

-- Numeros
seis    = S cinco
siete   = S seis
ocho    = S siete
nueve   = S ocho
diez    = S nueve

-- ===========================
-- Eq para N
-- ===========================
testsEqN :: [Bool]
testsEqN =
  [ O == O
  , uno == uno
  , not (O == uno)
  , not (uno == dos)
  , S (S tres) == cinco
  , not (S (S tres) == cuatro)
  ]

okEqN = and testsEqN

-- ===========================
-- Ord para N
-- ===========================
testsOrdN :: [Bool]
testsOrdN =
  [ O <= O
  , O <= uno
  , not (uno <= O)
  , uno < dos
  , tres > dos
  , cinco >= cinco
  , not (tres < tres)
  , S cinco > cinco
  , not (cuatro > cuatro)
  , cuatro >= tres
  ]

okOrdN = and testsOrdN

-- ===========================
-- Num para N (+, *, -)
-- ===========================
testsNumN :: [Bool]
testsNumN =
  -- + 
  [ O + O == O
  , O + tres == tres
  , tres + O == tres
  , uno + dos == dos + uno
  , (uno + (dos + tres)) == ((uno + dos) + tres)
  , uno + dos == tres
  , dos + dos == cuatro
  , tres + dos == cinco
  , cuatro + cinco == nueve
  , cinco + cinco == diez
  , (dos + (tres + cuatro)) == nueve
  -- * 
  , O * O == O
  , O * tres == O
  , tres * O == O
  , tres * uno == tres
  , dos * dos == cuatro
  , tres * dos == seis
  , tres * tres == nueve
  , cuatro * dos == ocho
  , cuatro * tres ==seis + seis
  , uno * cinco == cinco
  , dos * tres == tres * dos
  , (uno * (dos * tres)) == ((uno * dos) * tres)
  , tres * (uno + dos) == (tres * uno) + (tres * dos)
  , (uno + dos) * tres == (uno * tres) + (dos * tres)
  -- - 
  , tres - dos == uno
  , dos - tres == O
  , O - tres == O
  , cinco - O == cinco
  , diez - cinco == cinco
  , cinco - cinco == O
  , seis - cuatro == dos
  , cuatro - seis == O
  , (dos + tres) - tres == dos
  , (tres - dos) + dos == tres
  ]

okNumN = and testsNumN

