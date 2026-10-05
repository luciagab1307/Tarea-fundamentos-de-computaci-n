-- =============================================
-- Tests para Racionales.hs (Eq, Ord, Num)
-- Uso: para cada instancia hay una lista de  casos
--      y el test correspondiente es un booleano que
--      se llama ok<Clase><Tipo> (okEqSigno, okEqRacional,
--      okOrdSigno, okOrdRacional y okNumRacional).
-- Si un test no da True, probar caso por caso.
-- =============================================

import Naturales
import Racionales

-- Numeros
seis    = S cinco
siete   = S seis
ocho    = S siete
nueve   = S ocho
diez    = S nueve

-- Cero: se acepta indistintamente el cero positivo o el negativo
esCero :: Racional -> Bool
esCero r = r == Q Pos (O, uno) || r == Q Neg (O, uno)

-- ===========================
-- 1. Eq para Signo
-- ===========================
testsEqSigno :: [Bool]
testsEqSigno =
  [ Pos == Pos
  , Neg == Neg
  , not (Pos == Neg)
  , not (Neg == Pos)
  , Pos /= Neg
  ]

okEqSigno = and testsEqSigno

-- ===========================
-- 2. Eq para Racional
-- ===========================
testsEqRacional :: [Bool]
testsEqRacional =
  -- ejemplos de la letra
  [ Q Pos (dos, dos) == Q Pos (dos, dos)
  , Q Pos (dos, dos) /= Q Pos (dos, tres)
  , not (Q Pos (dos, dos) == Q Neg (dos, dos))
  , Q Neg (tres, tres) == Q Neg (dos, dos)
  -- distintas representaciones del mismo racional
  , Q Pos (uno, dos) == Q Pos (dos, cuatro)            -- 1/2 = 2/4
  , Q Pos (uno, dos) == Q Pos (cuatro, ocho)           -- 1/2 = 4/8
  , Q Pos (seis, cuatro) == Q Pos (tres, dos)          -- 6/4 = 3/2
  , Q Neg (dos, seis) == Q Neg (uno, tres)             -- -2/6 = -1/3
  , Q Pos (cinco, cinco) == Q Pos (uno, uno)           -- 5/5 = 1
  -- racionales distintos
  , not (Q Pos (uno, dos) == Q Pos (uno, tres))        -- 1/2 /= 1/3
  , not (Q Pos (uno, tres) == Q Pos (tres, uno))       -- 1/3 /= 3
  , not (Q Neg (uno, dos) == Q Pos (dos, cuatro))      -- -1/2 /= 2/4
  , Q Neg (dos, tres) /= Q Neg (tres, dos)             -- -2/3 /= -3/2
  -- ceros (con cualquier signo y denominador)
  , Q Pos (O, uno) == Q Neg (O, uno)                   -- cero doble
  , Q Pos (O, dos) == Q Neg (O, cinco)
  , Q Neg (O, tres) == Q Neg (O, uno)
  , not (Q Pos (O, uno) == Q Pos (uno, cinco))         -- 0 /= 1/5
  ]

okEqRacional = and testsEqRacional

-- ===========================
-- 3. Ord para Signo
-- ===========================
testsOrdSigno :: [Bool]
testsOrdSigno =
  [ Neg < Pos
  , Neg <= Pos
  , Pos > Neg
  , Pos >= Pos
  , Neg >= Neg
  , Pos <= Pos
  , not (Pos < Neg)
  , not (Pos <= Neg)
  ]

okOrdSigno = and testsOrdSigno

-- ===========================
-- 4. Ord para Racional
-- ===========================
testsOrdRacional :: [Bool]
testsOrdRacional =
  -- ejemplos de la letra
  [ Q Pos (uno, tres) <= Q Pos (uno, dos)              -- 1/3 <= 1/2
  , not (Q Neg (dos, cinco) > Q Pos (uno, dos))        -- not (-2/5 > 1/2)
  , Q Neg (dos, tres) < Q Neg (uno, dos)               -- -2/3 < -1/2
  , Q Pos (dos, tres) >= Q Pos (cuatro, seis)          -- 2/3 >= 4/6
  -- ambos positivos
  , Q Pos (tres, cuatro) > Q Pos (dos, tres)           -- 3/4 > 2/3
  , Q Pos (cinco, dos) > Q Pos (dos, uno)              -- 5/2 > 2
  , Q Pos (uno, dos) <= Q Pos (dos, cuatro)            -- 1/2 <= 2/4
  , not (Q Pos (uno, dos) < Q Pos (dos, cuatro))       -- not (1/2 < 2/4)
  , not (Q Pos (uno, dos) <= Q Pos (uno, tres))        -- not (1/2 <= 1/3)
  -- ambos negativos
  , Q Neg (tres, dos) < Q Neg (uno, uno)               -- -3/2 < -1
  , Q Neg (cuatro, seis) >= Q Neg (dos, tres)          -- -4/6 >= -2/3
  , not (Q Neg (uno, cuatro) < Q Neg (uno, dos))       -- not (-1/4 < -1/2)
  , Q Neg (uno, cuatro) > Q Neg (uno, dos)             -- -1/4 > -1/2
  -- signos distintos
  , Q Neg (cinco, uno) < Q Pos (uno, diez)             -- -5 < 1/10
  , Q Pos (uno, nueve) > Q Neg (siete, dos)            -- 1/9 > -7/2
  , not (Q Pos (uno, dos) <= Q Neg (uno, dos))         -- not (1/2 <= -1/2)
  -- con ceros
  , Q Pos (O, uno) <= Q Neg (O, dos)                   -- ceros equivalentes
  , Q Neg (O, uno) >= Q Pos (O, tres)                  -- ceros equivalentes
  , not (Q Pos (O, uno) < Q Neg (O, uno))              -- not (0 < 0)
  , Q Neg (uno, dos) < Q Pos (O, uno)                  -- -1/2 < 0
  , Q Neg (O, dos) < Q Pos (uno, cinco)                -- 0 < 1/5
  , Q Neg (uno, tres) < Q Neg (O, dos)                 -- -1/3 < 0
  , not (Q Neg (O, uno) < Q Neg (uno, uno))            -- not (0 < -1)
  ]

okOrdRacional = and testsOrdRacional

-- ===========================
-- 5. Num para Racional (+, *, -)
-- ===========================
testsNumRacional :: [Bool]
testsNumRacional =
  -- + : ejemplos de la letra
  [ Q Pos (uno, tres) + Q Pos (uno, dos) == Q Pos (cinco, seis)
  , Q Pos (uno, tres) + Q Pos (O, dos) == Q Pos (dos, seis)
  , Q Pos (uno, dos) + Q Neg (dos, dos) == Q Neg (dos, cuatro)
  -- + : mismos signos
  , Q Neg (uno, tres) + Q Neg (uno, dos) == Q Neg (cinco, seis)  -- (-1/3) + (-1/2) = -5/6
  , Q Pos (uno, dos) + Q Pos (uno, dos) == Q Pos (uno, uno)      -- 1/2 + 1/2 = 1
  , Q Neg (O, tres) + Q Neg (dos, cinco) == Q Neg (dos, cinco)   -- 0 + (-2/5) = -2/5
  -- + : signos opuestos
  , Q Neg (tres, cuatro) + Q Pos (uno, cuatro) == Q Neg (uno, dos) -- -3/4 + 1/4 = -1/2
  , Q Neg (uno, cuatro) + Q Pos (tres, cuatro) == Q Pos (uno, dos) -- -1/4 + 3/4 = 1/2
  , Q Pos (uno, tres) + Q Neg (tres, cuatro) == Q Neg (cinco, S (S diez)) -- 1/3 + (-3/4) = -5/12
  , esCero (Q Pos (uno, dos) + Q Neg (dos, cuatro))              -- 1/2 + (-2/4) = 0
  , esCero (Q Neg (tres, cinco) + Q Pos (tres, cinco))           -- -3/5 + 3/5 = 0
  -- * : ejemplos de la letra
  , Q Pos (dos, tres) * Q Pos (uno, dos) == Q Pos (dos, seis)
  , Q Pos (tres, dos) * Q Neg (uno, dos) == Q Neg (tres, cuatro)
  -- * : signos
  , Q Neg (dos, tres) * Q Neg (tres, cuatro) == Q Pos (uno, dos) -- (-2/3) * (-3/4) = 1/2
  , Q Neg (uno, uno) * Q Pos (dos, tres) == Q Neg (dos, tres)    -- (-1) * 2/3 = -2/3
  , Q Pos (tres, cinco) * Q Pos (cinco, tres) == Q Pos (uno, uno) -- 3/5 * 5/3 = 1
  , Q Pos (dos, cuatro) * Q Pos (cuatro, dos) == Q Pos (uno, uno) -- 2/4 * 4/2 = 1
  -- * : por cero (cualquier signo)
  , esCero (Q Neg (uno, dos) * Q Pos (O, cinco))
  , esCero (Q Pos (tres, dos) * Q Neg (O, uno))
  , esCero (Q Neg (O, tres) * Q Neg (O, dos))
  -- - : ejemplos de la letra
  , Q Pos (uno, tres) - Q Pos (uno, dos) == Q Neg (uno, seis)
  , Q Pos (uno, tres) - Q Neg (uno, dos) == Q Pos (cinco, seis)
  -- - : otras restas
  , Q Neg (uno, dos) - Q Neg (uno, tres) == Q Neg (uno, seis)    -- (-1/2) - (-1/3) = -1/6
  , Q Neg (uno, tres) - Q Neg (uno, dos) == Q Pos (uno, seis)    -- (-1/3) - (-1/2) = 1/6
  , Q Neg (uno, dos) - Q Pos (uno, dos) == Q Neg (uno, uno)      -- (-1/2) - 1/2 = -1
  , Q Pos (tres, dos) - Q Pos (uno, dos) == Q Pos (uno, uno)     -- 3/2 - 1/2 = 1
  , Q Pos (O, uno) - Q Pos (dos, tres) == Q Neg (dos, tres)      -- 0 - 2/3 = -2/3
  , Q Neg (O, dos) - Q Neg (tres, cuatro) == Q Pos (tres, cuatro) -- 0 - (-3/4) = 3/4
  , esCero (Q Pos (uno, dos) - Q Pos (dos, cuatro))              -- 1/2 - 2/4 = 0
  , esCero (Q Neg (O, uno) - Q Pos (O, uno))                     -- 0 - 0
  ]

okNumRacional = and testsNumRacional
