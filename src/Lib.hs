module Lib where
import Text.Show.Functions

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------
--Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {
  velocidad= 10, 
  precision = precisionJugador habilidad*2,
  altura = 0
  }

--El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
madera :: Palo
madera habilidad = UnTiro {
  velocidad = 100,
  precision = precisionJugador habilidad `div` 2,
  altura = 5
  }

--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.

hierro :: Int -> Palo
hierro numeroHierro habilidad 
                             |between 1 10 numeroHierro = UnTiro {
                               velocidad = fuerzaJugador habilidad * numeroHierro,
                               precision = precisionJugador habilidad `div` numeroHierro,
                               altura = (numeroHierro-3) `max` 0
                             }
--alturaMinima numeroHierro 
--                         | numeroHierro <= 3 = 0
--                         | otherwise = numeroHierro - 3

palos = [putter,madera] ++ map hierro [1 .. 10]
--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.

----------------------------------
--Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
--Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
golpe :: Palo -> Jugador -> Tiro
golpe palo = palo . habilidad

------------------------------------------------
--Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:
--a)
--type Obstaculo = Jugador -> Palo -> Tiro
type Obstaculo = Tiro -> (Tiro,Bool)
---
-- ¿En qué fallé?
--Pensé mal el Obstaculo, porque en realidad es Tiro -> Tiro
{-
tunelConRampa :: Obstaculo
tunelConRampa jugador palo = (precision) (golpe palo jugador) > 90 && (altura) (golpe palo jugador) == 0
  --(precision (golpe palo jugador)) > 90 && (altura (golpe palo jugador))
--esSuperado tiro criterio1 criterio2 = tiro criterio1 && tiro criterio2
laguna :: Obstaculo
laguna palo jugador = (velocidad) (golpe palo jugador) > 90 && (between 1 5 (altura (golpe palo jugador))) 

hoyo :: Obstaculo
hoyo palo jugador = (altura) (golpe palo jugador) == 0 && (between 5 20 (velocidad (golpe palo jugador))) 
-}

tiroQuieto :: Tiro -> Tiro
tiroQuieto tiro= tiro {velocidad=0,precision=0,altura=0}
alRazDelSuelo :: Tiro -> Bool
alRazDelSuelo tiro = (altura tiro) == 0 

superaObstaculo condicion efecto tiro
                                     |condicion tiro = (efecto tiro,True)
                                     |otherwise = (tiroQuieto tiro,False)

tunelConRampa :: Obstaculo
tunelConRampa tiro = superaObstaculo superaRampa efectoRampa tiro

efectoRampa :: Tiro -> Tiro
efectoRampa tiro = tiro {velocidad=velocidad tiro *2,precision=100,altura=0}

superaRampa :: Tiro -> Bool
superaRampa tiro = precision tiro > 90 && alRazDelSuelo tiro

laguna :: Int->Obstaculo
laguna largoLaguna tiro = superaObstaculo superaLaguna (efectoLaguna largoLaguna) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna tiro = tiro {altura= (altura tiro) `div` largoLaguna}
superaLaguna :: Tiro -> Bool
superaLaguna tiro = (velocidad tiro) > 80 && ((between 1 5.altura) tiro)

hoyo :: Obstaculo
hoyo tiro = superaObstaculo superaHoyo efectoHoyo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo = tiroQuieto
superaHoyo :: Tiro -> Bool
superaHoyo tiro = alRazDelSuelo tiro && ((between 5 20.velocidad) tiro) && (precision tiro) > 95

--palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirve jugador obstaculo) palos


leSirve jugador obstaculo palo = snd.obstaculo $ (golpe palo jugador) 


obstaculos1 = [tunelConRampa,tunelConRampa,hoyo]

superaObstaculosConsecutivos :: Tiro -> [Obstaculo] -> Int
superaObstaculosConsecutivos tiro [] = 0
superaObstaculosConsecutivos tiro (obstaculo : obstaculos)
  |esSuperado obstaculo tiro =
     1 + superaObstaculosConsecutivos (efectoEnTiro obstaculo tiro) obstaculos
  |otherwise = 0

esSuperado :: Obstaculo -> Tiro -> Bool
esSuperado obstaculo = snd.obstaculo

efectoEnTiro :: Obstaculo -> Tiro -> Tiro
efectoEnTiro obstaculo = fst.obstaculo

