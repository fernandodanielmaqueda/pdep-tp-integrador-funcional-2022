module TP where
import Text.Show.Functions
import Data.List
import Data.Maybe

----------------------
-- Código inicial
----------------------

ordenarPor :: Ord a => (b -> a) -> [b] -> [b]
ordenarPor ponderacion =
  foldl (\ordenada elemento -> filter ((< ponderacion elemento).ponderacion) ordenada
                                  ++ [elemento] ++ filter ((>= ponderacion elemento).ponderacion) ordenada) []

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

----------------------------------------------------------------------
-- Definir los tipos de datos y funciones para el TP a partir de acá
----------------------------------------------------------------------

type Color = String
type Velocidad = Int
type Distancia = Int

data Auto = Auto {
  color :: Color,
  velocidad :: Velocidad,
  distancia :: Distancia
} deriving (Show, Eq)

type Carrera = [Auto]

autoa :: Auto
autoa = Auto {color = "rojo", velocidad = 25, distancia = 0}

autob :: Auto
autob = Auto {color = "amarillo", velocidad = 200, distancia = 10}

autoc :: Auto
autoc = Auto { color = "verde", velocidad = 300, distancia = 15}

autod :: Auto
autod = Auto {color = "azul", velocidad = 400, distancia = 20}

autoe :: Auto
autoe = Auto { color = "violeta", velocidad = 25, distancia = 30}

carreraa :: Carrera
carreraa = [autoa,autob,autoc,autod,autoe]

sonDistintos :: Auto -> Auto -> Bool
sonDistintos auto1 = ((color auto1) /=).color

distanciaEntreAutos :: Auto -> Auto -> Distancia
distanciaEntreAutos auto1 = abs.((distancia auto1)-).distancia

distanciaCerca :: Distancia
distanciaCerca = 10

estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = (sonDistintos auto1 auto2) && (((< distanciaCerca).distanciaEntreAutos auto1) auto2)

vaAdelante :: Auto -> Auto -> Bool
vaAdelante auto1 = ((> (distancia auto1)).distancia)

type Cantidad = Int

cuantosLeVanGanando :: Auto -> Carrera -> Cantidad
cuantosLeVanGanando = (length.).filter.vaAdelante

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = ((all (== False).map (estanCerca auto)) carrera) && (((== 0).cuantosLeVanGanando auto) carrera)

type Puesto = Int

puesto :: Auto -> Carrera -> Puesto
puesto = ((+ 1).).cuantosLeVanGanando

type Tiempo = Int

correr :: Tiempo -> Auto -> Auto
correr tiempo auto = auto {distancia = ((+ (distancia auto)).(* tiempo).velocidad) auto}

alterarVelocidadAuto :: (Velocidad -> Velocidad) -> Auto -> Auto
alterarVelocidadAuto modificadorDeVelocidad auto = auto {velocidad = modificadorDeVelocidad (velocidad auto)}

restarVelocidades :: Velocidad -> Velocidad -> Velocidad
restarVelocidades = (max 0.).subtract

bajarVelocidadAuto :: Velocidad -> Auto -> Auto
bajarVelocidadAuto = alterarVelocidadAuto.restarVelocidades

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto autoGatillador = afectarALosQueCumplen (estanCerca autoGatillador) (bajarVelocidadAuto 50)

miguelitos :: Velocidad -> PowerUp
miguelitos velocidadABajar autoGatillador = afectarALosQueCumplen (((not.).vaAdelante) autoGatillador) (bajarVelocidadAuto velocidadABajar)

jetPack :: Tiempo -> PowerUp
jetPack duracion autoGatillador = afectarALosQueCumplen (((not.).sonDistintos) autoGatillador) ((correr duracion))

type Evento = Carrera -> Carrera
type TablaDePosiciones = [(Puesto, Color)]

hacerTablaDePosiciones :: Carrera -> [(Puesto,Color)]
hacerTablaDePosiciones carrera = map (\auto -> (puesto auto carrera,color auto)) carrera

simularCarrera :: Carrera -> [Evento] -> TablaDePosiciones
simularCarrera estadoInicial eventos = (ordenarPor (fst).hacerTablaDePosiciones.foldl (\carrera evento -> evento carrera) estadoInicial) eventos
--flip $

correnTodos :: Tiempo -> Evento
correnTodos tiempo carrera = map (correr tiempo) carrera

buscarAuto :: Color -> Carrera -> Auto
buscarAuto colorAuto carrera = case (find ((== colorAuto).color) carrera) of
  Just auto -> auto

usaPowerUp :: Color -> PowerUp -> Evento
usaPowerUp color powerUp estadoActual = ((flip powerUp estadoActual).buscarAuto color) estadoActual

carreraDeEjemplo :: Carrera
carreraDeEjemplo = map (\color -> Auto color 120 0) [ "rojo", "blanco", "azul", "negro"]

eventosDeEjemplo = [
  correnTodos 30,
  "azul" `usaPowerUp` (jetPack 3),
  "blanco" `usaPowerUp` terremoto,
  correnTodos 40,
  "blanco" `usaPowerUp` (miguelitos 20),
  "negro" `usaPowerUp` (jetPack 6),
  correnTodos 10
  ]

{-
> simularCarrera carreraDeEjemplo eventosDeEjemplo
RESULTADO: [(3,"negro"),(4,"rojo"),(1,"azul"),(2,"blanco")]
-}

-- 5. a.: Sí, la solución desarrollada hasta este punto permite agregar el nuevo power up sin necesidad de cambiar algo de lo desarrollado en los puntos anteriores.
-- > usaPowerUp "azul" (misilTeledirigido "rojo") carreraa

afectarAuto1 autoGatillador autoAfectado
  | ((velocidad autoAfectado) < 50) = afectarAuto2 autoGatillador autoAfectado
  | otherwise = autoAfectado

afectarAuto2 autoGatillador autoAfectado
  | vaAdelante autoGatillador autoAfectado = autoAfectado {velocidad = 10, distancia = ((+5).distancia) autoAfectado}
  | otherwise = autoAfectado {velocidad = 10}

misilTeledirigido :: Color -> PowerUp
misilTeledirigido colorAfectado autoGatillador = afectarALosQueCumplen ((== colorAfectado).color) (afectarAuto1 autoGatillador)