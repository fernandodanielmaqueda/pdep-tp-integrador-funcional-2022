module TP where
import Text.Show.Functions
import Data.List

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

-- Tipos de datos iniciales

type Color = String
type Velocidad = Int
type Distancia = Int

data Auto = Auto {
  color :: Color,
  velocidad :: Velocidad,
  distancia :: Distancia
} deriving (Show, Eq)

type Carrera = [Auto]

-- Punto 1.

sonDistintos :: Auto -> Auto -> Bool
sonDistintos auto1 auto2 = (color auto1) /= (color auto2)

distanciaEntreAutos :: Auto -> Auto -> Distancia
distanciaEntreAutos auto1 = abs.((distancia auto1)-).distancia

  -- Ítem a.
estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = (sonDistintos auto1 auto2) && (((< distanciaCerca).distanciaEntreAutos auto1) auto2)
  where
    distanciaCerca = 10

vaAtrasDe :: Auto -> Auto -> Bool
vaAtrasDe auto1 = ((> (distancia auto1)).distancia)

type Cantidad = Int

cuantosLeVanGanando :: Auto -> Carrera -> Cantidad
-- cuantosLeVanGanando'  = (length.).filter.vaAtrasDe 
cuantosLeVanGanando unAuto = length . filter (vaAtrasDe unAuto) 

noTieneNingunAutoCerca :: Auto -> Carrera -> Bool
noTieneNingunAutoCerca auto = not.any (estanCerca auto)

lesVaGanandoATodos :: Auto -> Carrera -> Bool
-- lesVaGanandoATodos' = ((== 0).).cuantosLeVanGanando
lesVaGanandoATodos unAuto = (== 0) . cuantosLeVanGanando unAuto

 -- Ítem b.
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (noTieneNingunAutoCerca auto carrera) && (lesVaGanandoATodos auto carrera)

type Puesto = Int

  -- Ítem c.
puesto :: Auto -> Carrera -> Puesto
puesto auto = (+1).cuantosLeVanGanando auto
--puesto' = ((+ 1).).cuantosLeVanGanando
-- Punto 2.

type Tiempo = Int

  -- Ítem a.
correr :: Tiempo -> Auto -> Auto
correr tiempo auto = auto {distancia = ((+ (distancia auto)).(* tiempo).velocidad) auto}

  -- Ítem b.

    -- Apartado i.
alterarVelocidadAuto :: (Velocidad -> Velocidad) -> Auto -> Auto
alterarVelocidadAuto modificadorDeVelocidad auto = auto {velocidad = modificadorDeVelocidad (velocidad auto)}

restarVelocidades :: Velocidad -> Velocidad -> Velocidad
restarVelocidades velocidadRestar = (max 0).subtract velocidadRestar
--restarVelocidades' = (max 0.).subtract

    -- Apartado ii.
bajarVelocidadAuto :: Velocidad -> Auto -> Auto
bajarVelocidadAuto velocidad = alterarVelocidadAuto (restarVelocidades velocidad)

--bajarVelocidadAuto' = alterarVelocidadAuto.restarVelocidades

-- Punto 3.

type PowerUp = Auto -> Carrera -> Carrera

  -- Ítem a.
terremoto :: PowerUp
terremoto autoGatillador = afectarALosQueCumplen (estanCerca autoGatillador) (bajarVelocidadAuto 50)

  -- Ítem b.
miguelitos :: Velocidad -> PowerUp
miguelitos velocidadABajar autoGatillador = afectarALosQueCumplen (not.vaAtrasDe autoGatillador) (bajarVelocidadAuto velocidadABajar)

  -- Ítem c.
jetPack :: Tiempo -> PowerUp
jetPack duracion autoGatillador = afectarALosQueCumplen (not.sonDistintos autoGatillador) ((alterarVelocidadAuto (`div` 2).correr duracion).alterarVelocidadAuto (*2))

-- Punto 4.

type Evento = Carrera -> Carrera
type TablaDePosiciones = [(Puesto, Color)]

generarTablaDePosiciones :: Carrera -> [(Puesto,Color)]
generarTablaDePosiciones carrera = map (\auto -> (puesto auto carrera,color auto)) carrera

puestoCarrera :: (Puesto, Color) -> Puesto
puestoCarrera = fst

  -- Ítem a.
simularCarrera :: Carrera -> [Evento] -> TablaDePosiciones
simularCarrera estadoInicial eventos = (ordenarPor puestoCarrera.generarTablaDePosiciones.foldl (flip ($)) estadoInicial) eventos

  -- Ítem b.

    -- Apartado i.
correnTodos :: Tiempo -> Evento
correnTodos tiempo carrera = map (correr tiempo) carrera

buscarAuto :: Color -> Carrera -> Auto
buscarAuto colorAuto = head.filter ((== colorAuto).color)

    -- Apartado ii.
usaPowerUp :: Color -> PowerUp -> Evento
usaPowerUp colorGatillador powerUp estadoActual = ((flip powerUp estadoActual).buscarAuto colorGatillador) estadoActual

  -- Ítem c.

carreraDeEjemplo :: Carrera
carreraDeEjemplo = map (\colorAuto -> Auto colorAuto 120 0) [ "rojo", "blanco", "azul", "negro"]

eventosDeEjemplo :: [Evento]
eventosDeEjemplo = [
  correnTodos 30,
  "azul" `usaPowerUp` (jetPack 3),
  "blanco" `usaPowerUp` terremoto,
  correnTodos 40,
  "blanco" `usaPowerUp` (miguelitos 20),
  "negro" `usaPowerUp` (jetPack 6),
  correnTodos 10
  ]

      -- > simularCarrera carreraDeEjemplo eventosDeEjemplo
      -- Resultado: [(1,"azul"),(2,"blanco"),(3,"negro"),(4,"rojo")]

-- Punto 5.

  -- Ítem a.
    -- Sí, la solución desarrollada hasta este punto permite agregar el nuevo power up sin necesidad de
    -- cambiar algo de lo desarrollado en los puntos anteriores, porque se lo puede implementar como un
    -- PowerUp más (como los del Punto 3.) y luego utilizarlo usando la función usaPowerUp ya desarrollada:
    -- > usaPowerUp "azul" (misilTeledirigido "rojo") carreraDeEjemplo

activarMisilSi :: Auto -> Auto -> Auto
activarMisilSi autoGatillador autoObjetivo
  | ((velocidad autoObjetivo) < 50) = impactarMisil autoGatillador autoObjetivo
  | otherwise = autoObjetivo

impactarMisil :: Auto -> Auto -> Auto
impactarMisil autoGatillador autoAfectado
  | vaAtrasDe autoGatillador autoAfectado = autoAfectado {velocidad = 10, distancia = ((+5).distancia) autoAfectado}
  | otherwise = autoAfectado {velocidad = 10}

  -- Ítem b.
misilTeledirigido :: Color -> PowerUp
misilTeledirigido colorAfectado autoGatillador = afectarALosQueCumplen ((== colorAfectado).color) (activarMisilSi autoGatillador)

{- Ejemplos para pruebas

autoA :: Auto
autoA = Auto {color = "rojo", velocidad = 25, distancia = 0}

autoB :: Auto
autoB = Auto {color = "amarillo", velocidad = 200, distancia = 10}

autoC :: Auto
autoC = Auto {color = "verde", velocidad = 300, distancia = 15}

autoD :: Auto
autoD = Auto {color = "azul", velocidad = 400, distancia = 20}

autoE :: Auto
autoE = Auto {color = "violeta", velocidad = 25, distancia = 30}

carreraA :: Carrera
carreraA = [autoA,autoB,autoC,autoD,autoE]

-}