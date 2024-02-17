data Chocobo = Chocobo {
    pistas :: [Tramo],
    color :: Color
    fuerza :: Int,
    peso :: Int,
    velocidad :: Int
}

--             nombre chocoboAsociado
type Jinete = (String , Chocobo)
type Jinetes = [Jinete]

--          distancia funcionRelacionadaChocobo
type Tramo = (Int , CorreccionVelocidad)
type CorreccionVelocidad = Chocobo -> Chocobo

type Color = Amarillo | Negro | Blanco | Rojo (show)

--correccionVelocidad Chocobo -> Chocobo
f1 chocobo = velocidad chocobo * 2 
f2 chocobo = velocidad chocobo + fuerza chocobo 
f3 chocobo = velocidad chocobo / peso chocobo
quicksort criterio lista

mayorSegun :: (Ord a => Jinete -> a) -> Valor1 -> Valor2 -> Bool
mayorSegun unaFuncion unValor otroValor = unaFuncion $ unValor > unaFuncion $ otroValor

menorSegun :: (a -> a) -> Valor1 -> Valor2 -> Bool
menorSegun = not mayorSegun

tiempoTardaRecorrer :: Tramo -> Chocobo -> Float
tiempoTardaRecorrer unTramo unChocobo = div (distancia unTramo) (velocidadCorregida unChocobo unTramo)

distancia :: Tramo -> Int
distancia unTramo = fst unTramo

velocidadCorregida :: Chocobo -> Tramo -> Float
velocidadCorregida unChocobo unTramo = velocidad . snd unTramo $ unChocobo

tiempoTotalPistasChocobo :: Chocobo -> Float
tiempoTotalPistasChocobo unChocobo = foldl1 ((+) . flip (tiempoTardaRecorrer unChocobo)) . pistas unChocobo

obtenerPodio :: Jinetes -> Tramo -> (Jinete, Jinete, Jinete)
obtenerPodio unosJinetesCompetidores unTramo = take 3 . quicksort (<) . map (tiempoTardaRecorrer unTramo) unosJinetesCompetidores

tiempoJinete :: Jinete -> Tramo -> Float
tiempoJinete unJinete unTramo = tiempoTardaRecorrer unTramo . chocoboDelJinete unJinete 

elMejorDelTramo :: Tramo -> Jinetes -> String
elMejorDelTramo unTramo unaListaJinetes = nombreJinete . foldl1 (quienRecorrioMenorTiempo unTramo) unaListaJinetes

quienRecorrioEnMenorTiempo :: Tramo -> Jinete -> Jinete -> Jinete
quienRecorrioEnMenorTiempo unTramo unJinete otroJinete 
    |tiempoJinete unJinete < tiempoJinete otroJinete = unJinete
    |otherwise = otroJinete

chocoboDelJinete :: Jinete -> Chocobo
chocoboDelJinete unJinete = snd unJinete

nombreJinete :: Jinete -> String
nombreJinete unJinete = fst unJinete

-- si lo recorrio en el menor tiempo es que lo gano
tramosGanados :: Jinete -> Int
tramosGanados unJinete = elMejorDelTramo   length . filter (== unJinete) 

elMasWinner :: Tramo -> Jinetes -> String
elMasWinner unTramo unaListaJinetes = 
