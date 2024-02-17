data Chocobo = Chocobo {
    color :: Color
    fuerza :: Int,
    peso :: Int,
    velocidad :: Int
} deriving (show)

--             nombre chocoboAsociado
type Jinete = (String , Chocobo)
type Jinetes = [Jinete]
type Pista = [Tramo]

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

tiempoTotal :: Pista -> Chocobo -> Float
tiempoTotal unChocobo unaPista = foldl1 ((+) . flip (tiempoTardaRecorrer unChocobo)) unaPista

obtenerPodio :: Jinetes -> Pista -> (Jinete, Jinete, Jinete)
obtenerPodio unosJinetesCompetidores unaPista = take 3 . quicksort (<) . map (tiempoJinete unaPista) unosJinetesCompetidores

tiempoJinete :: Pista -> Jinete -> Float
tiempoJinete unaPista = tiempoTotal unaPista . chocoboDelJinete  

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
tramosGanados :: Tramo -> Jinete -> Int
tramosGanados unTramo unJinete
    |elMejorDelTramo unTramo length . filter (== unJinete) = 

elMasWinner :: Pista -> Jinetes -> String
elMasWinner unTramo unaListaJinetes =  elMejorDelTramo unTramo unaListaJinetes

quinesPueden :: Int -> Tramo -> Jinetes -> Jinetes
quienesPuede unTiempoMaximo unTramo unaListaJinetes = filter ((< unTiempoMaximo) . tiempoJinete unTramo) unaListaJinetes

estadisticas :: Tramo -> Jinetes -> [(String, Int, Float)]
estadisticas unTramo unaListaJinetes = map (estadisticaParaUnJinete unTramo) unaListaJinetes

estadisticasParaUnJinete :: Tramo -> Jinete -> (String, Int, Float)
estadisticasParaUnJinete unTramo unJinete = (nombreJinete unJinete, tramosGanados unTramo unJinete, tiempoJinete unTramo unJinete)

fuePareja :: Tramo -> [Chocobo] -> Bool
fuePareja unTramo unosChocobosParticipantes = 
