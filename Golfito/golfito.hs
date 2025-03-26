data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

hab1 :: Habilidad
hab1 = Habilidad{fuerzaJugador=10, precisionJugador=25}

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
-- between n m x = elem x [n .. m]

-- maximoSegun f = foldl1 (mayorSegun f)
-- mayorSegun f a b
--   | f a > f b = a
--   | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}
madera :: Palo
madera habilidad = UnTiro{velocidad = 100, precision = precisionJugador habilidad `div` 2, altura = 5}
hierro :: Int -> Palo
hierro n habilidad = UnTiro{velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad `div` n, altura = cambioAltura n}

cambioAltura :: Int -> Int
cambioAltura n
            | n < 3 = 0
            | otherwise = n-3

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

golpe :: Jugador -> Palo -> Tiro
golpe player palo = (palo.habilidad) player

tiro1 :: Tiro
tiro1 = UnTiro 10 95 0

data Obstaculo = UnObstaculo{
  condicion :: Tiro -> Bool,
  postObs :: Tiro -> Tiro
}

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

superarObstaculo :: Obstaculo -> Tiro -> Tiro
superarObstaculo obs tiro = (postObs obs) tiro

tunel :: Obstaculo
tunel = UnObstaculo{condicion = \tiro -> precision tiro > 90 && altura tiro == 0, postObs = superarTunel}
superarTunel :: Tiro -> Tiro
superarTunel tiro
        | condicion tunel tiro = UnTiro (velocidad tiro *2) 100 0
        | otherwise = tiroDetenido

laguna :: Int -> Obstaculo
laguna num = UnObstaculo{condicion = \tiro -> velocidad tiro > 80 && altura tiro `elem` [1,2,3,4,5], postObs = superarLaguna num}
superarLaguna :: Int -> Tiro -> Tiro
superarLaguna largo tiro
                | condicion (laguna largo) tiro = tiro{altura = altura tiro `div` largo}
                | otherwise = tiroDetenido

hoyo :: Obstaculo
hoyo = UnObstaculo{condicion = \tiro -> velocidad tiro `elem` [5 .. 20] && altura tiro == 0 && precision tiro > 95, postObs = superarHoyo}
superarHoyo :: Tiro -> Tiro
superarHoyo tiro = tiroDetenido

--PARTE 4 
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles player obstaculo = filter (filtradoDePalos player obstaculo) palos

filtradoDePalos :: Jugador -> Obstaculo -> Palo -> Bool
-- filtradoDePalos player obstaculo palo = superarObstaculo obstaculo (palo (habilidad player)) /= tiroDetenido
filtradoDePalos player obstaculo palo = condicion obstaculo (palo (habilidad player))

obstaculosSuperados :: [Obstaculo] -> Tiro -> Int
-- obstaculosSuperados [] _ = 0
-- obstaculosSuperados (obs:obstaculos) tiro
--                           | condicion obs tiro = 1 + obstaculosSuperados obstaculos tiro
--                           | otherwise = 0
obstaculosSuperados listaObs tiro = length (takeWhile (`condicion` tiro) listaObs)

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil player listaObs = foldl1 (comparacionPalos player listaObs) palos

comparacionPalos :: Jugador -> [Obstaculo] -> Palo -> Palo -> Palo
comparacionPalos player lista palo1 palo2
                | obstaculosSuperados lista (palo1 (habilidad player)) >= obstaculosSuperados lista (palo2 (habilidad player)) = palo1
                | otherwise = palo2

perdedores :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
perdedores lista = filter (\tupla -> snd tupla /= snd (ganador lista)) lista

listaPadresPerdedores :: [(Jugador, Puntos)] -> [String]
listaPadresPerdedores lista = map (padre.fst) (perdedores lista)

ganador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
ganador = foldl1 comparacion
comparacion :: (Jugador, Puntos) -> (Jugador, Puntos) -> (Jugador, Puntos)
comparacion tupla1 tupla2
            | snd tupla1 >= snd tupla2 = tupla1
            | otherwise = tupla2