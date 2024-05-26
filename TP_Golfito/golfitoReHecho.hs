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

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = x `elem` [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
        | f a > f b = a
        | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (2 * precisionJugador habilidad) 0
madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5
hierros :: Int -> Palo
hierros n habilidad = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad `div` n) (max 0 (n-3))

palos :: [Palo]
-- palos = [putter, madera] ++ map hierros [1 .. 10]
palos = [putter, madera, hierros 1, hierros 2, hierros 3, hierros 4, hierros 5, hierros 6, hierros 7, hierros 8, hierros 9, hierros 10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

data Obstaculo = UnObstaculo{
    condicionSuperar :: Tiro -> Bool,
    efectoAlTiro :: Tiro -> Tiro
}

tunel :: Obstaculo
tunel = UnObstaculo (\tiro -> precision tiro > 90 && altura tiro == 0) (\tiro -> tiro{velocidad = 2 * velocidad tiro, precision = 100, altura = 0})

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo (\tiro -> velocidad tiro > 80 && altura tiro `elem` [1 .. 5]) (\tiro -> tiro{altura = altura tiro `div` largo})

hoyo :: Obstaculo
hoyo = UnObstaculo (\tiro -> precision tiro > 95 && altura tiro == 0 && velocidad tiro `elem` [5 .. 25]) (\tiro -> tiroDetenido)

tiroPostObstaculo :: Tiro -> Obstaculo -> Tiro
tiroPostObstaculo tiro obstaculo
                | condicionSuperar obstaculo tiro = efectoAlTiro obstaculo tiro
                | otherwise = tiroDetenido

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (verificarEfectividadPalo (habilidad jugador) obstaculo) palos

verificarEfectividadPalo :: Habilidad -> Obstaculo -> Palo -> Bool
verificarEfectividadPalo habilidadJugador obs palo = condicionSuperar obs (palo habilidadJugador)

cantObstaculosSuperados :: [Obstaculo] -> Tiro -> Int
cantObstaculosSuperados [] _ = 0
cantObstaculosSuperados (obs:resto) tiro
                        | condicionSuperar obs tiro = 1 + cantObstaculosSuperados resto tiro
                        | otherwise = cantObstaculosSuperados resto tiro

cantObstaculosSuperados' :: [Obstaculo] -> Tiro -> Int
cantObstaculosSuperados' obstaculos tiro = length (takeWhile (`condicionSuperar` tiro) obstaculos)

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = foldl1 (mejorPaloParaElCaso (habilidad jugador) obstaculos) palos

mejorPaloParaElCaso :: Habilidad -> [Obstaculo] -> Palo -> Palo -> Palo
mejorPaloParaElCaso habilidadJugador obstaculos palo1 palo2
                    | cantObstaculosSuperados obstaculos (palo1 habilidadJugador) >= cantObstaculosSuperados obstaculos (palo2 habilidadJugador) = palo1
                    | otherwise = palo2

padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores listaPuntos = map (padre.fst) (filter ((neneGanador listaPuntos /=) . fst) listaPuntos)

neneGanador :: [(Jugador, Puntos)] -> Jugador
neneGanador listaPuntos = fst (foldl1 (mayorSegun snd) listaPuntos)