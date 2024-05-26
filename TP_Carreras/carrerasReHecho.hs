data Auto = UnAuto{
    color :: Color,
    velocidad :: Int,
    distancia :: Int
}deriving(Show,Eq)

auto1 :: Auto
auto1 = UnAuto "rojo" 100 20
auto2 :: Auto
auto2 = UnAuto "azul" 80 15
auto3 :: Auto
auto3 = UnAuto "verde" 70 11
auto4 :: Auto
auto4 = UnAuto "blanco" 60 10
auto5 :: Auto
auto5 = UnAuto "negro" 50 5

type Carrera = [Auto]
todos :: [Auto]
todos = [auto1,auto2,auto3,auto4,auto5]

autoCerca :: Auto -> Auto -> Bool
autoCerca autoPrime otroAuto = autoPrime /= otroAuto && abs (distancia autoPrime - distancia otroAuto) < 10

autoVaTranki :: Auto -> Carrera -> Bool
autoVaTranki auto carrera = not (any (autoCerca auto) carrera) && all (vaGanando auto) carrera

vaGanando :: Auto -> Auto -> Bool
vaGanando autoGanador otroAuto = distancia autoGanador > distancia otroAuto

puestoEnCarrera :: Auto -> Carrera -> Int
puestoEnCarrera auto carrera = 1 + length (filter (`vaGanando` auto) carrera)

corra :: Int -> Auto -> Auto
corra tiempo auto = auto{distancia = distancia auto + tiempo * velocidad auto}

type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad = modificador (velocidad auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad vel auto = alterarVelocidad ((max 0).(flip (-) vel)) auto

type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (autoCerca auto) (bajarVelocidad 50) carrera

miguelitos :: Int -> PowerUp
miguelitos n auto carrera = afectarALosQueCumplen (vaGanando auto) (bajarVelocidad n) carrera

jetPack :: Int -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (auto ==) (efectoJetPack tiempo) carrera

efectoJetPack :: Int -> Auto -> Auto
efectoJetPack tiempo auto = (alterarVelocidad (\vel -> velocidad auto) . corra tiempo . alterarVelocidad (2*)) auto

type Evento = Carrera -> Carrera
type Color = String

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera eventos = map (\auto -> (puestoEnCarrera auto (aplicarEventos carrera eventos), color auto)) (aplicarEventos carrera eventos)

aplicarEventos :: Carrera -> [Evento] -> Carrera
aplicarEventos carrera eventos = foldl (\acc event -> event acc) carrera eventos

correnTodos :: Int -> Evento
correnTodos tiempo = map (corra tiempo)

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp power colorAuto carrera = power (encontrarAuto colorAuto carrera) carrera

encontrarAuto :: Color -> Carrera -> Auto
encontrarAuto colorAuto (auto:resto)
                | color auto == colorAuto = auto
                | otherwise = encontrarAuto colorAuto resto

-- 5)
-- El misil Teledirigido se podria agregar:
-- misil :: Color -> PowerUp
-- misil color auto carrera = afectarALosQueCumplen ((encontrarAuto color carrera) ==) (efecto) carrera

-- De la manera que esta implementadas las funciones autoVaTranki y puestoEnCarrera, no serian compatibles con una carrera de autos infinitos

