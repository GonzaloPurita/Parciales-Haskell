
data Auto = UnAuto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
}deriving(Show,Eq)

autoCercaDeOtro :: Auto -> Auto -> Bool
autoCercaDeOtro autoA autoB = autoA /= autoB && abs (distancia autoA - distancia autoB) < 10

type Carrera = [Auto]
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

--PARTE 1

autoTranquilo :: Auto -> Carrera -> Bool
autoTranquilo auto lista = not (any (autoCercaDeOtro auto) lista) && distancia auto > maximum (map distancia lista)

puestoAuto :: Carrera -> Auto -> Int
puestoAuto autos auto = 1 + length (filter (not.vaGanando auto) autos)
--puestoAuto auto carrera = (+1).(length.filter (not.vaGanando auto)) carrera

vaGanando :: Auto -> Auto -> Bool
--vaGanando autoA = (distancia autoA >).distancia
vaGanando autoA autoB = distancia autoA > distancia autoB

--PARTE 2

autoCorra :: Int -> Auto -> Auto
autoCorra tiempo auto = auto{distancia =((distancia auto +).(tiempo*).velocidad) auto}

type Modificador = Int -> Int

alterarVelocidad :: Auto -> Modificador -> Auto
--alterarVelocidad auto modificador = auto{velocidad = modificador (velocidad auto)}
alterarVelocidad auto modificador = auto{velocidad = max 0 ((modificador.velocidad) auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cant auto = alterarVelocidad auto (\vel -> vel - cant)

--PARTE 3

type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto auto = afectarALosQueCumplen (autoCercaDeOtro auto) (bajarVelocidad 50)

miguelitos :: Int -> PowerUp
miguelitos num auto = afectarALosQueCumplen (vaGanando auto) (bajarVelocidad num)

--En jetPack, el auto original si esta dentro de la lista
jetPack :: Int -> PowerUp
jetPack tiempo auto = afectarALosQueCumplen (auto==) (modificacionAuto tiempo)

modificacionAuto :: Int -> Auto -> Auto
modificacionAuto tiempo auto = (autoCorra tiempo (alterarVelocidad auto (*2))) {velocidad = velocidad auto}

--PARTE 4 a)

type Evento = Carrera -> Carrera
type Color = String

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera autos eventos = map (posicionesCarrera (aplicarEventosACarrera autos eventos)) (aplicarEventosACarrera autos eventos)

posicionesCarrera :: Carrera -> Auto -> (Int, Color)
posicionesCarrera carrera auto = (puestoAuto carrera auto - 1, color auto)

aplicarEventosACarrera :: Carrera -> [Evento] -> Carrera
aplicarEventosACarrera autos eventos = foldl (\acc f -> f acc) autos eventos

--PARTE 4 b)

correnTodos :: Int -> Evento
correnTodos tiempo carrera = map (autoCorra tiempo) carrera

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp poder nombreColor carrera = poder (encontrarAutoUsandoColor nombreColor carrera) carrera

encontrarAutoUsandoColor :: Color -> Carrera -> Auto
encontrarAutoUsandoColor nombreColor (auto:restoDeAutos)
                        | color auto == nombreColor = auto
                        | otherwise = encontrarAutoUsandoColor nombreColor restoDeAutos

--PARTE 5 a)
-- Si se quisiera agregar un nuevo power up, un misil teledirigido, que para poder activarlo se deba indicar el color del auto al que se quiere impactar
-- se podria agregar con lo hecho hasta ahora?

-- misilTeledirigido :: Color -> 