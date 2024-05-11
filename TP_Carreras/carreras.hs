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
auto2 = UnAuto "azul" 80 5
auto3 :: Auto
auto3 = UnAuto "verde" 70 2

--PARTE 1

autoTranquilo :: Auto -> Carrera -> Bool
autoTranquilo auto lista = not (any (autoCercaDeOtro auto) lista) && distancia auto > maximum (map distancia lista)

puestoAuto :: Auto -> Carrera -> Int
-- puestoAuto auto autos = 1 + length (filter (leVanGanando auto) autos)
puestoAuto auto = (+1).(length.filter (leVanGanando auto))

leVanGanando :: Auto -> Auto -> Bool
leVanGanando autoA = (distancia autoA <).distancia

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


