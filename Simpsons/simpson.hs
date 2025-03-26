data Personaje = UnPersonaje{
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
}deriving(Show,Eq)

type Actividad = Personaje -> Personaje

lisa :: Personaje
lisa = UnPersonaje{
    nombre = "Lisa",
    dinero = 20,
    felicidad = 70
}

homero :: Personaje
homero = UnPersonaje "Homero" 100 80
skinner :: Personaje
skinner = UnPersonaje "Skinner" 90 30
burns :: Personaje
burns = UnPersonaje "Burns" 1000 50

cambioParametro :: Personaje -> Int -> (Personaje -> Int) -> Int
cambioParametro personaje num funcionAcceso = max 0 (funcionAcceso personaje + num)

irALaEscuela :: Actividad
irALaEscuela personaje
            | personaje == lisa = personaje{felicidad = cambioParametro personaje 20 felicidad}
            | otherwise = personaje{felicidad = cambioParametro personaje (-20) felicidad}

comerDonas :: Int -> Actividad
comerDonas n personaje = personaje{felicidad =  cambioParametro personaje (10*n) felicidad, dinero = cambioParametro personaje (-10) dinero}

irATrabajar :: String -> Actividad
irATrabajar trabajo personaje = personaje{dinero = cambioParametro personaje (length trabajo) dinero}

trabajarDirector :: Actividad
trabajarDirector personaje = irATrabajar "escuela elemental" personaje

-- LOGROS

type Logro = Personaje -> Bool

millo :: Logro
millo personaje = dinero personaje > dinero burns
alegrarse :: Int -> Logro
alegrarse nivel personaje = felicidad personaje > nivel
programaKrosti :: Logro
programaKrosti personaje = dinero personaje >= 10

actDecisiva :: Personaje  -> Logro -> Actividad -> Bool
actDecisiva personaje logro actividad = (not.logro) personaje && (logro.actividad) personaje

aplicarDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
aplicarDecisiva personaje logro actividades
                | null (filter (actDecisiva personaje logro) actividades) = personaje
                | otherwise = (head (filter (actDecisiva personaje logro) actividades)) personaje

actInfinitas :: [Actividad]
actInfinitas = [irALaEscuela, trabajarDirector] ++ map comerDonas [1 ..]

