data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> [Aspecto] -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> [Aspecto] -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> [Aspecto] -> [Aspecto]
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

-- 1)
modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto{grado = f (grado aspecto)}

situacionMejor :: Situacion -> Situacion -> Bool
situacionMejor _ [] = True
situacionMejor mejor (cabeza:resto) = mejorAspecto (buscarAspecto cabeza mejor) cabeza && situacionMejor mejor resto

modificarSituacion :: String -> (Float -> Float) -> Situacion -> Situacion
-- modificarSituacion tipoDEAspecto alteracion situacion = (buscarAspectoDeTipo tipoDEAspecto situacion){grado = alteracion (grado (buscarAspecto tipoDEAspecto situacion))}
modificarSituacion tipoDEAspecto alteracion situacion = map (cambiarElBuscado alteracion (buscarAspectoDeTipo tipoDEAspecto situacion)) situacion

cambiarElBuscado :: (Float -> Float) -> Aspecto -> Aspecto -> Aspecto
cambiarElBuscado alteracion buscado enLista
                    | enLista == buscado = enLista{grado = alteracion (grado enLista)}
                    | otherwise = enLista

-- 2)
data Gema = UnaGema{
    nombre :: String,
    fuerza :: Float,
    personalidad :: Personalidad
}

type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente situacion = modificarSituacion "tension" (flip (-) 10) (modificarSituacion "incertidumbre" (/2) situacion)
-- relajada :: Personalidad
-- relajada situacion = 