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
relajada :: Float -> Personalidad
relajada relajamiento situacion = modificarSituacion "peligro" (relajamiento +) (modificarSituacion "tension" (flip (-) 10) situacion)

-- 3)
leGana :: Gema -> Gema -> Situacion -> Bool
leGana gema1 gema2 situacion = (fuerza gema1 >= fuerza gema2) && situacionMejor ((personalidad gema1) situacion) ((personalidad gema2) situacion)

fusion :: Situacion -> Gema -> Gema -> Gema
fusion situacion gema1 gema2 = UnaGema (nombreFusion gema1 gema2) (fuerzaFusion gema1 gema2 situacion) (personalidadFusion gema1 gema2)

nombreFusion :: Gema -> Gema -> String
nombreFusion gema1 gema2
                | nombre gema1 == nombre gema2 = nombre gema1
                | otherwise = nombre gema1 ++ nombre gema2

personalidadFusion :: Gema -> Gema -> Personalidad
personalidadFusion gema1 gema2 = personalidad gema2 . personalidad gema1 . map (modificarAspecto (flip (-) 10))

fuerzaFusion :: Gema -> Gema -> Situacion -> Float
fuerzaFusion gema1 gema2 situacion
            | situacionMejor ((personalidadFusion gema1 gema2) situacion) (personalidad gema1 situacion) && situacionMejor ((personalidadFusion gema1 gema2) situacion) (personalidad gema2 situacion) = (fuerza gema1 + fuerza gema2) * 10
            | leGana gema1 gema2 situacion = 7 * fuerza gema1
            | otherwise = 7 * fuerza gema2

-- 5)
fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal situacion gemas = foldl1 (fusion situacion) gemas

-- 6)

-- foo :: (a) -> (a -> c -> Bool) -> ([c]) -> Bool
-- foo x y z = any (== y x).z

-- -- Invocaciones de la función
-- foo 5 (+7) [1..]
-- foo 3 even (map (< 7))
-- foo 3 even [1, 2, 3]
-- foo [1..] head (take 5) [1.. ]
