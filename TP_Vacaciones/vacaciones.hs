data Turista = UnTurista{
    cansancio :: Float,
    stress :: Float,
    solo :: Bool,
    idiomas :: [Idioma]
}deriving(Show,Eq)

type Idioma = String
type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista
            | solo turista = turista{cansancio = cansancio turista - 5}
            | otherwise = turista{stress = stress turista - 1}

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje turista = turista{stress = stress turista - fromIntegral (length paisaje) :: Float}

salirHablarIdioma :: Idioma -> Excursion
salirHablarIdioma idioma turista = turista{idiomas = idiomas turista ++ [idioma], solo = False}

caminar :: Float -> Excursion
caminar tiempo turista = turista{cansancio = cansancio turista + (tiempo * 1 / 4), stress = stress turista - (tiempo * 1 / 4)}

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista
            | marea == "fuerte" = turista{cansancio = cansancio turista + 10, stress = stress turista + 6}
            | marea == "moderada" = turista
            | marea == "tranquila" = (caminar 10 . apreciarPaisaje "mar" . salirHablarIdioma "Aleman") turista

ana :: Turista
ana = UnTurista 0 21 False ["Español"]
beto :: Turista
beto = UnTurista 15 15 True ["Aleman"]
cathi :: Turista
cathi = UnTurista 15 15 True ["Aleman", "Catalan"]

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = (excursion turista){stress = stress turista - (stress turista * 0.1)}

deltaSegun :: (a -> Float) -> a -> a -> Float
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Float) -> Turista -> Excursion -> Float
deltaExcursionSegun indice turista excursion = deltaSegun indice (excursion turista) turista

cantIdiomas :: Turista -> Float
cantIdiomas = fromIntegral.length.idiomas

excursionEducativa :: Excursion -> Turista -> Bool
excursionEducativa excursion turista = deltaExcursionSegun cantIdiomas turista excursion >= 1

excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes excursiones turista = filter (filtradoExcursiones turista) excursiones

filtradoExcursiones :: Turista -> Excursion -> Bool
filtradoExcursiones turista excursion = deltaExcursionSegun stress turista excursion >= 3

-- 3)
type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminar 20, apreciarPaisaje "cascada", caminar 40, irALaPlaya, salirHablarIdioma "melmacquiano"]
tourLadoB :: Excursion -> Tour
tourLadoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120]
tourIslaVecina :: String -> Tour
tourIslaVecina marea
                | marea == "fuerte" = [paseoEnBarco marea, apreciarPaisaje "lago", paseoEnBarco marea]
                | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldl (\acc exc -> exc acc) (turista{stress = stress turista + fromIntegral (length tour) :: Float}) tour

tourConvincente :: [Tour] -> Turista -> Bool
tourConvincente conjuntoTour turista = any (filtradoToursConvincentes turista) conjuntoTour

filtradoToursConvincentes :: Turista -> Tour -> Bool
filtradoToursConvincentes turista tour = (not.null)(excursionesDesestresantes tour turista) && any (\excursion -> (not.solo) (hacerExcursion excursion turista)) tour

efectividadTour :: Tour -> [Turista] -> Float
efectividadTour tour turistas = sum (map (espiritualidad tour) (filter (`filtradoToursConvincentes` tour) turistas))

espiritualidad :: Tour -> Turista -> Float
espiritualidad tour turista = deltaSegun cansancio turista (hacerTour tour turista) + deltaSegun stress turista (hacerTour tour turista)

-- 4)
tourInfinito :: Tour
tourInfinito = cycle tourCompleto

-- Creo que funcionaria con ambos turistas, por la evaluacion perezosa de haskell, con la cual en funciones como (not.null) ya si el filter encuentra por lo menos 1, se cumple
-- lo mismo con el any y si no estoy mal, no importaria el turista q se usa.

-- Para conocer la efectividad, seria imposible por el map, ya que usa la funcion hacerTour, lo cual nunca terminaria