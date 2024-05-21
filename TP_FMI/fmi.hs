data Pais = UnPais{
    ingreso :: Float,
    secPublico :: Int,
    secPrivado :: Int,
    recursos :: [String],
    deuda :: Float
}deriving(Show,Eq)

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50000000

type Receta = [Estrategia]
type Estrategia = Pais -> Pais

prestar :: Float -> Estrategia
prestar n pais = pais{deuda = deuda pais + n * 2.5}

reducirSecPublico :: Int -> Estrategia
reducirSecPublico x pais = pais{secPublico = max 0 (secPublico pais - x), ingreso = ingreso pais - ingreso pais * porcentajePerCapita pais x}
porcentajePerCapita :: Pais -> Int -> Float
porcentajePerCapita pais x
                    | (secPublico pais - x) > 100 = 0.2
                    | otherwise = 0.15

explotacionRN :: String -> Estrategia
explotacionRN rn pais = pais{deuda = max 0 (deuda pais - 2000000), recursos = filter (rn /=) (recursos pais)}

blindaje :: Estrategia
blindaje pais = (prestar (pbi pais) pais){secPublico = max 0 (secPublico pais - 500)}

pbi :: Pais -> Float
pbi pais = ingreso pais * fromIntegral(secPrivado pais * secPublico pais)

receta1 :: Receta
receta1 = [prestar 200000000, explotacionRN "Mineria"]

-- 4)
zafariola :: [Pais] -> [Pais]
zafariola paices = filter (\pais -> "Petroleo" `elem` recursos pais) paices

totalDeuda :: [Pais] -> Float
totalDeuda paices = sum (map deuda paices)

-- 5)
verificarRecetasOrdenadas :: Pais -> [Receta] -> Bool
verificarRecetasOrdenadas _ [x] = True
verificarRecetasOrdenadas pais (receta1:receta2:recetas) = pbi (aplicarReceta pais receta1) < pbi (aplicarReceta pais receta2) && verificarRecetasOrdenadas pais (receta2:recetas)

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais receta = foldl (\acc estrategia -> estrategia acc) pais receta

-- 6)
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

-- En el caso del 4)a la funcion no terminaria porq nunca encontraria el petroleo

-- Con el 4)b no habria problema porque no interfieren los recursos naturales (rn)


