data Elemento = UnElemento{
    tipo :: String,
    ataque :: Personaje-> Personaje,
    defensa :: Personaje-> Personaje
}

data Personaje = UnPersonaje{
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}

-- 1)
sinEfecto :: Personaje -> Personaje
sinEfecto perso = perso

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje{anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje{salud = salud personaje * 1.5}

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio personaje = personaje{salud = max 0 (salud personaje - danio)}

-- 2)
esMalvado :: Personaje -> Bool
esMalvado personaje = any (("Maldad" ==).tipo) (elementos personaje)

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (matarloConUnElemento personaje) enemigos

matarloConUnElemento :: Personaje -> Personaje -> Bool
matarloConUnElemento personaje enemigo = any (\element -> salud (ataque element personaje) == 0) (elementos enemigo)

-- 3)
hacerNVeces :: Int -> (a -> a) -> a -> a
hacerNVeces 0 f valor = valor
hacerNVeces n f valor | n>0 = hacerNVeces (n-1) f (f valor)

concentracion :: Int -> Elemento
concentracion n = UnElemento "Magia" sinEfecto (hacerNVeces n meditar)

esbirro :: Elemento
esbirro = UnElemento "Maldad" (\personaje -> personaje{salud = salud personaje - 1})  sinEfecto

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados n = hacerNVeces n (\lista -> lista ++ lista) [esbirro]

jack :: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katana] 200

katana :: Elemento
katana = UnElemento "Magia" (\personaje -> personaje{salud = max 0 (salud personaje - 1000)}) sinEfecto

aku :: Int -> Float -> Personaje
aku anio saludConstructor = UnPersonaje "Aku" saludConstructor ([concentracion 4] ++ esbirrosMalvados (100 * anio) ++ [portalAlFuturo anio]) anio

portalAlFuturo :: Int -> Elemento
portalAlFuturo anio = UnElemento "Magia" (\personaje -> personaje{anioPresente = 2800 + anio}) (akuFuturo anio)
akuFuturo ::  Int -> Personaje -> Personaje
akuFuturo anio persoPostPortal = persoPostPortal{anioPresente = 2800 + anio}
-- Esto ultimo no esta bien

-- 4)
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
        | danioQueProduce defensor (head (elementos atacante)) == 0 = (atacante, defensor) 
        | aplicarDefensa atacante (head (elementos defensor)) == 0 = (defensor, atacante)
        | otherwise = luchar (defensor{elementos = drop 1 (elementos defensor)}) (atacante{elementos = drop 1 (elementos atacante)})

aplicarDefensa :: Personaje -> Elemento -> Float
aplicarDefensa personaje elemento = salud personaje - salud (defensa elemento personaje)

-- 5)
-- f :: (a -> (a,a)) -> (Num -> a) -> (a) -> ([a])
-- f x y z
--     | y 0 == z = map (fst.x z)
--     | otherwise = map (snd.x (y 0))