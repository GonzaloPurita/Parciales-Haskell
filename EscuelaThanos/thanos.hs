
data Guantelete = UnGuantelete{
    material :: String,
    gemas :: [Gema]
}

data Personaje = UnPersonaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving(Show,Eq)

type Universo = [Personaje]

guanteCompleto :: Guantelete -> Bool
guanteCompleto guante = length (gemas guante) == 6 && material guante == "uru"

chasqueo :: Guantelete -> Universo -> Universo
chasqueo guante universo
            | guanteCompleto guante = take (length universo `div` 2) universo
            | otherwise = universo

universoParaPendex :: Universo -> Bool
universoParaPendex = any ((45>).edad)

energiaTotal :: Universo -> Int
energiaTotal universo = sum (map energia (filter ((1<).length.habilidades) universo))

-- SEGUNDA PARTE
type Gema = Personaje -> Personaje

mente :: Int -> Gema
mente debilitar personaje = personaje{energia = energia personaje - debilitar}
alma :: String -> Gema
alma habilidad personaje = personaje{energia = energia personaje - 10, habilidades = filter (habilidad /=) (habilidades personaje)}
espacio :: String -> Gema
espacio planetaX personaje = personaje{planeta = planetaX, energia = energia personaje - 20}
poder :: Gema
poder personaje
        | length (habilidades personaje) <= 2 = personaje{energia = 0, habilidades = []}
        | otherwise = personaje{energia = 0}
tiempo :: Gema
tiempo personaje = personaje{energia = energia personaje - 50, edad = max 18 (edad personaje `div` 2)}
loca :: Gema -> Gema
loca gema personaje = hacerNVeces 2 gema personaje

hacerNVeces :: Int -> Gema -> Personaje -> Personaje
hacerNVeces 0 _ personaje = personaje
hacerNVeces n gema personaje | n>0 = hacerNVeces (n-1) gema (gema personaje)

ejGuantelete :: Guantelete
ejGuantelete = UnGuantelete{
    material = "goma",
    gemas = [tiempo, alma "usar Mjolnir", loca (alma "programación en Haskell")]
}

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (\acc gema -> gema acc) enemigo gemas

-- Punto 6)
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guante persona = separacionGemas (gemas guante) persona

separacionGemas :: [Gema] -> Personaje -> Gema
separacionGemas [gem] _ = gem
separacionGemas (gem1:gem2:gems) personaje
                | energia (gem1 personaje) <= energia (gem2 personaje) = separacionGemas (gem1:gems) personaje
                | otherwise = separacionGemas (gem2:gems) personaje

-- Punto 7)
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : infinitasGemas gema

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos
-- esta funcion no funcionara porque la funcion recursiva nunca terminara de analizar lo que pasa con cada gemaMasPoderosa

-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
-- esta funcion si se podria usar porque acoto la lista infinita de gemas