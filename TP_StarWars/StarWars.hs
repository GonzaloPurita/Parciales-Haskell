data Nave = UnaNave{
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Nave -> Nave
}

fighter :: Nave
fighter = UnaNave "TIE Fighter" 200 100 50 turbo
turbo :: Nave -> Nave
turbo nave = nave{ataque = ataque nave + 25}

wing :: Nave
wing = UnaNave "X Wing" 300 150 100 reparacionEmergencia
reparacionEmergencia :: Nave -> Nave
reparacionEmergencia nave = nave{durabilidad = durabilidad nave + 50, ataque = max (ataque nave) (ataque nave - 30)}

vader :: Nave
vader = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo
superTurbo :: Nave -> Nave
superTurbo = hacerNVeces 3 turbo.reducirDurabilidad 45     -- (\nave -> nave{durabilidad = max (durabilidad nave) (durabilidad nave - 45)})
reducirDurabilidad :: Int -> Nave -> Nave
reducirDurabilidad num nave = nave{durabilidad = max (durabilidad nave) (durabilidad nave - num)}

hacerNVeces :: Int -> (a -> a) -> a -> a
hacerNVeces 0 f valor = valor
hacerNVeces n f valor | n>0 = hacerNVeces (n-1) f (f valor)

falcon :: Nave
falcon = UnaNave "Millennium Falcon" 1000 500 50 reparacionYincrementarEscudos
reparacionYincrementarEscudos :: Nave -> Nave
reparacionYincrementarEscudos = reparacionEmergencia.incrementarEscudos 100
incrementarEscudos :: Int -> Nave -> Nave
incrementarEscudos num nave = nave{escudo = escudo nave + num}

--PARTE 2

durabilidadTotal :: [Nave] -> Int
durabilidadTotal = sum . map durabilidad

--PARTE 3

naveEsAtacada :: Nave -> Nave -> Nave
naveEsAtacada naveAtacada naveAtacante = reducirDurabilidad (max 0 (ataque naveAtacante - escudo naveAtacada)) (poder naveAtacada naveAtacada)

fueraDeCombate :: Nave -> Bool
fueraDeCombate = (0==) . durabilidad

type Estrategia = Nave -> Bool

misionSorpresa :: Nave -> Estrategia -> [Nave] -> [Nave]
misionSorpresa nave estrategia = map (aplicarEstrategia estrategia nave)

aplicarEstrategia :: Estrategia -> Nave -> Nave -> Nave
aplicarEstrategia estrategia naveMision n2
                | estrategia n2 = naveEsAtacada n2 naveMision
                | otherwise = n2

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesConCiertaPeligrosidad :: Int -> Estrategia
navesConCiertaPeligrosidad num nave = ataque nave > num

navesQueQuedarianFueraDeForma :: Nave -> Estrategia
navesQueQuedarianFueraDeForma n1 n2 = fueraDeCombate (naveEsAtacada n2 n1)

minimizarDurabilidadFlota :: Nave -> [Nave] -> Estrategia -> Estrategia -> [Nave]
minimizarDurabilidadFlota nave flota estrategia1 estrategia2
                            | durabilidadTotal (filter estrategia1 flota) <= durabilidadTotal (filter estrategia2 flota) = misionSorpresa nave estrategia1 flota
                            | otherwise = misionSorpresa nave estrategia2 flota

flotaInfinita :: [Nave]
flotaInfinita = cycle [fighter, wing, vader, falcon]

--No se podria sacar la durabilidadTotal porque no terminaria nunca de hacer el map
--Tampoco se podria hacer una mision por el mismo motivo