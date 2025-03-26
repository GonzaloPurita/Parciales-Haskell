data Chofer = UnChofer{
    nombre :: String,
    kilometrajeAuto :: Float,
    viajes :: [Viaje],
    condicion :: Condicion
}
type Condicion = Viaje -> Bool

sinCondicion :: Condicion
sinCondicion viaje = True
viajeCaro :: Condicion
viajeCaro = (200<).costo
clienteConNLetras :: Int -> Condicion
clienteConNLetras n = (n<).length.nombreCliente.clienteDelViaje
noVivirEnDichaZona :: String -> Condicion
noVivirEnDichaZona zona = (zona/=).vivienda.clienteDelViaje

data Viaje = UnViaje{
    fecha :: String,
    clienteDelViaje :: Cliente,
    costo :: Float
}

data Cliente = UnCliente{
    nombreCliente :: String,
    vivienda :: String
}

lucas :: Cliente
lucas = UnCliente "Lucas" "Victoria"

daniel :: Chofer
daniel = UnChofer "Daniel" 23500 [UnViaje "20/04/2017" lucas 150] (noVivirEnDichaZona "Olivos")
alejandra :: Chofer
alejandra = UnChofer "Alejandra" 180000 [] sinCondicion

-- 4)
choferTomaViaje :: Viaje -> Chofer -> Bool
choferTomaViaje viaje chofer = condicion chofer viaje
-- 5)
liquidacionChofer :: Chofer -> Float
liquidacionChofer chofer = sum (map costo (viajes chofer))

-- 6)
filtrarChoferesQueTomanViaje :: Viaje -> [Chofer] -> [Chofer]
filtrarChoferesQueTomanViaje viaje choferes = filter (`condicion` viaje) choferes

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes choferes = foldl1 menosViajes choferes

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes chofer1 chofer2
            | length (viajes chofer1) >= length (viajes chofer2) = chofer2
            | otherwise = chofer1

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer{viajes = viajes chofer ++ [viaje]}

-- 7)
nito :: Chofer
nito = UnChofer "Nito Infy" 70000 infinitoViajeConLucas sinCondicion

infinitoViajeConLucas :: [Viaje]
infinitoViajeConLucas = UnViaje "Infinito" lucas 50 : infinitoViajeConLucas

-- No se podria calcular la liquidacion por es infinita


-- 8)
-- gongNeng :: Num -> (b -> Bool) -> (a -> b) -> [a] -> Num
-- gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
-- 
