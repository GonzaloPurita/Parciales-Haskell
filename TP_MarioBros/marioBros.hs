data Plomero = UnPlomero{
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Float
}

data Herramienta = UnaHerramienta{
    denominacion :: String,
    precio :: Float,
    material :: String
}deriving(Show,Eq)

data Reparacion = UnaReparacion{
    descripcion :: String,
    requerimiento :: Plomero -> Bool
}

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa, martillo] [] 1200
llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave Inglesa" 200 "Hierro"
martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 "Madera"

wario :: Plomero
wario = UnPlomero "Wario" llavesInfinitas [] 0.5
llavesInfinitas :: [Herramienta]
llavesInfinitas = map (\n -> UnaHerramienta{denominacion="Llave Inglesa", precio=n, material="Hierro"}) [1 ..]

-- 2)
herramientaConDenominacion :: Plomero -> Bool
herramientaConDenominacion plomero = any ((not.null) . denominacion) (cajaHerramientas plomero)

esMalvado :: Plomero -> Bool
esMalvado = ("Wa"==) . take 2 . nombre

puedeComprarHerramientas :: Float -> Plomero -> Bool
puedeComprarHerramientas cantRequerida = (cantRequerida <=) . dinero

-- 3)
herramientaBuena :: Herramienta -> Bool
herramientaBuena herramienta = (material herramienta == "Hierro" && precio herramienta > 10000) || (herramienta == martillo{material = "Madera"} || herramienta == martillo{material = "Goma"})

-- 4)
comprarHerramientas :: Plomero -> Herramienta -> Plomero
comprarHerramientas plomero herramienta
                        | puedeComprarHerramientas (precio herramienta) plomero = plomero{cajaHerramientas = cajaHerramientas plomero ++ [herramienta]}
                        | otherwise = plomero

-- 5)
filtracion :: Reparacion
filtracion = UnaReparacion "una filtración de agua" (elem llaveInglesa.cajaHerramientas)

reparacionDificil :: Reparacion -> Bool
reparacionDificil reparacion = length (descripcion reparacion) > 100 && all (`elem` ['A' .. 'Z']) (descripcion reparacion)

presupuestoReparacion :: Reparacion -> Float
presupuestoReparacion reparacion = (fromIntegral (length (descripcion reparacion)) :: Float) * 3

-- 6)
hacerReparacion :: Plomero -> Reparacion -> Plomero
hacerReparacion plomero reparacion
                    | not (requerimiento reparacion plomero) = verificarPlomero (plomero{dinero = dinero plomero + 100}) reparacion
                    | requerimiento reparacion plomero = verificarPlomero (plomero{dinero = dinero plomero + presupuestoReparacion reparacion, reparaciones = reparaciones plomero ++ [reparacion]}) reparacion

verificarPlomero :: Plomero -> Reparacion -> Plomero
verificarPlomero plomero reparacion
                | esMalvado plomero = plomero{cajaHerramientas = cajaHerramientas plomero ++ [UnaHerramienta{denominacion="Destornillador", precio=0, material="Plastico"}]}
                | (not.esMalvado) plomero && reparacionDificil reparacion = plomero{cajaHerramientas = filter (not.herramientaBuena) (cajaHerramientas plomero)}
                | (not.esMalvado) plomero && (not.reparacionDificil) reparacion = plomero{cajaHerramientas = drop 1 (cajaHerramientas plomero)}

-- 7)
jornadaDeTrabajo :: [Reparacion] -> Plomero -> Plomero
jornadaDeTrabajo listaReparaciones plomero = foldl hacerReparacion plomero listaReparaciones

-- 8)a
masLaburador :: [Reparacion] -> [Plomero] -> Plomero
masLaburador listaReparaciones plomeros = encontrarAlMasLaburante (map (jornadaDeTrabajo listaReparaciones) plomeros)

encontrarAlMasLaburante :: [Plomero] -> Plomero
encontrarAlMasLaburante = foldl1 masReparaciones

masReparaciones :: Plomero -> Plomero -> Plomero
masReparaciones plomero1 plomero2
                | length (reparaciones plomero1) >= length (reparaciones plomero2) = plomero1
                | otherwise = plomero2

-- 8)b
masAdinerado :: [Reparacion] -> [Plomero] -> Plomero
masAdinerado listaReparaciones plomeros = encontrarAlMasAdinerado (map (jornadaDeTrabajo listaReparaciones) plomeros)

encontrarAlMasAdinerado :: [Plomero] -> Plomero
encontrarAlMasAdinerado = foldl1 masDinero

masDinero :: Plomero -> Plomero -> Plomero
masDinero plomero1 plomero2
            | dinero plomero1 >= dinero plomero2 = plomero1
            | otherwise = plomero2

-- 8)c
elQueMasInvirtio :: [Reparacion] -> [Plomero] -> Plomero
elQueMasInvirtio listaReparaciones plomeros = encontrarAlMasInvirtio (map (jornadaDeTrabajo listaReparaciones) plomeros)

encontrarAlMasInvirtio :: [Plomero] -> Plomero
encontrarAlMasInvirtio = foldl1 masInvirtio

masInvirtio :: Plomero -> Plomero -> Plomero
masInvirtio plomero1 plomero2
            | sum (map precio (cajaHerramientas plomero1)) >= sum (map precio (cajaHerramientas plomero2)) = plomero1
            | otherwise = plomero2

