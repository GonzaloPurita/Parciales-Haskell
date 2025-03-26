import Data.List

data Alfajor = UnAlfajor{
    rellenos :: [Relleno],
    peso :: Float,
    dulzor :: Float,
    nombre :: String
}deriving(Show,Eq)

data Relleno = UnRelleno{
    sabor :: String,
    costo :: Float
}deriving(Show,Eq)

ddl :: Relleno
ddl = UnRelleno "Dulce De Leche" 12
mousse :: Relleno
mousse = UnRelleno "Mousse" 15
fruta :: Relleno
fruta = UnRelleno "Fruta" 10

-- Parte 1

jorgito :: Alfajor
jorgito = UnAlfajor [ddl] 80 8 "Jorgito"
havanna :: Alfajor
havanna = UnAlfajor{
    rellenos = [mousse, mousse],
    peso = 60,
    dulzor = 12,
    nombre = "Havanna"
}
capitan :: Alfajor
capitan = UnAlfajor [ddl] 40 12 "Capitán del espacio"

cocienteAlfajor :: Alfajor -> Float
cocienteAlfajor alfajor = dulzor alfajor / peso alfajor
precioAlfajor :: Alfajor -> Float
precioAlfajor alfajor = (2 * peso alfajor) + sum (map costo (rellenos alfajor))
alfajorPotable :: Alfajor -> Bool
alfajorPotable alfajor = (not.null.rellenos) alfajor && all ((head.rellenos) alfajor ==) (rellenos alfajor) && cocienteAlfajor alfajor >= 0.1

-- Parte 2
abaratarAlfajor :: Alfajor -> Alfajor
abaratarAlfajor alfajor = alfajor{peso = max 0 (peso alfajor - 10), dulzor = max 0 (dulzor alfajor - 7)}

renombrarAlfajor :: String -> Alfajor -> Alfajor
renombrarAlfajor nombreNuevo alfajor = alfajor{nombre = nombreNuevo}

agregarCapa :: Relleno -> Alfajor -> Alfajor
agregarCapa capa alfajor = alfajor{rellenos = rellenos alfajor ++ [capa]}

alfajorPremium :: Alfajor -> Alfajor
alfajorPremium alfajor
                | alfajorPotable alfajor = (agregarCapa (head (rellenos alfajor)) . renombrarAlfajor "Premium") alfajor
                | otherwise = alfajor

premiumCiertoGrado :: Int -> Alfajor -> Alfajor
premiumCiertoGrado 0 alfajor = alfajor
premiumCiertoGrado n alfajor | n>0 = premiumCiertoGrado (n-1) (alfajorPremium alfajor)

jorgitito :: Alfajor
jorgitito = (abaratarAlfajor jorgito){nombre = "Jorgitito"}
jorgelin :: Alfajor
jorgelin = (agregarCapa ddl jorgito){nombre = "Jorgelin"}
capitanDelEspacio :: Alfajor
capitanDelEspacio = ((premiumCiertoGrado 4 . abaratarAlfajor) capitan){nombre = "Capitan del espacio de costa a costa"}

data Cliente = UnCliente{
    dinero :: Float,
    criterio :: [Criterio],
    alfajoresActuales :: [Alfajor]
}

type Criterio = Alfajor -> Bool

contieneEnNombre :: String -> Criterio
contieneEnNombre str alfajor = isInfixOf str (nombre alfajor)

criterioCociente :: Float -> Criterio
criterioCociente cociente alfajor = cocienteAlfajor alfajor > cociente

sinEsaCapa :: Relleno -> Criterio
sinEsaCapa capa alfajor = all (capa /=) (rellenos alfajor)

emi :: Cliente
emi = UnCliente 120 [contieneEnNombre "Capitan del espacio"] []
tomi :: Cliente
tomi = UnCliente 100 [contieneEnNombre "premium", criterioCociente 0.15] []
dante :: Cliente
dante = UnCliente 200 [sinEsaCapa ddl, not.alfajorPotable] []
juan :: Cliente
juan = UnCliente 500 [criterioCociente 0.15, contieneEnNombre "Jorgito", contieneEnNombre "premium", sinEsaCapa mousse] []

cualesLeGustan :: [Alfajor] -> Cliente -> [Alfajor]
cualesLeGustan listaAlfajores cliente = filter (verificarSiCumpleCriterio cliente) listaAlfajores

verificarSiCumpleCriterio :: Cliente -> Alfajor -> Bool
verificarSiCumpleCriterio cliente alfajor = all (\f -> f alfajor) (criterio cliente)

comprarAlfajor :: Cliente -> Alfajor -> Cliente
comprarAlfajor cliente alfajor
                | (dinero cliente - precioAlfajor alfajor) < 0 = cliente
                | otherwise = cliente{dinero = dinero cliente - precioAlfajor alfajor, alfajoresActuales = alfajoresActuales cliente ++ [alfajor]}

comprarTodosLosQueGustan :: [Alfajor] -> Cliente -> Cliente
comprarTodosLosQueGustan alfajores cliente = foldl comprarAlfajor cliente alfajores