data Personaje = UnPersonaje{
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
}deriving(Show,Eq)

type Tiempo = Int
type Receta = ([Material], Tiempo)

data Material = UnMaterial{
    receta :: Receta,
    cantidad :: Int
}deriving(Show,Eq)

materiaPrima :: Material
materiaPrima = UnMaterial ([], 0) 1

fogata :: Material
fogata = UnMaterial ([madera, fosforo], 10) 1
madera :: Material
madera = materiaPrima
fosforo :: Material
fosforo = materiaPrima

polloAsado :: Material
polloAsado = UnMaterial ([fogata, pollo], 300) 1
pollo :: Material
pollo = materiaPrima

sueter :: Material
sueter = UnMaterial ([lana, agujas, tintura], 600) 1
lana :: Material
lana = materiaPrima
agujas :: Material
agujas = materiaPrima
tintura :: Material
tintura = materiaPrima

--Parte 1 crafteo

crafteo :: Personaje -> Material -> Personaje
crafteo persona objeto
            | all (verificarMateriales (inventario persona)) (fst (receta objeto)) = eliminacionMateriales (persona{inventario = agregarMaterial persona objeto, puntaje = puntaje persona + (10 * snd (receta objeto))}) (fst (receta objeto))
            | otherwise = persona{puntaje = max 0 (puntaje persona - 100)}

agregarMaterial :: Personaje -> Material -> [Material]
agregarMaterial perso material
                | verificarMateriales (inventario perso) material = map (cambioCantidadIncremento 1 material) (inventario perso) --Verifico si ya tenia el material en el inventario
                | otherwise = material : inventario perso

cambioCantidadIncremento :: Int -> Material -> Material -> Material
cambioCantidadIncremento num material materialACambiar
                | materialACambiar == material = materialACambiar{cantidad = cantidad materialACambiar + num}
                | otherwise = materialACambiar

verificarMateriales :: [Material] -> Material -> Bool
verificarMateriales lista material = material `elem` lista

eliminacionMateriales :: Personaje -> [Material] -> Personaje
--eliminacionMateriales personaje receta = personaje{inventario = filter (not.verificarMateriales receta) (inventario personaje)} --Aca me fijo si las cosas del inventario no pertenecen a la receta, entonces la filtro
eliminacionMateriales personaje receta
                        | all (verificarCantidadUnicidadEnInventario (inventario personaje)) receta = personaje{inventario = filter (not.verificarMateriales receta) (inventario personaje)} --Caso en el q todos los materiales de la receta tengan una sola cantidad en el inventario
                        | all (verificarCantidadDistintaEnInventario (inventario personaje)) receta = personaje{inventario = map (cambioCantidadDecremento (-1) receta) (inventario personaje)} --Caso en el que todos los materiales de la receta tengan mas de una cantidad
                        | otherwise = personaje{inventario = filter (\material -> cantidad material >= 1) (map (cambioCantidadDecremento (-1) receta) (inventario personaje))} --Caso con materiales de receta de distinta cantidad, entonces primero les bajo la cant y despues filtro los q quedaron en negativo

verificarCantidadDistintaEnInventario :: [Material] -> Material -> Bool
verificarCantidadDistintaEnInventario [] _ = False
verificarCantidadDistintaEnInventario (objeto:resto) materialReceta
                                | objeto == materialReceta = cantidad objeto > 1
                                | otherwise = verificarCantidadUnicidadEnInventario resto materialReceta

verificarCantidadUnicidadEnInventario :: [Material] -> Material -> Bool
verificarCantidadUnicidadEnInventario [] _ = False
verificarCantidadUnicidadEnInventario (objeto:resto) materialReceta
                                | objeto == materialReceta = cantidad objeto == 1
                                | otherwise = verificarCantidadUnicidadEnInventario resto materialReceta

cambioCantidadDecremento :: Int -> [Material] -> Material -> Material
cambioCantidadDecremento num receta material
                        | verificarMateriales receta material = material{cantidad = cantidad material + num}
                        | otherwise = material

--Parte 2 crafteo

duplicarPuntajeOMas :: Personaje -> [Receta] -> [Receta]
duplicarPuntajeOMas personaje recetas = filter (verificarSiEsPorLoMenosElDoble personaje) recetas  -- Hago un filter para encontrar los objetos

verificarSiEsPorLoMenosElDoble :: Personaje -> Receta -> Bool
verificarSiEsPorLoMenosElDoble perso receta = puntaje (foldl crafteo perso (fst receta)) >= 2 * puntaje perso   -- Hago un foldl para quedarme con el pj con los crafteos

crafteoSucesivo :: Personaje -> [Receta] -> Personaje
crafteoSucesivo pj [] = pj
crafteoSucesivo pj (receta:restoDeRecetas) = crafteoSucesivo (foldl crafteo pj (fst receta)) restoDeRecetas -- Primero hago los crafteos de la primer receta y asi voy haciendo con el resto
-- crafteoSucesivo = foldl (\ pj receta -> foldl crafteo pj (fst receta))

lograQuedarConMasPuntos :: Personaje -> [Receta] -> Bool
lograQuedarConMasPuntos pj recetas
                        | puntaje pj < puntaje (crafteoSucesivo pj recetas) || puntaje pj < puntaje (crafteoSucesivo pj (reverse recetas)) = True
                        | otherwise = False

--MINE
--Parte 1

data Bioma = UnBioma{
    componentes :: [Material],
    elementoNecesario :: Material
}deriving(Show,Eq)

type Herramienta = Personaje -> Bioma -> Personaje

hacha :: Herramienta
hacha personaje bioma = personaje{inventario = last (componentes bioma) : inventario personaje}
espada :: Herramienta
espada personaje bioma = personaje{inventario = head (componentes bioma) : inventario personaje}
pico :: Int -> Herramienta
pico n personaje bioma = personaje{inventario = componentes bioma !! n : inventario personaje}

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta = herramienta

--Parte 2
pala :: Herramienta
pala personaje bioma = personaje{inventario = componentes bioma !! ((`div` 2) . length . componentes) bioma : inventario personaje}

azada :: Herramienta
azada personaje bioma = personaje{inventario = componentes bioma !! f bioma : inventario personaje} --Quiero agarrar el elemento igual a la cantidad de material del bioma o el ultimo en caso q se pase
-- (\bioma -> min (length (componentes bioma) - 1) (cantidad (elementoNecesario bioma)))

f :: Bioma -> Int
f bioma = min (length (componentes bioma) - 1) (cantidad (elementoNecesario bioma))

-- Hablando de materiales infinitos de bioma, minar con un hacha seria imposible porq no existiria un ultimo material
-- minar con una espada si seria posible porque agarras el primero y la evaluacion perezosa de haskell lo perimitiria
-- minar con una pala tmp seria posible porque no podrias sacar el lenght de la lista de materiales
-- minar con una azada no seria posible por la misma razon que con la pala