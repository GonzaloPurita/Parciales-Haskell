data Personaje = UnPersonaje{
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
}deriving(Show,Eq)

jugador :: Personaje
jugador = UnPersonaje " " 1000 [("Sueter", 1), ("Fogata", 3), ("Pollo", 2), ("Pollo Asado", 1)]

type Material = (String, Int)  -- (material, cant)

data Receta = UnaReceta{
    requerido :: Material,
    componentes :: [Material],
    tiempo :: Int
}deriving(Show,Eq)

fogata :: Receta
fogata = UnaReceta ("Fogata", 1) [("Madera", 1), ("Fosforo", 1)] 10
polloAsado :: Receta
polloAsado = UnaReceta ("Pollo Asado", 1) [("Fogata", 1), ("Pollo", 1)] 300
sueter :: Receta
sueter = UnaReceta ("Sueter", 1) [("Lana", 1), ("Agujas", 1), ("Tintura", 1)] 600

craftear :: Personaje -> Receta -> Personaje
craftear personaje materialACraftear
        | verificoSiSePuedeCraftear (inventario personaje) (componentes materialACraftear) = (eliminarMatUsados (guardarEnInventario personaje (requerido materialACraftear)) (componentes materialACraftear)){puntaje = puntaje personaje + 10 * tiempo materialACraftear}
        | otherwise = personaje{puntaje = max 0 (puntaje personaje - 100)}

verificoSiSePuedeCraftear :: [Material] -> [Material] -> Bool
verificoSiSePuedeCraftear inventary componentesReceta = all (verificoSiLosTieneEnInventario inventary) componentesReceta

verificoSiLosTieneEnInventario :: [Material] -> Material -> Bool
verificoSiLosTieneEnInventario inventary material = any (\matInventary -> fst matInventary == fst material) inventary --material `elem` inventary

guardarEnInventario :: Personaje -> Material -> Personaje
guardarEnInventario personaje material
                    | verificoSiLosTieneEnInventario (inventario personaje) material = personaje{inventario = map (cambioCantMaterial 1 [material]) (inventario personaje)}
                    | otherwise = personaje{inventario = inventario personaje ++ [material]}

-- cambioUnSoloMaterial :: Int -> Material -> Material -> Material
-- cambioUnSoloMaterial cant material materialInventario
--                     | fst material == fst materialInventario = (fst materialInventario, snd materialInventario + cant)
--                     | otherwise = materialInventario

cambioCantMaterial :: Int -> [Material] -> Material -> Material
cambioCantMaterial cant materiales materialInventario
                    | (fst materialInventario) `elem` (map fst materiales) = (fst materialInventario, snd materialInventario + cant)
                    | otherwise = materialInventario

-- cambioCantVariosMateriales :: Int -> [Material] -> Material -> Material
-- cambioCantVariosMateriales cant materiales materialInventario
--                     | materialInventario `elem` materiales = (fst materialInventario, snd materialInventario + cant)
--                     | otherwise = materialInventario

eliminarMatUsados :: Personaje -> [Material] -> Personaje
-- Tengo que plantear 3 opciones:
-- 1) que los materialesAEliminar sean solo de 1 cant en el inventario
-- 2) que los materialesAEliminar sean todos de cantidades mayores a 1 en el inventario
-- 3) que los materialesAEliminar sean mixtos en cuanto a cantidades

eliminarMatUsados personaje materialesAEliminar
                | all (verificoCantidades (1 ==) (inventario personaje)) materialesAEliminar = personaje{inventario = filter (\mateInventario -> not (mateInventario `elem` materialesAEliminar)) (inventario personaje)}
                | all (verificoCantidades (1 <) (inventario personaje)) materialesAEliminar = personaje{inventario = map (cambioCantMaterial (-1) materialesAEliminar) (inventario personaje)}
                | otherwise = personaje{inventario = filter (\mateInventario -> snd mateInventario /= 0) (map (cambioCantMaterial (-1) materialesAEliminar) (inventario personaje))}


verificoCantidades :: (Int -> Bool) -> [Material] -> Material -> Bool
verificoCantidades criterio (mat1:resto) matEliminar
                    | fst mat1 == fst matEliminar = (criterio . snd) mat1
                    | otherwise = verificoCantidades criterio resto matEliminar

-- 2)
crafteoParaDuplicarPuntaje :: Personaje -> [Receta] -> [String]
crafteoParaDuplicarPuntaje _ [] = []
crafteoParaDuplicarPuntaje personaje (receta:resto)
                            | verificoSiSePuedeCraftear (inventario personaje) (componentes receta) && verificoDuplicado personaje receta = fst (requerido receta) : crafteoParaDuplicarPuntaje personaje resto
                            | otherwise = crafteoParaDuplicarPuntaje personaje resto

verificoDuplicado :: Personaje -> Receta -> Bool
verificoDuplicado personaje receta = puntaje (craftear personaje receta) >= 2 * puntaje personaje

crafteoSucesivo :: Personaje -> [Receta] -> Personaje
crafteoSucesivo = foldl craftear

mayorPuntaje :: Personaje -> [Receta] -> String
mayorPuntaje personaje recetas
            | puntaje (crafteoSucesivo personaje recetas) > puntaje (crafteoSucesivo personaje (reverse recetas)) = "Se logra mas puntos en el orden indicado"
            | puntaje (crafteoSucesivo personaje recetas) < puntaje (crafteoSucesivo personaje (reverse recetas)) = "Se logra mas puntos en el orden inverso"
            | otherwise = "Se logra el mismo puntaje"

-- MINE
data Bioma = UnBioma{
    elementos :: [Material],
    necesario :: Material
}deriving(Show,Eq)

-- artico :: Bioma
-- artico = UnBioma [("Hielo",1),()]

type Herramienta = Bioma -> Material

hacha :: Herramienta
hacha bioma = (last.elementos) bioma
espada :: Herramienta
espada bioma = (head.elementos) bioma
pico :: Int -> Herramienta
pico n bioma = elementos bioma !! n
azada :: Herramienta
azada bioma = elementos bioma !! (((`div` 2) . length . elementos) bioma)

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta personaje bioma
        | ((fst.necesario) bioma) `elem` (map fst (inventario personaje)) = (guardarEnInventario personaje (herramienta bioma)){puntaje = puntaje (guardarEnInventario personaje (herramienta bioma)) + 50}
        | otherwise = personaje

-- Si intentas minar un bioma con infinitos materiales, se te romperia el programa si usas el hacha o la azada