import Data.List

data Ley = UnaLey{
    tema :: String,
    presupuesto :: Int,
    sectoresApoyan :: [String]
}deriving(Show,Eq)

medicinal :: Ley
medicinal = UnaLey "Uso medicinal del cannabis" 5 ["partido cambio de todos", "sector financiero"]
educacion :: Ley
educacion = UnaLey "Educación superior" 30 ["docentes universitarios", "partido de centro federal"]
tenisDeMesa :: Ley
tenisDeMesa = UnaLey "Profesionalización del tenista de mesa" 1 ["partido de centro federal", "liga de deportistas autónomos", "club paleta veloz"]
tenis :: Ley
tenis = UnaLey "tenis" 2 ["liga de deportistas autónomos"]

leyesCompatibles :: Ley -> Ley -> Bool
leyesCompatibles ley1 ley2 = any (`elem` sectoresApoyan ley1) (sectoresApoyan ley2) &&  (isInfixOf (tema ley1) (tema ley2) || isInfixOf (tema ley2) (tema ley1))

type CriterioJuez = Ley -> Bool
type Agenda = [String]
agenda :: Agenda
agenda = ["Uso medicinal del cannabis", "tenis"]

opinionPublica :: CriterioJuez
opinionPublica ley = tema ley `elem` agenda

apoyoSector :: String -> CriterioJuez
apoyoSector sector ley = sector `elem` sectoresApoyan ley

presupuestoMayorA :: Int -> CriterioJuez
presupuestoMayorA num ley = presupuesto ley > num

apoyaUnSoloPartido :: String -> CriterioJuez
apoyaUnSoloPartido partido ley = partido `elem` sectoresApoyan ley

leyConstitucional :: Ley -> CriterioJuez -> Bool
leyConstitucional ley criterio = criterio ley

siempreAfirmativo :: CriterioJuez
siempreAfirmativo _ = True

leyesNoConstitucionales :: [Ley] -> [CriterioJuez] -> [Ley]
leyesNoConstitucionales leyes jueces = filter (verificarNoConsti jueces) leyes

verificarNoConsti :: [CriterioJuez] -> Ley -> Bool
verificarNoConsti jueces ley = not (all (leyConstitucional ley) jueces)

borocotizar :: [CriterioJuez] -> [CriterioJuez]
borocotizar lista = map (\f -> not.f) lista

coincidencia :: [Ley] -> CriterioJuez -> String -> Bool
coincidencia leyes juez sector = all (verificarSiCoincide juez sector) leyes

verificarSiCoincide :: CriterioJuez -> String -> Ley -> Bool
verificarSiCoincide criterioJuez sector ley
                    | criterioJuez ley = sector `elem` sectoresApoyan ley
                    | otherwise = True

-- Si una ley tuviera infinitos sectores que lo apoyan, podria ser declarada constitucional siempre y cuando el criterio no involucre los partidos
