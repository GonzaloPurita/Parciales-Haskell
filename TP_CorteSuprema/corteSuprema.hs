
data Ley = UnaLey{
    tema :: String,
    presupuesto :: Int,
    sectoresApoyan :: [String]
}deriving(Show,Eq)

medicinal :: Ley
medicinal = UnaLey "Uso medicinal del cannabis " 5 ["partido cambio de todos", "sector financiero"]
educacion :: Ley
educacion = UnaLey "Educación superior" 30 ["docentes universitarios", "partido de centro federal"]
tenisDeMesa :: Ley
tenisDeMesa = UnaLey "Profesionalización del tenista de mesa" 1 ["partido de centro federal", "liga de deportistas autónomos", "club paleta veloz"]
tenis :: Ley
tenis = UnaLey "tenis" 2 ["liga de deportistas autónomos"]

leyesCompatibles :: Ley -> Ley -> String
leyesCompatibles ley1 ley2 = {-- any (`elem` sectoresApoyan ley1) (sectoresApoyan ley2) && --} temaPerteneceAOtro (tema ley1) (tema ley2)    -- filter (\c -> c `elem` tema ley1) (tema ley2)

temaPerteneceAOtro :: String -> String -> Bool
temaPerteneceAOtro _ [] = False
temaPerteneceAOtro [] _ = True
temaPerteneceAOtro (x:strABuscar) (c:str)
                    | x == c = 
                    | otherwise = 