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
leyesCompatibles ley1 ley2 = {-- any (`elem` sectoresApoyan ley1) (sectoresApoyan ley2) && --} dropWhile (\c -> c `elem` "tenis") (tema ley1)
