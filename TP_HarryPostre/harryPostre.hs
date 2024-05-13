data Postre = UnPostre{
    sabores :: [String],
    peso :: Float,
    temperatura :: Float
}deriving(Show,Eq)

bizcocho :: Postre
bizcocho = UnPostre ["Fruta", "Crema"] 100 25
tarta :: Postre
tarta = UnPostre ["Melaza"] 50 0

type Hechizo = Postre -> Postre

restaParametros :: Float -> Float -> Float
restaParametros parametro porcentaje = parametro - parametro * (porcentaje/10)

incendio :: Hechizo
incendio postre = postre{temperatura = temperatura postre + 1, peso = restaParametros (peso postre) 5}
immobulus :: Hechizo
immobulus postre = postre{temperatura = 0}
wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre{sabores = sabores postre ++ ["Concreto"], peso = restaParametros (peso postre) 10}
diffindo :: Float -> Hechizo
diffindo porcentaje postre = postre{peso = restaParametros (peso postre) porcentaje}
riddikulus :: String -> Hechizo
riddikulus nuevoSabor postre = postre{sabores = sabores postre ++ [reverse nuevoSabor]}
avadaKadavra :: Hechizo
avadaKadavra postre = immobulus (postre{sabores = []})

estanListos :: [Postre] -> Hechizo -> Bool
-- estanListos postres hechizo = all (\postre -> peso (hechizo postre) > 0 && not (null (sabores (hechizo postre))) && temperatura (hechizo postre) > 0) postres
estanListos postres hechizo = all (verificarSiEstanListos.hechizo) postres

verificarSiEstanListos :: Postre -> Bool
verificarSiEstanListos postre = peso postre > 0 && not (null (sabores postre)) && temperatura postre > 0

pesoPromedio :: [Postre] -> Float
pesoPromedio postres = sum (map peso (filter verificarSiEstanListos postres)) / fromIntegral (length (filter verificarSiEstanListos postres)) :: Float

data Mago = UnMago{
    hechizos :: [Hechizo],
    cantHorrorcruxes :: Int
}

asistirAClase :: Hechizo -> Postre -> Mago -> Mago
asistirAClase hechizo postre mago
                | hechizo postre == avadaKadavra postre = mago{hechizos = hechizos mago ++ [hechizo], cantHorrorcruxes = cantHorrorcruxes mago + 1}
                | otherwise = mago{hechizos = hechizos mago ++ [hechizo]}

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (mayorCantSabores postre) (hechizos mago)

mayorCantSabores :: Postre -> Hechizo -> Hechizo -> Hechizo
mayorCantSabores postre hechizo1 hechizo2 
                    | length (sabores (hechizo1 postre)) > length (sabores (hechizo2 postre)) = hechizo1
                    | otherwise = hechizo2

-- Parte de listas infinitas

postresInfinitos :: [Postre]
postresInfinitos = [bizcocho, tarta] ++ postresInfinitos

-- Si preguntas pidiendo ALGUN hechizo, entonces la funcion estanListos la tendria que cambiar, en vez de all por any, y si daria una respuesta siempre y cuando en algun punto haya uno que devuelva True

-- Con hechizos infinitos seria imposible preguntar sobre el mejor hechizo, porque nunca podrias terminar de analizarlos a todos


