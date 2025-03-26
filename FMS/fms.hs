type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String

esVocal :: Char -> Bool
esVocal = flip elem "aeiouáéíóú"
tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

--PARTE 1

type Rimas = Palabra -> Palabra -> Bool

palabrasQueRiman :: Rimas
palabrasQueRiman pal1 pal2 = pal1 /= pal2 && (rimaConsonante pal1 pal2 || rimaAsonante pal1 pal2)

rimaConsonante :: Rimas
rimaConsonante pal1 pal2 = take 3 (reverse pal1) == take 3 (reverse pal2)

rimaAsonante :: Rimas
rimaAsonante pal1 pal2 = (take 2.reverse.filter esVocal) pal1 == (take 2.reverse.filter esVocal) pal2

--PARTE 2

type Conjugaciones = Verso -> Verso -> Bool

conjugaciones :: Conjugaciones
conjugaciones v1 v2 = rimaUltimasPalabras v1 v2 || rimaPalTerminaYEmpieza v1 v2

rimaUltimasPalabras :: Conjugaciones
rimaUltimasPalabras v1 v2 = palabrasQueRiman (ultimaPalabra v1) (ultimaPalabra v2)

ultimaPalabra :: Verso -> Palabra
--ultimaPalabra verso = reverse (takeWhile (' '/=) (reverse verso))
ultimaPalabra = reverse . takeWhile (' '/=) . reverse

rimaPalTerminaYEmpieza :: Conjugaciones
rimaPalTerminaYEmpieza v1 v2 = ultimaPalabra v1 == takeWhile (' '/=) v2

--PTM acabo de leer lo de la funcion words, modo tryhard lo mio

--PARTE 3 a)

type Patrones = Estrofa -> Bool

simple :: Int -> Int -> Patrones
--simple estrofa = rimaUltimasPalabras (head estrofa) (last estrofa) || rimaUltimasPalabras (estrofa !! 1) (estrofa !! 2)
-- La forma anterior seria perfecta si solo hubieran estrofas de 4 versos y que solo tendria q rimar la 1 con la 4, y la 2 con la 3
simple numVerso1 numVerso2 estrofa = rimaUltimasPalabras (estrofa !! numVerso1) (estrofa !! numVerso2)

esdrujula :: Patrones
esdrujula = all terminaConEsdrujula

terminaConEsdrujula :: Verso -> Bool
terminaConEsdrujula = tieneTilde . last . take 3 . reverse . filter esVocal . ultimaPalabra
--terminaConEsdrujula verso = tieneTilde (last (take 3 (reverse (filter esVocal (ultimaPalabra verso)))))

primerPalabra :: Verso -> Palabra
primerPalabra = head . words

anafora :: Patrones
anafora estrofa = all (comienzaConLaMismaPal (primerPalabra (head estrofa))) estrofa

comienzaConLaMismaPal :: Palabra -> Verso -> Bool
comienzaConLaMismaPal pal verso = primerPalabra verso == pal

cadena :: Conjugaciones -> Patrones
cadena _ [] = True
cadena _ [v] = True
cadena conj (v1:v2:vs) = conj v1 v2 && cadena conj (v2:vs)

combinaDos :: Patrones -> Patrones -> Estrofa -> Bool
combinaDos patron1 patron2 estrofa = ((patron1 estrofa &&) . patron2) estrofa

--PARTE 3 b)

aabb :: Patrones
aabb = combinaDos (simple 1 2) (simple 3 4)

abab :: Patrones
abab = combinaDos (simple 1 3) (simple 2 4)

abba :: Patrones
abba = combinaDos (simple 1 4) (simple 2 3)

hardcore :: Patrones
hardcore = combinaDos (cadena conjugaciones) esdrujula

--PARTE 3 c)
-- ¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore? ¿Y el aabb?
-- En el aabb si cumpliria porque vos le especificas exactamente que versos del patron queres, entonces no habria problema
-- En el caso de hardcore ya no funcionaria porque funciona con recursividad, y en el caso que siempre sea True, jamas terminara la funcion

--PARTE 4

data Escena = UnaEscena{
    publicoExaltado :: Bool,
    potencia :: Float,
    estrofaArtista :: Estrofa,
    artista :: Artista
}deriving(Show,Eq)

cambiarPotencia :: Float -> Float -> Float
cambiarPotencia potenciaOG porcentaje = potenciaOG * porcentaje

gritar :: Escena -> Escena
gritar escena = escena{potencia = cambiarPotencia (potencia escena) 1.5}

responderAAcote :: Bool -> Escena -> Escena
responderAAcote efectividad escena
                | efectividad = escena{potencia = cambiarPotencia (potencia escena) 1.2, publicoExaltado = True}
                | otherwise = escena{publicoExaltado = False}

tirarTecnicas :: Patrones -> Escena -> Escena
tirarTecnicas patron escena
                | patron (estrofaArtista escena) = escena{potencia = cambiarPotencia (potencia escena) 1.1, publicoExaltado = True}
                | otherwise = escena{publicoExaltado = False}

--PARTE 5

-- data Jurado = UnJurado{
--     condicion :: [Escena -> Bool],
--     puntaje :: Int
-- }

type Jurado = [(Escena -> Bool, Float)]

alToke :: Jurado
alToke = [(aabb . estrofaArtista, 0.5), (combinaDos esdrujula (simple 1 4) . estrofaArtista, 1), (publicoExaltado , 1), ((1.5<) . potencia, 2)]

sumaPuntosJurado :: Escena -> Jurado -> Float
sumaPuntosJurado _ [] = 0
sumaPuntosJurado escena (criterio:restoCriterios)
                    | (fst criterio) escena = min 3 (snd criterio + sumaPuntosJurado escena restoCriterios)
                    | otherwise = sumaPuntosJurado escena restoCriterios

--BONUS

type Batalla = [(Escena, Escena)] -- (EscenaDePrimerArtista, EscenaDeSegundoArtista)

ganadorCinto :: Batalla -> [Jurado] -> Artista
ganadorCinto batalla listaJurados
                | sum (map (sumaPuntosJuradoTotal1 listaJurados) batalla) >= sum (map (sumaPuntosJuradoTotal2 listaJurados) batalla) = artista (fst (head batalla))
                | otherwise = artista (snd (head batalla))

-- map (sumaPuntosJuradoTotal1 listaJurados) batalla   -->  este map me devuelve una lista con todas las sumas de de los jurados, cada posicion es una escena

sumaPuntosJuradoTotal1 :: [Jurado] -> (Escena, Escena) -> Float
sumaPuntosJuradoTotal1 listaJurados tuplaEscena = sum (map (sumaPuntosJurado (fst tuplaEscena)) listaJurados) --Suma de los puntos de todos los jurados sobre esa escena
sumaPuntosJuradoTotal2 :: [Jurado] -> (Escena, Escena) -> Float
sumaPuntosJuradoTotal2 listaJurados tuplaEscena = sum (map (sumaPuntosJurado (snd tuplaEscena)) listaJurados)