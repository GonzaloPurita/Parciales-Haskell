type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

type Rima = Palabra -> Palabra -> Bool

palabrasRiman :: Palabra -> Palabra -> Bool
palabrasRiman palabra1 palabra2 = palabra1 /= palabra2 && rimaAsonante palabra1 palabra2 && rimaConsonante palabra1 palabra2

rimaAsonante :: Rima
rimaAsonante palabra1 palabra2 = (take 2. reverse . filter esVocal) palabra1 == (take 2. reverse . filter esVocal) palabra2

rimaConsonante :: Rima
rimaConsonante palabra1 palabra2 = (take 3. reverse) palabra1 == (take 3. reverse) palabra2

type Conjugacion = Verso -> Verso -> Bool

medianteRimas :: Conjugacion
medianteRimas verso1 verso2 = palabrasRiman ((last.words) verso1) ((last.words) verso2)

anadiplosis :: Conjugacion
anadiplosis verso1 verso2 = (last.words) verso1 == (head.words) verso2

type Patron = Estrofa -> Bool

simple :: Int -> Int -> Patron
simple v1 v2 estrofa = palabrasRiman ((last.words) (estrofa !! v1)) ((last.words) (estrofa !! v2))

esdrujulas :: Patron
esdrujulas estrofa = all (tieneTilde . head . take 3 . (filter esVocal) . reverse . last . words) estrofa

anafora :: Patron
anafora estrofa = all ((primerPalabraEstrofa estrofa ==) . head . words) estrofa

primerPalabraEstrofa :: Estrofa -> Palabra
primerPalabraEstrofa = head . words . head

cadena :: Conjugacion -> Patron
cadena _ [] = True
cadena conjugacion (v1:v2:resto)
        | conjugacion v1 v2 = cadena conjugacion resto
        | otherwise = False

combinaDos :: Patron -> Patron -> Estrofa -> Bool
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

patronAABB :: Patron
patronAABB estrofa = combinaDos (simple 1 2) (simple 3 4) estrofa
patronABAB :: Patron
patronABAB estrofa = combinaDos (simple 1 3) (simple 2 4) estrofa
patronABBA :: Patron
patronABBA estrofa = combinaDos (simple 1 4) (simple 2 3) estrofa
hardcore :: Patron
hardcore estrofa = combinaDos (cadena medianteRimas) esdrujulas estrofa

-- una estrofa con infinitos versos romperia el patron hardcore, primero porque no podria saber si todos los versos terminan con esdrujula, y despues porque si la conjugacion de la cadena siempre es True, nunca terminaria
-- para el patron aabb no habria problema, porque acotas los versos infinitos

-- PUESTA EN ESCENA
data PuestaEnEscena = UnaPuesta{
    publicoExaltado :: Bool,
    potencia :: Float,
    estrofaArtista :: Estrofa,
    artista :: Artista
}deriving(Show,Eq)

type Estilo = PuestaEnEscena -> PuestaEnEscena

gritar :: Estilo
gritar escena = escena{potencia = potencia escena * 1.5}

responderAcote :: Bool -> Estilo
responderAcote efectividad escena
                | efectividad = escena{potencia = potencia escena * 1.2, publicoExaltado = True}
                | otherwise = escena{potencia = potencia escena * 1.2}

tirarTecnicas :: Patron -> Estilo
tirarTecnicas patron escena
                | patron (estrofaArtista escena) = escena{potencia = potencia escena * 1.1, publicoExaltado = True}
                | otherwise = escena{potencia = potencia escena * 1.1}

artistaBase :: PuestaEnEscena -> PuestaEnEscena
artistaBase escena = escena{potencia = 1, publicoExaltado = False}

hacerFreestyle :: Estilo -> PuestaEnEscena -> PuestaEnEscena
hacerFreestyle estilo escena = estilo (artistaBase escena)

type Jurado = [(PuestaEnEscena -> Bool, Float)]

alToke :: Jurado
alToke = [(patronAABB.estrofaArtista, 0.5), (combinaDos esdrujulas (simple 1 4).estrofaArtista, 1), (publicoExaltado, 1), ((1.5<).potencia, 2)]

puntajeJurado :: Jurado -> PuestaEnEscena -> Float
puntajeJurado jurados escena = min 3 (sum (map snd (filter (`fst` escena) jurados)))

-- BONUS
type Batalla = [(PuestaEnEscena, PuestaEnEscena)]

ganadorBatalla :: Batalla -> [Jurado] -> Artista
ganadorBatalla batalla jurados = fst (ganadorCinto (tuplaPuntajesTotales (map (puntajeDeJurados jurados) batalla)))

puntajeDeJurados :: [Jurado] -> (PuestaEnEscena, PuestaEnEscena) -> ((Artista, Float), (Artista, Float))
puntajeDeJurados jurados puestasEnEscena = (((artista.fst) puestasEnEscena, sum (map (`puntajeJurado` fst puestasEnEscena) jurados)), ((artista.snd) puestasEnEscena, sum (map (`puntajeJurado` snd puestasEnEscena) jurados)))

tuplaPuntajesTotales :: [((Artista, Float), (Artista, Float))] -> ((Artista, Float), (Artista, Float))
tuplaPuntajesTotales tuplas = (((fst.fst.head) tuplas, sum (map (snd.fst) tuplas)), ((fst.snd.head) tuplas, sum (map (snd.snd) tuplas)))

ganadorCinto :: ((Artista, Float), (Artista, Float)) -> (Artista, Float)
ganadorCinto rivales
            | (snd.fst) rivales > (snd.snd) rivales = fst rivales
            | otherwise = snd rivales