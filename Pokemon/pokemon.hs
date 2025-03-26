
data Pokemon = UnPokemon {
  nombre :: String,
  tipo :: TipoPokemon
}deriving (Show, Eq)

data TipoPokemon = Tipo{
    propio :: String,
    fuerteContra :: String,
    debilContra :: String
}deriving(Show,Eq)

planta :: TipoPokemon
planta = Tipo "planta" "agua" "fuego"
agua :: TipoPokemon
agua = Tipo "agua" "fuego" "planta"
fuego :: TipoPokemon
fuego = Tipo "fuego" "planta" "agua"

charmander :: Pokemon
charmander = UnPokemon "Charmander" fuego
flareon :: Pokemon
flareon = UnPokemon "Flareon" fuego
squirtle :: Pokemon
squirtle = UnPokemon "Squirtle" agua
oddish :: Pokemon
oddish = UnPokemon "Oddish" planta
gyarados :: Pokemon
gyarados = UnPokemon "Gyarados" agua
carpinchos :: Pokemon
carpinchos = UnPokemon "Carpinchos" agua
bulbasur :: Pokemon
bulbasur = UnPokemon "Bulbasur" planta

todosLosPokemon :: [Pokemon]
todosLosPokemon = [charmander, flareon, squirtle, oddish, gyarados, carpinchos, bulbasur]

-- 1)
aCualesLeGana :: Pokemon -> [Pokemon] -> [Pokemon]
aCualesLeGana pokemon = filter (pelea pokemon)

pelea :: Pokemon -> Pokemon -> Bool
pelea pokeAtacante pokeRecibe = fuerteContra (tipo pokeAtacante) == propio (tipo pokeRecibe)

-- 2)
aCuantosLeGana :: Pokemon -> [Pokemon] -> Int
aCuantosLeGana pokemon listaPokemon = length (aCualesLeGana pokemon listaPokemon)

-- 3)
mejorPokemonEnLista :: [Pokemon] -> Pokemon
mejorPokemonEnLista listaPokemon = quienTieneMasPuntos (map (tuplaPokePuntos listaPokemon) listaPokemon)    -- map (tuplaPokePuntos listaPokemon) listaPokemon  creo una tupla con el pokemon y a cuentos gana

tuplaPokePuntos :: [Pokemon] -> Pokemon -> (Pokemon, Int)
tuplaPokePuntos listaPokemon pokemon = (pokemon, aCuantosLeGana pokemon listaPokemon)

quienTieneMasPuntos :: [(Pokemon, Int)] -> Pokemon
quienTieneMasPuntos tuplas = fst (foldl1 quienGana tuplas)

quienGana :: (Pokemon, Int) -> (Pokemon, Int) -> (Pokemon, Int)
quienGana poke1 poke2
            | snd poke1 >= snd poke2 = poke1
            | otherwise = poke2

-- 4)
data Destino = UnGimnasio {nombreGym :: String, siguiente :: Destino} | UnaLiga {contrincantes :: [Pokemon]} deriving(Show,Eq)
ligaKanto :: Destino
ligaKanto = UnaLiga [gyarados, carpinchos, squirtle]
ligaGali :: Destino
ligaGali = UnaLiga [charmander, bulbasur, oddish]

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno _ (UnGimnasio _ _) = True
estaAlHorno pokemon liga = aCuantosLeGana pokemon (contrincantes liga) == 0

-- 5)
gymRoca :: Destino
gymRoca = UnGimnasio "Gimnasio tipo Roca" gymAgua
gymAgua :: Destino
gymAgua = UnGimnasio "Gimnasio tipo Agua" gymElectrico
gymElectrico :: Destino
gymElectrico = UnGimnasio "Gimnasio tipo Electrico" ligaKanto
gymFuego :: Destino
gymFuego = UnGimnasio "Gimnasio tipo Fuego" gymPlanta
gymPlanta :: Destino
gymPlanta = UnGimnasio "Gimnasio tipo Planta" ligaGali

puedoViajar :: Destino -> Destino -> Bool
puedoViajar (UnaLiga _) _ = False
puedoViajar destinoOrigen destinoALlegar
              | siguiente destinoOrigen == destinoALlegar = True
              | otherwise = puedoViajar (siguiente destinoOrigen) destinoALlegar

