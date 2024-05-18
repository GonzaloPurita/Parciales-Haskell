
data Pokemon = UnPokemon {
  nombre :: String,
  tipo :: TipoPokemon
}deriving (Show, Eq)

data TipoPokemon = Tipo{
    fuerteContra :: TipoPokemon,
    debilContra :: TipoPokemon
}deriving(Show,Eq)

planta :: TipoPokemon
planta = Tipo agua fuego
agua :: TipoPokemon
agua = Tipo fuego planta
fuego :: TipoPokemon
fuego = Tipo planta agua

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
pelea pokeAtacante pokeRecibe = fuerteContra (tipo pokeAtacante) == debilContra (tipo pokeRecibe)

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
data Destino = UnGimnasio {nombreGym :: String, siguiente :: Destino} | UnaLiga {contrincantes :: [Pokemon]}
ej :: Destino
ej = UnGimnasio "si" ej
ligaKanto :: Destino
ligaKanto = UnaLiga [gyarados, carpinchos, squirtle]

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno _ (UnGimnasio _ _) = True
estaAlHorno pokemon liga = aCuantosLeGana pokemon (contrincantes liga) == 0

