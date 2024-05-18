data Persona = UnaPersona{
    edad :: Int,
    items :: [String],
    experiencia :: Int
}deriving(Show,Eq)

dipper :: Persona
dipper = UnaPersona 12 ["soplador", "hojas"] 100

type Criatura = (Int, Persona -> Bool)

siempredetras :: Criatura
siempredetras = (0, const False)

gnomos :: Int -> Criatura
gnomos n = (2 ^ n, ("soplador" `elem`).items)

fantasmas :: Int -> Criatura
fantasmas n = (n * 20, asuntoPendiente n)

asuntoPendiente :: Int -> Persona -> Bool
asuntoPendiente n persona
                | n == 1 = experiencia persona > 10
                | n == 3 = edad persona >= 13 && "disfraz de oveja" `elem` items persona
                | otherwise = False

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura
                    | snd criatura persona = persona{experiencia = experiencia persona + fst criatura}
                    | otherwise = persona{experiencia = experiencia persona + 1}

enfrentarGrupoCriaturas :: Persona -> [Criatura] -> Persona
enfrentarGrupoCriaturas = foldl enfrentarCriatura

-- Segunda Parte
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ listaQuefecta [] = []
zipWithIf funcionAAplicar condicionParaAplicar (xs:xl) (ys:yl)
            | condicionParaAplicar ys = funcionAAplicar xs ys : zipWithIf funcionAAplicar condicionParaAplicar xl yl
            | otherwise = ys : zipWithIf funcionAAplicar condicionParaAplicar (xs:xl) yl

abecedario :: [Char]
abecedario = ['a' .. 'z']

abecedarioDesde :: Char -> [Char]
abecedarioDesde 'a' = abecedario
abecedarioDesde 'z' = 'z' : ['a' .. 'y']
abecedarioDesde c = c : [(siguiente c abecedario) .. 'z'] ++ ['a' .. (anterior c abecedario)]

siguiente :: Char -> [Char] -> Char
siguiente c (cs1:cs2:cl)
            | c == cs1 = cs2
            | otherwise = siguiente c (cs2:cl)

anterior :: Char -> [Char] -> Char
anterior c (cs1:cs2:cl)
            | c == cs2 = cs1
            | otherwise = anterior c (cs2:cl)

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraADesencriptar = comparacionConAbecedario letraADesencriptar (abecedarioDesde letraClave) abecedario

comparacionConAbecedario :: Char -> [Char] -> [Char] -> Char
comparacionConAbecedario letraEncontrar (cs:cl) (abc:resto)
                            | cs == letraEncontrar = abc
                            | otherwise = comparacionConAbecedario letraEncontrar cl resto

cesar :: Char -> String -> String
cesar letraClave frase = zipWithIf desencriptarLetra (\c -> c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']) (repeat letraClave) frase
-- Se usa repeat porq desencriptarLetra siempre tiene q recibir la latera clave

-- BONUS
vigenere :: String -> String -> String
vigenere textoClave frase = zipWithIf desencriptarLetra (\c -> c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']) (concat (repeat textoClave)) frase

--Para reolver el problema que "pdep" se termina y queda vacio, haces que queda una frase infinita de "pdeppdeppdep..."