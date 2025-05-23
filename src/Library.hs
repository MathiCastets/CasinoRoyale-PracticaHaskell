module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Carta = (Number, String)
numeroCarta = fst
paloCarta = snd

data Jugador = Jugador {
    nombre :: String,
    mano :: [Carta],
    bebidaFavorita :: String
}

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

jamesBond = Jugador "James Bond" pokerDeAses "Martini"
leChiffre = Jugador "Le Chiffre" fullDeJokers "Gin"
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

ocurrenciasDe x = length . filter (== x)
concatenar = foldl (++) []

--1
--a
mayorSegun funcion valor1 = (max $ funcion valor1)  . funcion 

--b el valor que hace maximo a la funcion, no el maximo tras la funcion
--maximoSegun funcion = maximum . 

--c
sinRepetidos [] = []
sinRepetidos (elemento : elementos)
    | elemento `elem` elementos = sinRepetidos elementos
    | otherwise = elemento : sinRepetidos elementos

--2
--a
esoNoSeVale carta = numeroCarta carta > 13 || numeroCarta carta < 1 || notElem (paloCarta carta) palos

--b 
manoMalArmada jugador = (>5).length.mano $ jugador || any esoNoSeVale mano $ jugador 

--3

esPar mano = 