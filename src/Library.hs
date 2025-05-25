module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Carta = (Number, String)

numeroCarta = fst
paloCarta= snd

data Jugador = Jugador {
    nombre :: String,
    mano :: [Carta],
    bebidaPreferida :: String
} deriving (Show)

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]
jamesBond = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre = Jugador "Le Chiffre" fullDeJokers "Gin"
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

--FUNCIONES DISPONIBLES
ocurrenciasDe x = length . filter (== x)
concatenar = concat

--1
--a
mayorSegun f v1 v2
    | f v1 > f v2 = v1
    |otherwise = v2

--b 
maximoSegun f = foldl1 (mayorSegun f)

--c
sinRepetidos [] = []
sinRepetidos (x:xs)
    | x `elem` xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

--2
--a
esoNoSeVale :: Carta -> Bool
esoNoSeVale carta = notElem (paloCarta carta) palos || 1 > numeroCarta carta || 13 < numeroCarta carta

--b
manoMalArmada :: Jugador -> Bool
manoMalArmada jugador= any esoNoSeVale (mano jugador) || 5 < length (mano jugador)

--3
--a

seRepiteXVeces x cartas = any ((x==).flip ocurrenciasDe listaDeNumeros) listaDeNumeros
    where listaDeNumeros = map fst cartas

esPar :: [Carta] -> Bool
esPar = seRepiteXVeces 2

--b
esPierna :: [Carta] -> Bool
esPierna = seRepiteXVeces 3

--c
esColor cartas = (==length listaPalos).length $ filter (head listaPalos==) listaPalos
    where listaPalos = map paloCarta cartas

--d
esFullHouse cartas= esPar cartas && esPierna cartas

--e
esPoker :: [Carta] -> Bool
esPoker = seRepiteXVeces 4

--f
esOtro mano = True

--4
alguienSeCarteo :: [Jugador] -> Bool
alguienSeCarteo jugadores = (length listaDeManos /=) . length $ sinRepetidos listaDeManos
    where listaDeManos = concatMap mano jugadores

--5. a
valores = [(esPar,1), (esPierna,2), (esColor,3), (esFullHouse,4), (esPoker,5), (esOtro, 0)]
valor cartas = maximum $ map puntaje listaJuegosPosibles
    where  
        puntaje = snd
        listaJuegosPosibles = filter (\(juego,_) -> juego cartas) valores

--b
bebidaWinner = bebidaPreferida . maximoSegun (valor.mano)  

 
--6. a
-- nombre . maximoSegun (length.bebidaPreferida ) $ [jamesBond, leChiffre, felixLeiter]

--b
--  maximoSegun (filter esoNoSeVale.mano) [jamesBond, leChiffre, felixLeiter]

--c maximoSegun (negate.length.nombre) [jamesBond, leChiffre, felixLeiter]

--d nombre . maximoSegun (valor.mano) $ jugadores 

--7.a

--ordenar _ [] = []
--ordenar criterio lista=
    

--b
--esEscalera cartas = ordenar (<) cartas 
--esEscaleraDeColor cartas = esEscalera cartas && esColor cartas 