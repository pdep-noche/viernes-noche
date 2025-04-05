import Control.Concurrent.STM (newBroadcastTChan)
import Distribution.Simple.Flag (BooleanFlag)
siguiente :: Integer -> Integer
siguiente nro = nro + 1

calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro


doble :: Integer -> Integer
doble nro = 2 * nro




aproboAlumno :: Integer -> Bool
aproboAlumno nota = nota >= 6


calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (nro, otroNum) = (duplicarPar nro, siguienteImpar otroNum)

duplicarPar :: Integer -> Integer
duplicarPar nro | even nro = doble nro 
                | otherwise = nro

siguienteImpar :: Integer -> Integer
siguienteImpar nro | odd nro = siguiente nro
                   | otherwise = nro


and' :: Bool -> Bool -> Bool
and' cond1   cond2 | cond1 = cond2
                    | otherwise = False 

--Es más declarativa
and'' :: Bool -> Bool -> Bool
and'' True cond2 = cond2
and'' _ _ = False

-- Es más declartiva
or'' :: Bool  -> Bool -> Bool
or''  False False =  False
or'' _ _ =  True

-- Es Menos declarativa
or''' :: Bool -> Bool -> Bool
or''' cond1 cond2| cond1 = True
                 | otherwise = cond2
                    

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)


notaMaxima :: Alumno-> Nota  
notaMaxima (_, nota1, nota2, nota3) =  nota1 `max`  nota2 `max` nota3


cuadruple :: Integer  -> Integer
cuadruple nro = doble (doble nro)


esMayorA :: Integer -> Bool
esMayorA nro = doble (siguiente (nro + 2)) > 10
    
-- triple
-- (\nro -> nro* 3)


--siguiente
-- (\nro -> nro + 1)

--suma
--(\ nro otroNum -> nro + otroNum)


--suma 2
--(\nro -> nro + 2)

