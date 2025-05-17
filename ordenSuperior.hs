enterosDesde :: Int -> [Int]
enterosDesde x = x : enterosDesde (x + 1)



cantidadDeElementos :: [a] -> Int
cantidadDeElementos lista = foldl (\sem _ -> sem + 1)    0  lista


cantidadDeElementos' :: [a] -> Int
cantidadDeElementos' lista = foldr  (\_ sem -> sem + 1) 0 lista


masGastador :: [(String,Integer)] -> (String, Integer)
masGastador (emple:lista) = foldl   maxEmple   emple   lista


maxEmple :: (String, Integer) -> (String, Integer) -> (String, Integer)
maxEmple unEmple otroEmple | snd unEmple >= snd otroEmple = unEmple
                           | otherwise = otroEmple

masGastador' ::[(String, Integer)] -> (String, Integer)
masGastador' lista = foldr maxEmple (head lista) (tail lista)


monto :: [(String, Integer)] -> Integer
monto lista = foldl (\sem (_, gasto) -> sem + gasto) 0 lista

monto' :: [(String, Integer)] -> Integer
monto' lista = foldr (\(_, gasto) sem -> sem + gasto) 0 lista
 

 {-
 
 foldl (\sem f -> f sem) 2 [(3+), (*2), (5+)]
 15

  foldl (flip ($)) 2 [(3+), (*2), (5+)] 

 foldr (\f sem -> f sem)   2  [(3+), (*2), (5+)]
 17

 foldr ($)   2  [(3+), (*2), (5+)]    
 -}


type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  200000 ["ing. en sistemas", "contador"],
    Proy "restaurante" 50000 ["cocinero", "adm. de empresas", "contador"] ,
    Proy "ventaChurros" 10000 ["cocinero"] ]



maximoProyectoSegun :: (Proyecto ->  Int) -> [Proyecto] -> Proyecto
maximoProyectoSegun f (proyecto :proyectos)  =  foldl (maxProy f)  proyecto  proyectos 


maxProy :: (Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maxProy f unProy otroProy | f unProy > f otroProy = unProy
                           | otherwise = otroProy


{-
ghci> maximoProyectoSegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}
-}


{-
ghci> maximoProyectoSegun (length.profesionales )    proyectos         
Proy {nombre = "restaurante", inversionInicial = 50000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{-
ghci> maximoProyectoSegun (length.words.nombre) proyectos 
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}
-}



