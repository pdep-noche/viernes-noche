import Data.List (genericLength)
import Text.Show.Functions
import Text.XHtml (ADDATTRS)
data Atraccion = Atraccion { nombre :: String, alturaMin:: Double, duracion:: Int,  opiniones :: [String], estaMantenimiento:: Bool, reparaciones::[Reparacion]} deriving Show

data Reparacion = Reparacion { dias :: Int, trabajo :: Trabajo} deriving Show

type Trabajo = Atraccion -> Atraccion

montania :: Atraccion
montania = Atraccion "montaña" 150 6 ["fantastico"] False [Reparacion 5 (engrase 10)]

--1 

puntaje :: Atraccion -> Double
puntaje atraccion | (<10).duracion $ atraccion = 100
                  | (<3).length.reparaciones $ atraccion = calcularPuntaje atraccion
                  | otherwise = (10*).alturaMin $ atraccion

calcularPuntaje :: Atraccion -> Double
calcularPuntaje atraccion = ((10*).genericLength.nombre $ atraccion) + ((2*).genericLength.opiniones $ atraccion)

{-
ghci> puntaje montania
100.0
-}


ajusteDeTornilleria :: Int -> Trabajo
ajusteDeTornilleria tornillos atraccion = atraccion { duracion = min (duracion atraccion + tornillos) 10}


engrase :: Double -> Trabajo
engrase grasa atraccion =  agregarOpinion "para valientes" . modificarAltura grasa $ atraccion


modificarAltura :: Double -> Trabajo
modificarAltura cant atraccion = atraccion { alturaMin = alturaMin atraccion + (0.1 *cant)}

agregarOpinion :: String -> Trabajo
agregarOpinion opinion atraccion = atraccion { opiniones = opiniones atraccion ++ [opinion]}

mantenimientoElectrico ::Trabajo
mantenimientoElectrico atraccion = atraccion { opiniones = (take 2.opiniones) atraccion}

mantenimientoBasico :: Trabajo 
mantenimientoBasico atraccion = engrase 10 . ajusteDeTornilleria 8 $ atraccion

---3

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion = any ((>4).dias).reparaciones $ atraccion

{-
ghci> meDaMiedito montania
True
-}

cerramos :: Atraccion -> Bool
cerramos atraccion =  (>7).cantDiasReparacion $ atraccion 


cantDiasReparacion :: Atraccion -> Int
cantDiasReparacion atraccion = foldl (\sem repa -> sem + dias repa) 0.reparaciones $ atraccion

{-
ghci> cerramos montania
False
-}
type Parque = [Atraccion]
disneyNoExistis :: Parque -> Bool
disneyNoExistis parque = all (null. reparaciones).filter((>5).length.nombre) $ parque

{-
ghci> disneyNoExistis [montania]
False
-}

---- 4
tieneReparacionesPiolas :: Atraccion -> Bool
tieneReparacionesPiolas atraccion = sonPiolas (reparaciones atraccion) atraccion

sonPiolas :: [Reparacion] -> Atraccion -> Bool
sonPiolas [] _ = True
sonPiolas [_] _ = True
sonPiolas (repa:otraRepa:resto) atraccion =  puntaje ((trabajo repa) atraccion) < puntaje ((trabajo otraRepa)atraccion) && sonPiolas (otraRepa:resto) atraccion

---5

realizarTrabajos :: Atraccion -> Atraccion
realizarTrabajos atraccion = ponerFueraMantenimiento.eliminarReparaciones.realizarReparaciones $ atraccion

realizarReparaciones :: Atraccion -> Atraccion
realizarReparaciones atraccion = foldl (\atrac reparacion -> (trabajo reparacion) atrac) atraccion . reparaciones $ atraccion

eliminarReparaciones :: Atraccion -> Atraccion
eliminarReparaciones atraccion = atraccion {reparaciones = []}

ponerFueraMantenimiento:: Atraccion -> Atraccion
ponerFueraMantenimiento atraccion = atraccion {estaMantenimiento = False}

{--
ghci> realizarTrabajos montania
Atraccion {nombre = "monta\241a", alturaMin = 151.0, duracion = 6, opiniones = ["fantastico","para valientes"], estaMantenimiento = False, reparaciones = []}
--}

