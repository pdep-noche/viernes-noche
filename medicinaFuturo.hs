import GHC.IO.Handle.Lock (hUnlock)


data Animal= Raton {nombre :: String, edad :: Double, peso :: Double,
 enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

modificarNombre :: (String -> String ) -> Animal -> Animal
modificarNombre f animal = animal { nombre = (f.nombre)animal}


modificarEdad :: (Double -> Double) -> Animal -> Animal
modificarEdad f animal  = animal { edad = (f.edad)animal} 

modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso f animal = animal { peso= f.peso $ animal}


modificarEnfermedades :: ([String]->[String] ) ->Animal -> Animal
modificarEnfermedades f animal = animal { enfermedades = (f.enfermedades) animal}


hierbaBuena :: Animal -> Animal
hierbaBuena animal = modificarEdad sqrt animal

{-
ghci> hierbaBuena cerebro
Raton {nombre = "Cerebro", edad = 3.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}