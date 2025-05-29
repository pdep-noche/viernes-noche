
data Animal= Raton {nombre :: String, edad :: Double, peso :: Double,
 enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
orejudo = Raton "Orejudo" 4.0 10.0 ["obesidad", "sinusitis"]

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
type Enfermedad = String

hierbaVerde :: Enfermedad -> Animal -> Animal
hierbaVerde enfermedad animal = modificarEnfermedades (filter (/= enfermedad)) animal

{-ghci> hierbaVerde "brucelosis" cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["sarampi\243n","tuberculosis"]}
-}

alcachofa :: Animal -> Animal
alcachofa animal = modificarPeso perderPeso animal

perderPeso :: Double  -> Double
perderPeso peso | peso > 2 = peso * 0.9
                | otherwise = peso * 0.95 

{-
ghci> alcachofa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

hierbaMagica :: Animal -> Animal
hierbaMagica animal = (modificarEdad (0*). modificarEnfermedades (\_ -> [])) animal

{-
ghci> hierbaMagica cerebro
Raton {nombre = "Cerebro", edad = 0.0, peso = 0.2, enfermedades = []}
-}
type Hierba = Animal -> Animal
medicamento :: [Hierba] -> Animal -> Animal
medicamento hierbas animal = foldl (\sem hierba -> hierba sem)  animal hierbas

medicamento' hierbas animal = foldr ($)  animal hierbas

medicamento'' hierbas animal = foldl (flip ($)) animal hierbas

{-
ghci> medicamento' [alcachofa, hierbaVerde "brucelosis"] cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.19, enfermedades = ["sarampi\243n","tuberculosis"]}
-}

{-
ghci> medicamento'' [alcachofa, hierbaBuena] cerebro
Raton {nombre = "Cerebro", edad = 3.0, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

antiage :: Hierba
antiage animal = medicamento (listaHierbas 3 hierbaBuena alcachofa) animal

listaHierbas :: Int -> Hierba -> Hierba -> [Hierba]
listaHierbas nro unaHierba otraHierba = replicate nro unaHierba ++ [otraHierba]
{-
ghci> antiage cerebro
Raton {nombre = "Cerebro", edad = 1.3160740129524924, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

reduceFatFast :: Int -> Hierba
reduceFatFast potencia animal = medicamento (listaHierbas potencia alcachofa (hierbaVerde "obesidad")) animal

{-
ghci> reduceFatFast 3 cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.171475, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}


hierbaMilagrosa :: Hierba
hierbaMilagrosa animal = medicamento (map hierbaVerde enfermedadesInfecciosas) animal

{-
ghci> hierbaMilagrosa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["sarampi\243n"]}
-}

cantidadIdeal f = (head.filter f) [1..]

estanMejoresQueNunca :: [Animal] -> Hierba -> Bool
estanMejoresQueNunca animales medicamento = all ((<1).peso.medicamento) animales

{-ghci> estanMejoresQueNunca [cerebro, orejudo] hierbaMilagrosa
False
-}


experimento ratones = cantidadIdeal (estanMejoresQueNunca ratones.reduceFatFast)

{-
ghci> experimento [cerebro, orejudo]
29
-}