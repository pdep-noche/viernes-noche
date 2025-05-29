data Postulante = UnPostulante {nombre :: String, edad :: Double, remuneracion :: Double, conocimientos :: [String]} deriving Show 
 
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto González" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

type Requisito = Postulante -> Bool

---1 a
tieneConocimientos :: Puesto -> Requisito
tieneConocimientos unPuesto unPostulante = (all (\requerido -> elem requerido (conocimientos unPostulante)).conocimientoRequeridos) unPuesto

--1 b
type Edad = Double
edadAceptable :: Edad -> Edad -> Requisito
edadAceptable edadMin edadMax postulante = edad postulante >= edadMin && edad postulante <= edadMax

sinArreglo :: Requisito
sinArreglo postulante = (apellidoDueno /=).last.words.nombre $ postulante

------2

preseleccion :: [Postulante] -> [Requisito] -> [Postulante]
preseleccion postulantes requisitos = filter (cumpleTodosReq requisitos)  postulantes

cumpleTodosReq :: [Requisito] -> Postulante -> Bool
cumpleTodosReq requisitos postulante = all ($ postulante)  requisitos


{-
ghci> preseleccion [tito, pepe] [(edadAceptable 30 40), tieneConocimientos jefe, sinArreglo]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}


{-
ghci> preseleccion [tito, pepe] [(edadAceptable 30 40), tieneConocimientos jefe, sinArreglo, (not.(elem "repetir logica").conocimientos)]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}

incrementarEdad :: Postulante -> Postulante
incrementarEdad postulante = postulante { edad = edad postulante + 1}

aumentarSueldo :: Double -> Postulante -> Postulante
aumentarSueldo porcentaje postulante = postulante {
    remuneracion = sueldoActualizado porcentaje postulante
}

sueldoActualizado porcentaje postulante = remuneracion postulante + ((remuneracion postulante)* porcentaje)

-----Aplicanto composición, aplicación parcial y orden superior
actualizarPostulantes :: [Postulante]-> [Postulante]
actualizarPostulantes postulantes = map (aumentarSueldo 27.incrementarEdad) postulantes

---Listas por comprensión
actualizarPostulantes' :: [Postulante]-> [Postulante]
actualizarPostulantes' postulantes = [(aumentarSueldo 27.incrementarEdad) postulante | 
                postulante <- postulantes]

{-
ghci> actualizarPostulantes' [tito, pepe]
[UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21.0, remuneracion = 336000.0, conocimientos = ["Haskell","Php"]},UnPostulante {nombre = "Jose Perez", edad = 36.0, remuneracion = 420000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}