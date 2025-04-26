import Data.ByteString (find)
sayHello :: String -> String
sayHello alguien = " Hello " ++ alguien ++ "!"

numerosPares  :: [Integer] -> [Integer]
numerosPares numeros = [num | num <- numeros , even num]


divisiblePor :: Integer -> [Integer] -> [Integer]
divisiblePor n numeros = [num | num <-numeros , esDivisiblePor n num]

esDivisiblePor :: Integer -> (Integer -> Bool)
esDivisiblePor n m = ((==0).(`mod` n)) m


seleccionar :: (Integer -> Bool ) -> [Integer ] -> [Integer]
seleccionar criterio numeros = [num | num <- numeros , criterio num]

{-
ghci> filter (even.fst) [(2,6),(7,8), (8, 9), (4,3)]
[(2,6),(8,9),(4,3)]
-}


find' :: (a -> Bool) -> [a] -> a
find' condicion lista = ((head.filter condicion))  lista


data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int } deriving Show 

juan :: Politico
juan = Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81

politicos :: [Politico]
politicos = [ juan, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]
 

 {--
 ghci> find' ((<50).edad)  politicos 
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}

{--
ghci> find' ((>3).length.proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}

--}

{--
ghci> find' (any((>3).length.words).proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}
--}


type Nombre = String
type Notas = [Int]

data Persona = Alumno {nombre :: Nombre, notas :: Notas}

promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos alumnos = map (\unAlumno -> (nombre unAlumno, (promedio.notas)unAlumno)) alumnos 

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)

{--
ghci> promediosAlumnos [(Alumno "juan" [5,7,6,9]), (Alumno "julia" [6,7,3,8])]
[("juan",6),("julia",6)]
--}