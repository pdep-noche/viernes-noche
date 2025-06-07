import Text.Show.Functions

data Pelicula = Pelicula {nombreP:: String, genero:: String, duracion:: Int, paisOrigen:: String } deriving (Show,Eq)


psicosis :: Pelicula
psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"

data Usuario = Usuario {nombre:: String, categoria:: String, edad:: Int, pais:: String, peliculas :: [Pelicula], estadoSalud:: Int} deriving Show
juan :: Usuario
juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer, elSaborDeLasCervezas] 60

----2---
ver:: Pelicula -> Usuario -> Usuario
ver pelicula usuario = usuario {peliculas = peliculas usuario ++ [pelicula]}

{-
ghci> ver psicosis juan
Usuario {nombre = "juan", categoria = "estandar", edad = 23, pais = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, paisOrigen = "Estados Unidos"},Pelicula {nombreP = "Psicosis", genero = "Terror", duracion = 109, paisOrigen = "Estados Unidos"}], estadoSalud = 60}
-}

--- 3 --
premiar :: [Usuario] -> [Usuario]
premiar usuarios = map premiarUsuarioInterFiel usuarios

premiarUsuarioInterFiel :: Usuario -> Usuario
premiarUsuarioInterFiel usuario | cumpleCondiciones usuario = subirCategoria usuario
                                | otherwise = usuario

cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (>=20).length.peliculasQueNoSeanDe "Estados Unidos".peliculas $ usuario


peliculasQueNoSeanDe :: String -> [Pelicula] -> [Pelicula]
peliculasQueNoSeanDe pais peliculas = filter ((pais /=).paisOrigen) peliculas


subirCategoria:: Usuario -> Usuario
subirCategoria usuario = usuario{categoria = nuevaCategoria.categoria $ usuario}

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria _  = "premium"

---------------4------------
type Criterio = Pelicula -> Bool

teQuedasteCorto :: Criterio
teQuedasteCorto pelicula = ((<35).duracion) pelicula

cuestionDeGenero:: [String] -> Criterio
cuestionDeGenero generos pelicula = any (==(genero pelicula)) generos


{-
ghci> cuestionDeGenero ["Terror","Drama"] elSaborDeLasCervezas
True
-}

deDondeSaliste :: String -> Criterio
deDondeSaliste unOrigen pelicula = (== unOrigen).paisOrigen $ pelicula

vaPorEseLado :: (Eq a) => Pelicula -> (Pelicula -> a) -> Criterio
vaPorEseLado pelicula caracteristica otraPelicula = caracteristica pelicula == caracteristica otraPelicula 

---5----
buscarPeliculas :: Usuario -> [Criterio] -> [Pelicula] -> [Pelicula]
buscarPeliculas usuario criterios peliculas = take 3.filter (esRecomendable usuario criterios)  $ peliculas


esRecomendable :: Usuario -> [Criterio] -> Pelicula -> Bool
esRecomendable usuario criterios pelicula = (not.vio pelicula) usuario && cumpleTodosLosCrite pelicula criterios

vio :: Pelicula -> Usuario -> Bool
vio pelicula usuario = elem pelicula . peliculas $ usuario

cumpleTodosLosCrite ::Pelicula -> [Criterio] -> Bool
cumpleTodosLosCrite pelicula criterios = all ($ pelicula) criterios


{-

buscarPeliculas juan [deDondeSaliste "iran", cuestionDeGenero ["Drama", "Comedia"], (not.teQuedasteCorto)]  [psicosis]
-} 

data Capitulo = Capitulo {nombreC :: String, generoC :: String, duracionC :: Int, origen :: String, afecta:: Usuario -> Usuario } deriving Show

capitulo :: Capitulo
capitulo = Capitulo "Scream" "Terror" 40 "Estados Unidos"  (\usuario -> usuario {estadoSalud = (estadoSalud usuario) - 10})

consumirSerie :: Usuario -> Capitulo -> Usuario
consumirSerie usuario capitulo = (afecta capitulo) usuario


{--
ghci> capitulo
Capitulo {nombreC = "Scream", generoC = "Terror", duracionC = 40, origen = "Estados Unidos", afecta = <function>}

ghci> consumirSerie juan capitulo
Usuario {nombre = "juan", categoria = "estandar", edad = 23, pais = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, paisOrigen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, paisOrigen = "Iran"}], estadoSalud = 50}
--}

type Serie = [Capitulo]

maraton :: Usuario -> Serie -> Usuario
maraton usuario serie = foldl consumirSerie usuario serie


{--
ghci> maraton juan [capitulo]
Usuario {nombre = "juan", categoria = "estandar", edad = 23, pais = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, paisOrigen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, paisOrigen = "Iran"}], estadoSalud = 50}
--}


serieInfinita :: Serie
serieInfinita = repeat capitulo


{--
ghci> maraton juan (take 4 serieInfinita)
Usuario {nombre = "juan", categoria = "estandar", edad = 23, pais = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, paisOrigen = "Estados Unidos"},Pelicula {nombreP = "El sabor de las cervezas", genero = "Drama", duracion = 95, paisOrigen = "Iran"}], estadoSalud = 20}
--}