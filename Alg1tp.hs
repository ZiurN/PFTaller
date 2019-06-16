module Alg1tp
where

-------------------------------------------------------------------------------------------------------------

-- DEFINICIONES
type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

-- FUNCIONES BASES
usuarios :: RedSocial -> Set Usuario
usuarios (us, _, _) = us

relaciones :: RedSocial -> Set Relacion
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> Set Publicacion
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> Set Usuario
likesDePublicacion (_, _, us) = us

-- FUNCIONES AUXILIARES GENERALES

-- Eliminar posibles repeticiones en las listas, de esa forma respetar que es conjunto.
eliminaRepetidos :: (Eq a) => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (n:xs) | elem n xs = eliminaRepetidos(xs)
 | otherwise = n : (eliminaRepetidos(xs))


----------------------------------------------------------------------------------------------------------------------------
-- PRUEBAS PARA CHECAR

-- Para comodidad del usuario, al emplear las funciones utilize "redTest" para probarlas
redTest = (redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba)


-- RED TEST se compone de los siguientes ejemplos:

-- Usuarios que integran la red, con su id y avatar correspondientes:
usuariosPrueba = [(001, "Jeferson"),(002, "Juan"),(003, "Karen"),(004, "Camila"),(006, "Jhonatan"), (007, "James"), (001, "Jeferson")]
-- Notar, que el primer y ultimo elemento de la lista son iguales, esto fue así para poner a prueba la eliminación de posibles elementos repetidos.

-- Publicaciones varias que realizaron los usuarios.
-- Son tuplas compuestas por (el usuario autor; su publicación; quiénes le dieron like).
publicacionesPrueba = [((001,"Jeferson"), "Jef hizo su primer publicacion",[]), ((004, "Camila"), "Cami hizo su primer publicacion", [(006, "Jhonatan"), (001, "Jeferson")]), ((003, "Karen"), "Karu hizo su primer publicación", [(002, "Juan")]), ((001, "Jeferson"), "Jef hizo su segunda publicacion",[(004, "Camila"), (006, "Jhonatan"), (002, "Juan"), (001, "Jeferson")])] 

-- Relaciones de amistad existentes entre los usuarios.
relacionesPrueba = [ ((002, "Juan"),(003, "Karen")) , ((004, "Camila"),(006, "Jhonatan")) , ((006, "Jhonatan"),(004, "Camila")) , ((003, "Karen"), (001, "Jeferson"))]

-- Finalmente, se compilan las pruebas para dar forma a la red social.
redSocialPrueba :: Set Usuario -> Set Relacion -> Set Publicacion -> RedSocial
redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba = (usuariosPrueba, relacionesPrueba, publicacionesPrueba)


----------------------------------------------------------------------------------------------------------------------------------------


-- PULIDO de definiciones base
usuariosDeLaRed :: RedSocial -> Set Usuario
usuariosDeLaRed (us, rs, ps) = eliminaRepetidos(usuarios(us, rs, ps))

relacionesDeLaRed :: RedSocial -> Set Relacion
relacionesDeLaRed (us, rs, ps) = eliminaRepetidos(relaciones (us, rs, ps))

publicacionesDeLaRed :: RedSocial -> Set Publicacion
publicacionesDeLaRed (us, rs, ps) = eliminaRepetidos(publicaciones (us, rs, ps))

--------------------------------------------------------------------------------------------------------

-- PUNTO 1: nombresDeUsuarios
-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios red = eliminaRepetidos(obtenerNombresDeUsuarios (usuarios red))

-- Función auxiliar para nombresDeUsuarios.
-- De la tupla "usuario" obtiene la segunda componente String del usuario
obtenerNombresDeUsuarios :: Set Usuario -> Set String
obtenerNombresDeUsuarios [] = []
obtenerNombresDeUsuarios ( user:users ) = ( snd user ) : obtenerNombresDeUsuarios users

-- PUNTO 2: amigosDe
-- Dada una red social y un usuario retorna el conjunto de amigos del mismo.
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe red usuario = amigosDeAux listaRelaciones listaUsuarios usuario
                     where listaUsuarios = usuariosDeLaRed red
                           listaRelaciones = relacionesDeLaRed red
 
-- Función auxiliar para amigosDe.
-- Toma un usuario y la lista de usuarios que integran la red. Luego, crea una tupla entre ese usuario y el que de momento encabeza la lista de usuarios.
-- Compara esa tupla con el conjunto de relaciones. Si machean los agrega a la lista de amigos del usuario.
-- Si no machean, revisa el siguiente que encabeza la cola de la lista de usuarios de la red.
amigosDeAux :: Set Relacion -> Set Usuario -> Usuario -> Set Usuario
amigosDeAux [] _ _  = []
amigosDeAux _ [] _ = [] 
amigosDeAux relaciones usuarios usuario | (elem (usuario, head usuarios) relaciones) || (elem (head usuarios, usuario) relaciones) = (head usuarios) : (amigosDeAux relaciones (tail usuarios) usuario)
                                        | otherwise = (amigosDeAux relaciones (tail usuarios) usuario)

-- PUNTO 3: cantidadDeAmigos										
-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = length (amigosDe red usuario)

-- PUNTO 4: usuarioConMasAmigos
-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = fst (usuarioConMasAmigosAux red (usuariosDeLaRed red) (relacionesDeLaRed red) (000, "Sin Amigos"))

-- Función auxiliar para usuarioConMasAmigos.
-- Cuenta las relaciones que tiene un usuario, y las compara con la cantidad que sumen cada uno de los demás usuarios.
-- Y al final se queda con el usuario que tenga más.
usuarioConMasAmigosAux :: RedSocial -> Set Usuario -> Set Relacion -> Usuario -> (Usuario, Int)
usuarioConMasAmigosAux red usuarios relaciones usuario | usuarios == [] = (usuario, valorActual)
                                                       | valorActual > valorLista = usuarioConMasAmigosAux red (tail usuarios) relaciones usuario
                                                       | otherwise = usuarioConMasAmigosAux red (tail usuarios) relaciones (head usuarios)
                                                         where valorActual = cantidadDeAmigos red usuario
                                                               valorLista = cantidadDeAmigos red (head usuarios)
-- PUNTO 5: estaRobertoCarlos
-- Dada una red social retorna True SI ALGÚN usuario tiene más de un millón de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | snd (usuarioConMasAmigos red) > 1000000 = True
                      | otherwise = False

-- PUNTO 6: publicacionesDe
-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe red usuario = publicacionesDeAux (publicacionesDeLaRed red) usuario

-- -- Toma un usuario y la lista de publicaciones que hay en la red. Luego, toma el autor de la primer publicacion de la lista
-- Compara si este autor es el mismo que el usuario entregado.
-- Si es cierto, agrega la publicación a la lista, sino, continua con la recursión.
publicacionesDeAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesDeAux [] _ = []
publicacionesDeAux (pb: pbs) usuario | usuario == autor =  pb : (publicacionesDeAux pbs usuario)
                                     | otherwise = publicacionesDeAux pbs usuario
                                       where autor = autorDePublicacion pb
-- Mejor que sobre a que falte ahrre <3 :)
autorDePublicacion :: Publicacion -> Usuario
autorDePublicacion (usuario, _, _ ) = usuario
 
-- PUNTO 7: publicacionesQueLeGustanA
-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA red usuario = publicacionesQueLeGustanAAux (publicacionesDeLaRed red) usuario

-- Se le da la lista de publicaciones y un usuario. Mira la primera publicación de la lista, si el usuario figura,
-- lo devuelve, sino, continua con la recursión.
publicacionesQueLeGustanAAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (pb:pbs) usuario | (elem usuario likes) == True = pb : publicacionesQueLeGustanAAux pbs usuario
                                              | otherwise = publicacionesQueLeGustanAAux pbs usuario
                                                where likes = likesDePublicacion pb

-- PUNTO 8: lesGustanLasMismasPublicaciones
-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- PUNTO 9: tieneUnSeguidorFiel
-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined


-- OPCIONAL --

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined