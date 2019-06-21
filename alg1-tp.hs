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

------------------------------------------------------------------------------------------------------------------------------

-- FUNCIONES AUXILIARES GENERALES

-- Eliminar posibles repeticiones en las listas, de esa forma respetar que es conjunto.
eliminaRepetidos :: (Eq a) => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (n:xs) | elem n xs = eliminaRepetidos(xs)
 | otherwise = n : (eliminaRepetidos(xs))

-- Dado una red y un ID, devuelve el usuario correspondiente al ID
getUser :: RedSocial -> Integer -> Usuario
getUser (u:us,_,_) id 
    | fst u == id = u
    | otherwise = getUser (us,[],[]) id

-- Dada una red social y dos usuarios, indica si los usuarios son amigos. Útil para verificar rápido.
sonAmigos :: RedSocial -> Usuario -> Usuario -> Bool
sonAmigos (_,rs,_) u1 u2 = elem (u1,u2) rs || elem (u2,u1) rs

----------------------------------------------------------------------------------------------------------------------------

-- PULIDO de definiciones base. Con el fin de asegurar que trabajamos con conjuntos, sin alterar las
-- definiciones dadas por defecto arriba.

usuariosDeLaRed :: RedSocial -> Set Usuario
usuariosDeLaRed (us, rs, ps) = eliminaRepetidos(usuarios(us, rs, ps))

relacionesDeLaRed :: RedSocial -> Set Relacion
relacionesDeLaRed (us, rs, ps) = eliminaRepetidos(relaciones (us, rs, ps))

publicacionesDeLaRed :: RedSocial -> Set Publicacion
publicacionesDeLaRed (us, rs, ps) = eliminaRepetidos(publicaciones (us, rs, ps))

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------EJERCICIOS OBLIGATORIOS-------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 1: nombresDeUsuarios
-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.

nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios red =obtenerNombresDeUsuarios (usuariosDeLaRed red)

-- Función auxiliar para nombresDeUsuarios.
-- De la tupla "usuario" obtiene la segunda componente String del usuario

obtenerNombresDeUsuarios :: Set Usuario -> Set String
obtenerNombresDeUsuarios [] = []
obtenerNombresDeUsuarios (user:users) = (snd user) : obtenerNombresDeUsuarios users

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 2: amigosDe
-- Dada una red social y un usuario retorna el conjunto de amigos del mismo.

amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe red user = amigosDeAux listaRelaciones listaUsuarios user
                     where listaUsuarios = usuariosDeLaRed red
                           listaRelaciones = relacionesDeLaRed red
 
-- Función auxiliar para amigosDe.
-- Toma un usuario y la lista de usuarios que integran la red. Luego, crea una tupla entre ese usuario
-- y el que de momento encabeza la lista de usuarios.
-- Compara esa tupla con el conjunto de relaciones. Si cumple lo pedido los agrega a la lista de amigos 
-- del usuario. Si no, revisa el siguiente que encabeza la cola de la lista de usuarios de la red.

amigosDeAux :: Set Relacion -> Set Usuario -> Usuario -> Set Usuario
amigosDeAux [] _ _  = []
amigosDeAux _ [] _ = [] 
amigosDeAux relations users user | (elem (user, head users) relations) || (elem (head users, user) relations) = (head users) : (amigosDeAux relations (tail users) user)
                                        | otherwise = (amigosDeAux relations (tail users) user)

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 3: cantidadDeAmigos										
-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario.

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red user = length (amigosDe red user)

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 4: usuarioConMasAmigos
-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = fst (usuarioConMasAmigosAux red (usuariosDeLaRed red) (relacionesDeLaRed red) (000, "Sin Amigos"))

-- Función auxiliar para usuarioConMasAmigos.
-- Cuenta las relaciones que tiene un usuario, y las compara con la cantidad que sumen cada uno de los demás usuarios.
-- Y al final se queda con el usuario que tenga más.

usuarioConMasAmigosAux :: RedSocial -> Set Usuario -> Set Relacion -> Usuario -> (Usuario, Int)
usuarioConMasAmigosAux red users relations user | users == [] = (user, valorActual)
                                                | valorActual > valorLista = usuarioConMasAmigosAux red (tail users) relations user
                                                | otherwise = usuarioConMasAmigosAux red (tail users) relations (head users)
                                                  where valorActual = cantidadDeAmigos red user
                                                        valorLista = cantidadDeAmigos red (head users)

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 5: estaRobertoCarlos
-- Dada una red social retorna True SI ALGÚN usuario tiene más de un millón de amigos.

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | fst (usuarioConMasAmigos red) >= 1000000 = True
                      | otherwise = False

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 6: publicacionesDe
-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.

publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe red user = publicacionesDeAux (publicacionesDeLaRed red) user

-- Función auxiliar para publicacionesDe.
-- Toma un usuario y la lista de publicaciones que hay en la red.
-- Luego, toma el autor de la primer publicacion de la lista
-- Compara si este autor es el mismo que el usuario entregado.
-- Si es cierto, agrega la publicación a la lista, sino, continua con la recursión.

publicacionesDeAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesDeAux [] _ = []
publicacionesDeAux (pb: pbs) user | user == autor =  pb : (publicacionesDeAux pbs user)
                                     | otherwise = publicacionesDeAux pbs user
                                       where autor = autorDePublicacion pb
-- Para dejar claro que no se trata de cualquier usuario, sino del que escribió concretamente la(s) publicación(es).
autorDePublicacion :: Publicacion -> Usuario
autorDePublicacion (user, _, _ ) = user

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 7: publicacionesQueLeGustanA
-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.

publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA red user = publicacionesQueLeGustanAAux (publicacionesDeLaRed red) user

-- Función auxiliar para publicacionesQueLeGustanA.
-- Se le da la lista de publicaciones y un usuario. Mira la primera publicación de la lista, si el usuario figura,
-- lo devuelve, sino, continua con la recursión.

publicacionesQueLeGustanAAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (pb:pbs) user | (elem user likes) == True = pb : publicacionesQueLeGustanAAux pbs user
                                              | otherwise = publicacionesQueLeGustanAAux pbs user
                                                where likes = likesDePublicacion pb

------------------------------------------------------------------------------------------------------------------------------												

-- PUNTO 8: lesGustanLasMismasPublicaciones
-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones.

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = (publicacionesQueLeGustanA red user1) == (publicacionesQueLeGustanA red user2)

------------------------------------------------------------------------------------------------------------------------------

-- PUNTO 9: tieneUnSeguidorFiel
-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
-- Observaciones:
-- Un usuario no puede ser seguidor fiel de si mismo
-- Un usuario no puede tener seguidores fieles si no hizo ninguna publicacion

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],_,_) _ = False
tieneUnSeguidorFiel (ufiel:us,rs,ps) user
    | publicacionesDe ([],[],ps) user == [] = False
    | ufiel == user = tieneUnSeguidorFiel (us,rs,ps) user
    | otherwise = publicacionesDe ([],[],ps) user == publicacionesDe ([],[],publicacionesQueLeGustanA ([],[],ps) ufiel) user || tieneUnSeguidorFiel (us,rs,ps) user

------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------EJERCICIOS OPCIONALES-------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

-- Opcional: existeSecuenciaDeAmigos
-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us,rs,_) u1 u2 
    | sonAmigos red u1 u2 == True = True
    | cantidadDeAmigos red u1 == 0 || cantidadDeAmigos red u2 == 0 = False
    | otherwise = existeSecuenciaDeAmigos red2 u1 u2 || existeSecuenciaDeAmigos red2 amigoDeU1 u2
    where red  = (us,rs,[])
          amigoDeU1 = head (amigosDe red u1)
          red2 = (us,eliminaRelacion rs u1 amigoDeU1,[])

------------------------------------------------------------------------------------------------------------------------------

-- Opcional (?): eliminaRelacion
-- Dado un conjunto de relaciones y dos usuarios u1 y u2, elimina la relacion de amistad entre u1 y u2 si esta existiera.
-- Esta funcion la voy a usar para poder hacer recursion sobre todas las amistades de un usuario en 
-- la funcion existeSecuenciaDeAmigos

eliminaRelacion :: Set Relacion -> Usuario -> Usuario -> Set Relacion
eliminaRelacion [] _ _ = []
eliminaRelacion (r:rs) u1 u2
    | r == (u1,u2) || r == (u2,u1) = eliminaRelacion rs u1 u2
    | otherwise = r:(eliminaRelacion rs u1 u2)

------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------TEST------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

-- Para comodidad del usuario, al emplear las funciones utilize "redTest" para probarlas
redTest = (redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba)

-- RED TEST se compone de los siguientes ejemplos:

-- Usuarios que integran la red, con su id y avatar correspondientes:
usuariosPrueba = [(1, "Jeferson"),(2, "Juan"),(3, "Karen"),(4, "Camila"),(5, "Micaela"), (6, "Jhonatan"), (7, "James"), (1, "Jeferson")]
-- Notar, que el primer y ultimo elemento de la lista son iguales,
-- esto fue así para poner a prueba la eliminación de posibles elementos repetidos.

-- Publicaciones varias que realizaron los usuarios.
-- Son tuplas compuestas por (el usuario autor; su publicación; quiénes le dieron like).
publicacionesPrueba = [((1,"Jeferson"), "Me gusta el verano",[]), ((4, "Camila"), "Hola, soy Cami :)", [(6, "Jhonatan"), (1, "Jeferson")]), ((3, "Karen"), "Saliendo a pasear con el perro", [(2, "Juan")]), ((1, "Jeferson"), "Hoy es un lindo dia",[(4, "Camila"), (6, "Jhonatan"), (2, "Juan"), (1, "Jeferson")]), ((1,"Jeferson"), "Me gusta el verano",[]), ((7, "James"), "Mi apellido es Bond", [(7, "James")])] 
-- Notar, que hay dos elementos de la lista son iguales,
-- esto fue así para poner a prueba la eliminación de posibles repeticiones.

-- Relaciones de amistad existentes entre los usuarios.
relacionesPrueba = [ ((2, "Juan"),(3, "Karen")) , ((4, "Camila"),(6, "Jhonatan")) , ((6, "Jhonatan"),(4, "Camila")) , ((3, "Karen"), (1, "Jeferson")), ((1, "Jeferson"), (3, "Karen"))]

-- Finalmente, se compilan las pruebas para dar forma a la red social.
redSocialPrueba :: Set Usuario -> Set Relacion -> Set Publicacion -> RedSocial
redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba = (usuariosPrueba, relacionesPrueba, publicacionesPrueba)

-----------------------------------------------------------------------------------------------------------------------------

-- Punto 1
-- nombreDeUsuario redTest ~~> ["Juan","Karen","Camila","Jhonatan","James","Jeferson"]

-- Punto 2
-- amigosDe redTest (3, "Karen")   ~~>  [(2,"Juan"),(1,"Jeferson")]
-- amigosDe redTest (5, "Micaela") ~~>  []
-- amigosDe redTest (4, "Camila")  ~~>  [(6,"Jhonatan")]

-- Punto 3
-- cantidadDeAmigos redTest (6, "Jhonatan") ~~> 1
-- cantidadDeAmigos redTest (3, "Karen")    ~~> 2

-- Punto 4
-- usuarioConMasAmigos redTest ~~> (3,"Karen")

-- Punto 5
-- estaRobertoCarlos redTest ~~> False

-- Punto 6
-- publicacionesDe redTest (1, "Jeferson") ~~> [((1,"Jeferson"),"Hoy es un lindo dia",[(4,"Camila"),(6,"Jhonatan"),(2,"Juan"),(1,"Jeferson")]),((1,"Jeferson"),"Me gusta el verano",[])]
-- publicacionesDe redTest (5, "Micaela")  ~~> []
-- publicacionesDe redTest (4, "Camila")   ~~> [((4,"Camila"),"Hola, soy Cami :)",[(6,"Jhonatan"),(1,"Jeferson")])]

-- Punto 7
-- publicacionesQueLeGustanA redTest (1, "Jeferson") ~~> [((4,"Camila"),"Hola, soy Cami :)",[(6,"Jhonatan"),(1,"Jeferson")]),((1,"Jeferson"),"Hoy es un lindo dia",[(4,"Camila"),(6,"Jhonatan"),(2,"Juan"),(1,"Jeferson")])]
-- publicacionesQueLeGustanA redTest (7, "James")    ~~> [((7,"James"),"Mi apellido es Bond",[(7,"James")])]
-- publicacionesQueLeGustanA redTest (5, "Micaela")  ~~> []

-- Punto 8
-- lesGustanLasMismasPublicaciones redTest (1, "Jeferson") (6, "Jhonatan") ~~>  True
-- lesGustanLasMismasPublicaciones redTest (1, "Jeferson") (4, "Camila")   ~~>  False

-- Punto 9
-- tieneUnSeguidorFiel redTest (4, "Camila")    ~~>  True
-- tieneUnSeguidorFiel redTest (1, "Jeferson")  ~~>  False
-- tieneUnSeguidorFiel redTest (7, "James")     ~~>  False

-- Opcional
-- existeSecuenciaDeAmigos redTest (1, "Jeferson") (6, "Jhonatan") ~~>  False
-- existeSecuenciaDeAmigos redTest (1, "Jeferson") (4, "Camila")   ~~>  False
-- existeSecuenciaDeAmigos redTest (1, "Jeferson") (2, "Juan")     ~~>  True