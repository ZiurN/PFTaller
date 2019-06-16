import Definitions

usuariosPrueba = [(001, "Jeferson"),(002, "Juan"),(003, "Karen"),(004, "Camila"),(006, "Jhonatan")]
publicacionesPrueba = [((001,"Jeferson"), "Publicacion de prueba de Jeferson",[]), ((004, "Camila"), "SOy Camila", [(006, "Jhonatan")]), ((003, "Karen"), "Me llamo Karen", [(002, "Juan")]), ((006, "Jhonatan"), "Otra prueba Jhonatan",[(001,"Jeferson")])] 
relacionesPrueba = [ ((002, "Juan"),(003, "Karen")) , ((004, "Camila"),(006, "Jhonatan")) , ((001, "Jeferson"),(004, "Camila")) ]

redSocialPrueba :: Set Usuario -> Set Relacion -> Set Publicacion -> RedSocial
redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba = (usuariosPrueba, relacionesPrueba, publicacionesPrueba)

redTest = (redSocialPrueba usuariosPrueba relacionesPrueba publicacionesPrueba)

--redSocialPrueba = ( [(001, "Jeferson"),(002, "Juan"),(003, "Karen"),(004, "Camila"),(006, "Jhonatan")],  [((001,"Jeferson"),(003, "Karen")),((002, "Juan"),(003, "Karen")),((004, "Camila"),(006, "Jhonatan")),((001, "Jeferson"),(006, "Jhonatan"))],  [((001,"Jeferson"), "Publicacion de prueba de Jeferson", [(004, "Camila"),(006, "Jhonatan")]),((003, "Karen"), "Me llamo Karen", [(002, "Juan")]),((006, "Jhonatan"), "Otra prueba Jhonatan",[])] )

-- FUnciones auxiliares más generales

usuariosDeLaRed :: RedSocial -> Set Usuario
usuariosDeLaRed (users, _, _) = users

relacionesDeLaRed :: RedSocial -> Set Relacion
relacionesDeLaRed (_, relaciones, _) = relaciones

publicacionesDeLaRed :: RedSocial -> Set Publicacion
publicacionesDeLaRed (_, _, publicaciones) = publicaciones

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios red = obtenerNombresDeUsuarios (usuarios red)

obtenerNombresDeUsuarios :: Set Usuario -> Set String
obtenerNombresDeUsuarios [] = []
obtenerNombresDeUsuarios ( user:users ) = ( snd user ) : obtenerNombresDeUsuarios users

-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe red usuario = amigosDeAux listaRelaciones listaUsuarios usuario
                     where listaUsuarios = usuariosDeLaRed red
                           listaRelaciones = relacionesDeLaRed red
                                 
amigosDeAux :: Set Relacion -> Set Usuario -> Usuario -> Set Usuario
amigosDeAux [] _ _  = []
amigosDeAux _ [] _ = [] 
amigosDeAux relaciones usuarios usuario | (elem (usuario, head usuarios) relaciones) || (elem (head usuarios, usuario) relaciones) = (head usuarios) : (amigosDeAux relaciones (tail usuarios) usuario)
                                        | otherwise = (amigosDeAux relaciones (tail usuarios) usuario)

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = length (amigosDe red usuario)

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> (Usuario, Int)
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuariosDeLaRed red) (relacionesDeLaRed red) (000, "Sin Amigos")

usuarioConMasAmigosAux :: RedSocial -> Set Usuario -> Set Relacion -> Usuario -> (Usuario, Int)
usuarioConMasAmigosAux red usuarios relaciones usuario | usuarios == [] = (usuario, valorActual)
                                                       | valorActual > valorLista = usuarioConMasAmigosAux red (tail usuarios) relaciones usuario
                                                       | otherwise = usuarioConMasAmigosAux red (tail usuarios) relaciones (head usuarios)
                                                         where valorActual = cantidadDeAmigos red usuario
                                                               valorLista = cantidadDeAmigos red (head usuarios)

-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | snd (usuarioConMasAmigos red) > 1000000 = True
                      | otherwise = False

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe red usuario = publicacionesDeAux (publicacionesDeLaRed red) usuario

publicacionesDeAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesDeAux [] _ = []
publicacionesDeAux (pb: pbs) usuario | usuario == autor =  pb : (publicacionesDeAux pbs usuario)
                                     | otherwise = publicacionesDeAux pbs usuario
                                       where autor = autorDePublicacion pb

autorDePublicacion :: Publicacion -> Usuario
autorDePublicacion (usuario, _, _) = usuario

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA red usuario = publicacionesQueLeGustanAAux (publicacionesDeLaRed red) usuario


publicacionesQueLeGustanAAux :: Set Publicacion -> Usuario -> Set Publicacion
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (pb:pbs) usuario | (elem usuario likes) = pb : publicacionesQueLeGustanAAux pbs usuario
                                              | otherwise = publicacionesQueLeGustanAAux pbs usuario
                                                where likes = likesDeLaPublicacion pb
                                                  
likesDeLaPublicacion :: Publicacion -> Set Usuario
likesDeLaPublicacion (_, _, lista) = lista

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined