-- (BORRAR COMENTARIO ANTES DE ENVIAR EL TP)
-- Comentario: Cambios que se realizaron en la revición 

-- ej 1 : 
-- -- "Proyectarnombres" : le agregue una funcion auxiliar que reune los nombres de todos los usuarios de la red, para que luego se pueda 
-- eliminar todos los nombres repetidos. Debido que, si bien el "requiere: RedSocialValida " nos aseguraba tener ids no repetidos, los usuarios podian
-- tener nombres iguales.

-- Contraejemplo:
-- proyectarNombres [(1, "Juan"), (2, "Natalia"), (3, "Juan"), (4, "Mariela"), (5, "Natalia")] = ["Juan","Natalia","Juan","Natalia"]

-- ej varios : 
-- Cambie la definicion de parametros por las dudas que no este permitido definir una variable, y volverla a definirla por dentro, ya que por algo
-- nos dejaron las "funciones basicas" al comienzo de "iap1-tp".

-- ej 4: no cambio nada
-- observación: no se contempla el caso donde la lista de usuarios es vacio porque en el "requiere" se nos asegura que no se puede ingresar [].
-- A tener en cuenta en el testeo.

-- ej 8 :
-- cambie la funcion "mismoElementos" para simplificarla

-- ej 9 :
-- Agregue la condicion donde el usuario que le daba like a todas las publicaciones no puede ser el mismo usuario que tiene un seguidor fiel, esto
-- por las especificaciones del problema. Además que la lista de publicaciones del usuario con un seguidor fiel debe ser mayor a cero, o 
-- se cae un falso verdadero en todos esos casos.

-- ej 10 :
-- Agregue la condicion donde en "existeSecuenciaDeAmigos" los dos usuarios ingresados no pueden ser iguales, porque sino
-- "existeSecuenciaDeAmigosDesdeCamino" lo tomaria como True, un falso verdadero, siendo que la secuencia no es mayor o igual a 2,
-- un requisito en la especificacion.

module Solucion where

-- Completar con los datos del grupo

-- Nombre de Grupo: xx
-- Integrante 1: Antonella Daiana Solis, forever.anti456@gmail.com , 558/22
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, [Usuario])
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us


-- Funciones  auxiliares generales:

-- Devuelve la cantidad de elementos de una lista.

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Comprueba si un elemento especifico pertenece a una lista.

pertenece :: (Eq t) => t -> [t]-> Bool
pertenece x [] = False
pertenece x (y:ys)| x == y = True 
                  | otherwise = pertenece x ys

-- Verifica que dos listan contengan los mismos elementos,sin importar su orden.

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] _  = True 
mismosElementos (x:xs) y | (longitud (x:xs) == longitud y) && (pertenece x y) == True = mismosElementos xs y
                         | otherwise = False

-- Devuelve la lista sin elementos repetidos

sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = sinRepetidos xs
                    | otherwise = x : sinRepetidos xs

------------------------------------------------------------------------------------


-- Ejercicio 1:

-- Dada una red social, extrae su lista de usuarios y devuelve una lista con todos los nombres de los usuarios sin repetición.

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

-- Dada una lista de usuarios, la función auxliar extrae el nombre de cada usuario de la lista, para posteriormente se pueda controlar que no existan repetidos.

proyectarNombres :: [Usuario] -> [String]
proyectarNombres u = sinRepetidos (auxiliar u)

auxiliar [] = []
auxiliar u = (nombreDeUsuario(head u)) : auxiliar (tail u)

-- Ejercicio 2:

-- Dada una red social y un usuario de la red, devuelve la lista de amigos de dicho usuario.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_,[],_) _ = []
amigosDe (us,(r:rs),ps) u1
    |(u1 == fst(r)) = (snd(r)) : (amigosDe (us,rs,ps) u1)
    |(u1 == snd(r)) = (fst(r)) : (amigosDe (us,rs,ps) u1)
    | otherwise = amigosDe (us,rs,ps) u1

-- Ejercicio 3:

-- Dada una red social y un usuario de la red, devuelve la cantidad de amigos del usuario en la red.

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

-- Ejercicio 4:

-- Dada una red social, devuelve el usuario de la red con más amigos en la red.

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], _, _) = u
usuarioConMasAmigos ((u:us), rs, ps)
    | cantidadDeAmigos red u >= cantidadDeAmigos red (usuarioConMasAmigos (us, rs, ps)) = u
    | otherwise = usuarioConMasAmigos (us, rs, ps)
	where
	red = ((u:us), rs, ps)

-- Ejercicio 5:

-- Dada una red social devuelve True si y sólo si existe algún usuario en la red con diez amigos o más.

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos ((u:us), rs, ps)
    | cantidadDeAmigos ((u:us), rs, ps) u >= 10 = True
    | otherwise = estaRobertoCarlos (us, rs, ps)
    where
    red = ((u:us), rs, ps)

-- Ejercicio 6:

-- Dada una red social y un usuario de la red, devuelve la lista de publicaciones de la red que pertenecen a dicho usuario.

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, (p:ps)) u
    | u == (usuarioDePublicacion p) = p : publicacionesDe (us, rs, ps) u
    | otherwise = publicacionesDe (us, rs, ps) u

-- Ejercicio 7:

-- Dada una red social y un usuario de la red, devuelve la lista de publicaciones a las que dicho usuario les dio "Me gusta".

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, (p:ps)) u
    | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) u
    | otherwise = publicacionesQueLeGustanA (us, rs, ps) u

-- Ejercicio 8:

-- Dada una red y dos usuarios de la red, devuelve True si y sólo si a ambos usuarios les gustan las mismas publicaciones.

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- Ejercicio 9:

-- Dada una red social y un usuario de la red, devuelve True si y sólo si hay algún usuario en la red que dio "Me gusta" a todas
-- las publicaciones del usuario pasado como parámetro de entrada.

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel ((u:us), rs, ps) u1
    | longitud(publicacionesDe red u1) == 0 = False 
    | (u /= u1) && (likeATodasLasPublicaciones u (publicacionesDe red u1)) = True
    | otherwise = tieneUnSeguidorFiel (us, rs, ps) u1
    where
    red = ((u:us), rs, ps)

-- Función auxiliar que dado un usuario y una lista de publicaciones, devuelve True si y sólo si dicho usuario
-- dio "Me gusta" a todas las publicaciones de la lista

likeATodasLasPublicaciones :: Usuario -> [Publicacion] -> Bool
likeATodasLasPublicaciones _ [] = True
likeATodasLasPublicaciones u (p:ps) = (pertenece u (likesDePublicacion p)) && (likeATodasLasPublicaciones u ps)

-- Ejercicio 10:

-- Dada una red social y dos usuarios de la red, devuelve True si y sólo si existe una secuencia ( llamado en la funcion como "camino") de relaciones entre ellos,
-- tal que empieze con una relacion del usuario1 (u1) y termine con otra del usuario2 (u2)

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | u1 /= u2  = existeSecuenciaDeAmigosDesdeCamino red u1 u2 []
                                  | otherwise = False

-- Funciones auxiliares que recorren recursivamente las listas de amigos de usuarios en busca de un camino.

recorrerAmigos :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
recorrerAmigos _ _ [] _ = False
recorrerAmigos red u1 (u:us) camino = (existeSecuenciaDeAmigosDesdeCamino red u1 u camino) || (recorrerAmigos red u1 us camino)

existeSecuenciaDeAmigosDesdeCamino :: RedSocial -> Usuario -> Usuario -> [Usuario] -> Bool
existeSecuenciaDeAmigosDesdeCamino red u1 u2 camino
    | u1 == u2 = True
    | pertenece u2 camino = False
    | otherwise = recorrerAmigos red u1 (amigosDe red u2) (u2:camino)