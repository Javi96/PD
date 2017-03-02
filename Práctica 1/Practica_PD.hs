-- Alumno: José Javier Cortés Tejada
-- Programación declarativa 2016/2017

-- En cuanto a la realización de la práctica he usado como material de apoyo la página http://aprendehaskell.es/main.html,
-- más que nada el tema de usar una pareja para la lista de palabras y las operaciones que se realizan sobre ella, eso lo explican
-- concretamente en al apartado 14, en el punto de 'breadcrums'.

{-
    Dado el tipo de operaciones que son propuestas por la práctica, encontrar una solución coherente en base a unas palabras dadas no es trivial pues depende de como se haya implementado la secuencia de operaciones, por ejemplo si la frase de partida es 'hola como estas' y la palabra a buscar es 'comoestas' puede que nunca se encuentre a pesar de ser aparantemente inmediata, pues el que se encuentre esta solución depende completamente de como sea la implementación, si primero se concatena y se desplaza o de si es al revés, y sobre que se apliacan además. Por ello, dejo por aquí un ejemplo donde si se encuentra una solución adecuada, para ver que el programa funciona correctamente:

        frase: hola como estas
        palabra: qmeohgs

    Lo que he hecho para sacar un ejemplo válido es cambiar la función solución, la tercera guarda debe añadir al mensaje que indica que no hay coincidencias opList, para ello se concatena a ese mensaje, lanzamos la práctica con una frase (hola como estas) en este caso y luego miramos en la secuencia de operaciones una de las palabras y ejecutamos de nuevo con la misma frase y con la palabra elegida (qmeohgs en este caso).
-}
{-
    Elimina los elementos repetidos dentro de la lista x:xs, para ello usamos la función any, donde en caso de que x se igual a algún elemento de xs, lanzamos la llamada recursiva y lo eliminamos, en otro caso la mantenemos en la lista.
-}
remove_rep           :: Eq a => [a] -> [a]
remove_rep []        =  []
remove_rep (x:xs)
    | any ((==) x) xs = remove_rep xs
    | otherwise = x : remove_rep xs

{-
    Dadas dos listas de palabras xs e ys, miramos si alguna de las palabras de xs está en ys, en ese caso la añadimos a la cadena de retorno y la destacamos en otro caso. LLamamos siempre con la función remove_rep para eliminar replicados.
-}
intersect          :: Eq a => [a] -> [a] -> [a]
intersect [] _     =  []
intersect _  []    =  []
intersect xs ys    =  remove_rep [x | x <- xs, any ((==) x) ys]

{-
    Dadas dos listas xs e ys, nos quedamos con cada palabra de la lista xs que no esté en la lista ys, es decir, aplicamos la misma condición que al intersecar pero con condición invertida; si no está incluímos en la lista de retorno.
-}
sub          :: Eq a => [a] -> [a] -> [a]
sub [] _     =  []
sub _  []    =  []
sub xs ys    =  [x | x <- xs, not $ any ((==) x) ys]

{-
    Conviene leer primero la definición y los comentarios de insert para enterarse de lo que hace esto.

    Devuelve la lista de posibles permutaciones para una palabra (se rompe si son de más de 8 letras). Para ello llamamos a perm de nuevo con el cuerpo de la lista, lo cual nos irá devolviendo listas cada vez más pequeñas sobre las que llamaremos a insert junto con la cabeza de la misma, permutando así todas las palabras tras acabar de la recursión de manera que al ir 'subiendo', vamos llamando a insert con palabras de diferentes tamaños, formando así las combinaciones de letras desde abajo.
-}
perm           :: [a] -> [[a]]
perm []        =  [[]]
perm (x:xs)    =  [zs | ys <- perm xs, zs <- insert x ys]

{-
    Toma como entrada una letra y una cadena (en el caso de esta práctica, aunque el tipo sea un 'a' cualquiera solo nos interesan los String) y nos devuelve las combinaciones de las entradas, insertando la letra en cada posición de la palabra. La idea aquí en llamar al método, por ejemplo con la palabra 'hola', el parámetro e=h y x:xs = o:la, en este caso el resultado contendria la palabra 'hola' y además las palabras con la forma o:ys, donde ys es la llamada recursiva a la funcion con 'h' y 'la', que nos devolvera de nuevo otra cadena con 'hla' como cabeza de lista, luego la segunda palabra de la cadena que se devuelve al final (retorno de la primera llamada a insert) es 'ohla', devolviéndonos las palabras obtenidas al permutar la primera letra de la cadena.
-}
insert             :: a -> [a] -> [[a]]
insert e []        =  [[e]]
insert e (x:xs)    =  (e:x:xs) : [(x:ys) | ys <- (insert e xs)]

{-
    Desplaza una palabra, es decir, dada una palabra devolvemos otra donde cada posición dentro de la misma corresponde a la siguiente letra del abecedario. En el cuerpo de la función usamos la función 'succ' para obtener la siguiente letra salvo en el caso de la 'z' para volver de nuevo a la letra 'a'
-}
displace           :: [Char] -> [Char]
displace []        =  []
displace (x:xs)
    | x == 'z'     = 'a' : displace xs
    | otherwise    = succ x : displace xs

{-
    Imprime un pequeño comentario por pantalla sobre la práctica y sus posibles acciones
-}
start :: IO()
start = do  putStrLn "\n\tPráctica 1 de Programación Funcional"
            putStrLn "\tPara ejecutar la práctica introduce el siguiente comando:"
            putStrLn "\t\t> ejecutar"
            putStrLn "\tPara terminar la ejecución introduce el siguiente comando:"
            putStrLn "\t\t> salir"
            parserCommand

{-
    Lee la entrada del usuario por medio de getChain, si la longitud es mayor de 1, mostramos error (solo se admiten dos comandos de una sola palabra) y en caso contrario llamamos a showInfo con el elemento en posición 0.
-}
parserCommand :: IO()
parserCommand = do putStr "\t$: "
                   chain <- getChain
                   if length chain == 1 then showInfo $ chain !! 0 else errInfo

{-
    Lee la entrada del usuario por medio de getLine y devuelve una lista de palabras tras aplicar words sobre la entrada.
-}
getChain :: IO[String]
getChain = do chain <- getLine
              return $ words chain

{-
    Imprime por pantalla el comando elegido y lanza la ejecución si es el adecuado, termina la ejecución o muestra un error y pide entrada de nuevo.
-}
showInfo :: String -> IO()
showInfo word = do  putStr "\n\tComando seleccionado: "
                    putStrLn word
                    case word of
                        "ejecutar"          -> execute
                        "salir"             -> exit
                        _                   -> errInfo

{-
    Lee la frase de partida y la palabra a buscar e imprime por pantalla la cadena resultado de la ejecución de la práctica. Para ello, tomamos el segundo argumento de la pareja que devuelve solution
-}
execute :: IO()
execute = do    putStr "\tIntroduce la frase de partida: "
                chain <- getChain
                putStr "\tIntroduce la palabra a buscar: "
                word <- getChain
                putStrLn(snd $ solution (chain, "") "Reorder" (word !! 0) 0)
                parserCommand

{-
    Aplica las operaciones permitidas sobre la frase original y mira si hay solución, en tal caso imprime por pantalla el conjunto de operaciones a aplicar (tambien sobre que palabras aplicarlas) y la profundidas de la solución (entiendase profundidad como numero de operaciones oplicadas sobre la lista entera), en caso de que no esté, si aún tenemos al menos una palabra y no hemos ejecutado más de 'deep' veces, lanzmos una nueva ejecución y en otro caso mostramos por pantalla un eror indicando que no hemos encontrado solución.

    Añadir a la linea 123 opList ++  a la cadena devuelta según lo explicado al principio del documento.
-}
solution :: ([String], String) -> String -> String -> Int -> ([String],String)
solution (xs, opList) op word deep
    | elem word xs                      = ([], opList ++ "\n\tSolución encontrada\n\tProfundidad: " ++ show deep)
    | length xs /= 0 && deep /= 5000    = loop (filter (\x -> x /= "") xs, opList) op word deep
    | otherwise                         = ([], "\n\tNo se han encontrado coincidencias\n\tProfundidad: " ++ show deep)

{-
    Aplica las operaciones permitidas en función del parametro 'op' y llama de nuevo a soluction para que mire si hay solución o continue la ejecución. En el retorno se llama a applyOperation que transforma la lista de palabras y addInformation que guarda información sobre las operaciones que vamos aplicando.
-}
loop :: ([String], String) -> String -> String -> Int -> ([String],String)
loop ([], opList) op word  deep         = ([], opList)
loop (xs, opList) "Reorder" word deep   = solution (applyOperation xs "Reorder", (opList ++ addInformation xs "Reorder")) "Concat" word (deep+1)
loop (xs, opList) "Concat" word deep    = solution (applyOperation xs "Concat", (opList ++ addInformation xs "Concat")) "Intersect" word (deep+1)
loop (xs, opList) "Intersect" word deep = solution (applyOperation xs "Intersect", (opList ++ addInformation xs "Intersect")) "Sub" word (deep+1)
loop (xs, opList) "Sub" word deep       = solution (applyOperation xs "Sub", (opList ++ addInformation xs "Sub")) "Displace" word (deep+1)
loop (xs, opList) "Displace" word deep  = solution (applyOperation xs "Displace", (opList ++ addInformation xs "Displace")) "Concat" word (deep+1)

{-
    Las cosas aparentemente raras como (last (x2:xs)) se explican en el comentario siguiente.

    Concatena información sobre las transformaciones aplicadas.
-}
addInformation :: [String] -> String -> String
addInformation (x:xs) "Reorder"         = ("\n\tPermutada: " ++ x) ++ addInformation xs "Reorder"
addInformation (x1:x2:xs) "Concat"      = ("\n\tConcatenadas: " ++ x1 ++ " " ++ (last (x2:xs))) ++ addInformation (init (x2:xs)) "Concat"
addInformation (x1:x2:xs) "Intersect"   = ("\n\tIntersecadas: " ++ x1 ++ " " ++ (last (x2:xs))) ++ addInformation (init (x2:xs)) "Intersect"
addInformation (x1:x2:xs) "Sub"         = ("\n\tRestadas: " ++ x1 ++ " " ++ (last (x2:xs))) ++ addInformation (init (x2:xs)) "Sub"
addInformation (x1:x2:xs) "Displace"    = ("\n\tDesplazada: " ++ x1 ++ " (2 veces)") ++ ("\n\tDesplazada: " ++ x2) ++ addInformation xs "Displace"
addInformation (x:xs) "Displace"        = ("\n\tDesplazadas: " ++ x) ++ addInformation xs "Displace"
addInformation _ _ = ""

{-
    Aplica las operaciones sobre las palabras de la lista xs. En el caso de todas las operaciones salvo Permutar se han definido las listas como x1:x2:xs para quedarnos con las listas que tengan al menos dos elementos, pues la idea es concatenar, intersectar, etc la primera palabra de la lista con la última, luego de haberlas definido x:xs, si tomanos last xs y xs es lista vacía tendríamos problemas a la hora de aplicar la transformación.
-}
applyOperation :: [String] -> String -> [String]
applyOperation [] _ = []
applyOperation xs "Reorder"             = foldl (\x y -> if length y >7 then y : x else perm y ++ x) [] xs
applyOperation (x1:x2:xs) "Concat"      = (x1 ++ (last (x2:xs))) : applyOperation (init (x2:xs)) "Concat"
applyOperation (x1:x2:xs) "Intersect"   = intersect x1 (last (x2:xs)) : last (x2:xs) : applyOperation (init (x2:xs)) "Intersect"
applyOperation (x1:x2:xs) "Sub"         = sub x1 (last (x2:xs)) : last (x2:xs) : applyOperation (init (x2:xs)) "Sub"
applyOperation (x1:x2:xs) "Displace"    = (displace $ displace x1) : (displace x2) : applyOperation xs "Displace"
applyOperation (x:xs) "Displace"        = displace x : applyOperation xs ""
applyOperation (x:xs) _                 = x : applyOperation xs ""

{-
    Muestra por pantalla un mensaje de error y pide de nuevo otra entrada
-}
errInfo :: IO()
errInfo = do putStrLn "\tComando no váido"
             parserCommand

{-
    Muestra un mensaje de salida de la ejecución
-}
exit :: IO()
exit = do putStrLn "\tSaliendo...\n\n"

{-
    Lanza la práctica
-}
main :: IO()
main = start