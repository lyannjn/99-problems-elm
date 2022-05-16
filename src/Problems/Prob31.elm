module Problems.Prob31 exposing (..)

{-
   Problem 31

   Determine whether a given integer number is prime.

   Example:
   isPrime 113 == True
-}

import Html


isPrime :
    Int
    -> Bool --Sencillo: doy el número y que me regrese si sí es primo o no
isPrime n =
    if n < 2 then
        --Todos los números absolutos menores a |2| no son primos
        False

    else
        --Analizar los números absolutos mayores a |2|
        eratos (abs n) (List.range 2 (n // 2))



--El número a analizar debe de ser absoluto,
--El rango va desde 2 a ???????


eratos : Int -> List Int -> Bool
eratos n cs =
    case cs of
        [] ->
            --Si el loop se detiene con una lista vacía
            --eso quiere decir que el último n/x tuvo residuo, por lo tanto es primo
            True

        x :: xs ->
            --destrucura el elemento inicial "x" de la lista [xs]
            --split a list into a head and a tail
            if modBy x n == 0 then
                --Si n/x no tiene residuo, no es primo
                False

            else
                {- La lista Int que recibe, la va filtrando con una función anónima para verificar
                   si cada elemento de la lista ("y" representa cada elemento) de la lista "xs"
                   tiene un residuo diferente a cero (dividiéndolo entre el primer elemento de la lista)
                   (List.filter regresa una lista con los datos que SI pasaron el filtro)
                   es decir, List.filter está regresando posibles números primos y por lo tanto,
                   vuelve a llamar a la función y hace lo mismo hasta 0
                -}
                eratos n (List.filter (\y -> modBy x y /= 0) xs)


main : Html.Html a
main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ Debug.toString x ++ " tests."


test : Int
test =
    List.length <|
        List.filter (\( result, expect ) -> result /= expect)
            [ ( isPrime 36, False )
            , ( isPrime 10, False )
            , ( isPrime -1, False )
            , ( isPrime 1, False )
            , ( isPrime 0, False )
            , ( isPrime 120, False )
            , ( isPrime 2, True )
            , ( isPrime 23, True )
            , ( isPrime 6000, False )
            , ( isPrime 7919, True )
            , ( isPrime 7911, False )
            , ( isPrime 63247, True )
            , ( isPrime 63249, False )
            ]
