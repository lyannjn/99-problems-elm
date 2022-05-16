module Problems.Prob34 exposing (..)

{-
   Problem 34

   Calculate Euler's totient function phi(m).
   Euler's totient function phi(m) is defined as the number of positive integers
   r (1 <= r < m) that are coprime with m.

   Example
   let m = 10
   then coprimes of m are 1,3,7,9
   and totient of m is 4

   Note the special case: totient 1 == 1
-}

import Html
import List
import Problems.Prob33 exposing (coprime)


totient : Int -> Int
totient n =
    if n == 0 then
        0

    else if n == 1 then
        1

    else
        let
            listn =
                List.range 1 n
        in
        -- List.length (List.filter (\y -> coprime n y) listn)
        listn |> List.filter (coprime n) |> List.length



-- List.length <| List.filter (\x -> coprime n x) (List.range 1 n)


coprime : Int -> Int -> Bool
coprime a b =
    if abs a == 0 then
        False

    else if abs b == 0 then
        False

    else
        gcd a b == 1


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd (abs b) (modBy b a)


main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            n ->
                "Your implementation failed " ++ Debug.toString n ++ " tests."


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ totient 10 == 4
            , totient 25 == 20
            , totient 120 == 32
            , totient 0 == 0
            , totient 1600 == 640
            , totient 37 == 36
            , totient 330 == 80
            , totient 65934 == 19440
            , totient 1313 == 1200
            , totient 45 == 24
            , totient -23 == 0
            ]
