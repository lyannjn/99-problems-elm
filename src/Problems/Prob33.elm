module Problems.Prob33 exposing (..)

{-
   Problem 33

   Determine whether two positive integer numbers are coprime.
   Two numbers are coprime if their greatest common divisor equals 1.

   coprime 35 64 = True
-}

import Html
import List


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
        abs a

    else
        gcd (abs b) (modBy b a)


main =
    Html.text
        (if test then
            "Your implementation passed all tests."

         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [ ( coprime 36 63, False )
        , ( coprime 10 25, False )
        , ( coprime 120 120, False )
        , ( coprime 2 12, False )
        , ( coprime 1313 1600, True )
        , ( coprime 23 37, True )
        , ( coprime 45 330, False )
        , ( coprime 24528 65934, False )
        , ( coprime 1600 1313, True )
        , ( coprime -23 37, True )
        , ( coprime 330 45, False )
        , ( coprime -23 37, True )
        , ( coprime -330 -45, False )
        , ( coprime -24528 65934, False )
        ]
