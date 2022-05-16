module Problems.Extra1 exposing (..)

{-
   Extra #1 - dropWhile
   Drop items from the start of a list until an item does not satisfy criteria specified by a function.

   Example
   dropWhile isEven [2, 4, 5, 6, 7] == [5, 6, 7]
-}

import Html
import Html.Attributes exposing (list)
import List exposing (drop)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list


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
        List.filter ((==) False)
            [ dropWhile isOdd [ 1, 2, 1 ] == [ 2, 1 ]
            , dropWhile isEven [ 1, 2, 1 ] == [ 1, 2, 1 ]
            , dropWhile isEven [] == []
            , dropWhile isEven [ 2, 4, 100000, 1 ] == [ 1 ]
            , dropWhile ((>) 5) (List.range 1 10) == [ 5, 6, 7, 8, 9, 10 ]
            , dropWhile ((>) 50) (List.range 1 10) == []
            ]


isEven x =
    modBy 2 x == 0


isOdd x =
    modBy 2 x /= 0
