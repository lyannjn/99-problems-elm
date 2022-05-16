module Problems.Prob15 exposing (..)

{-
   Problem 15
   Repeat each element of a list a given number of times.

   Example
   repeatElements 3 [1, 2, 3, 3] == [1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3]
-}

import Html
import List


repeatElements : Int -> List a -> List a
repeatElements count list =
    case list of
        [] ->
            []

        x :: xs ->
            List.repeat count x ++ repeatElements count xs


repeatElementsB : Int -> List a -> List a
repeatElementsB n list =
    let
        repeatElement =
            List.repeat n
    in
    List.concatMap repeatElement list



{-
   Solution 2 - concatMap
   repeatElements : Int -> List a -> List a
   repeatElements n list =
       List.concatMap (\x -> List.repeat n x) list

   Solution 3 - fold
   repeatElements : Int -> List a -> List a
   repeatElements n list =
       List.foldl (\x y -> y ++ (List.repeat n x)) [] list
-}


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
            [ repeatElements 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 1, 2, 2, 5, 5, 5, 5, 2, 2, 1, 1 ]
            , repeatElements 4 [ 1, 2 ] == [ 1, 1, 1, 1, 2, 2, 2, 2 ]
            , repeatElements 4 [] == []
            , repeatElements 0 [ 1, 2 ] == []
            , repeatElements -1 [ 1, 2 ] == []
            , repeatElements 40 [ 1 ] == [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
            , repeatElements 4 [ "1", "2" ] == [ "1", "1", "1", "1", "2", "2", "2", "2" ]
            ]
