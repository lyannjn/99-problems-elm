module Problems.Prob10 exposing (..)

{-
   Problem 10
   Run-length encode a list of list to a list of tuples.
   Unlike lists, tuples can mix types.
   Use tuples (n, e) to encode a list where n is the number of duplicates of the element e.

   Example
   tuples =
       [(3, 'a')
       , (1, 'b')
       , (3, 'c')
       , (4, 'd')
       , (5, 'e')
       , (6,'f')]

   lists =
       [ ['a','a','a']
       , ['b']
       , ['c', 'c', 'c']
       , ['d','d','d','d']
       , ['e']
       , ['f', 'f']
       ]

   runLengths lists == tuples

   Hints
   What's a function to convert a list of one type to a list of another type?
-}

import Html
import List
import Maybe


id : a -> a
id x =
    x


runLengths : List (List a) -> List ( Int, a )
runLengths list =
    let
        f x =
            case x of
                [] ->
                    Nothing

                y :: _ ->
                    Just ( List.length x, y )

        maybes =
            List.map f list
    in
    List.filterMap id maybes


runLengths2 : List (List a) -> List ( Int, a )
runLengths2 list =
    let
        f x =
            x |> List.head |> Maybe.map (Tuple.pair (List.length x))

        maybes =
            List.map f list
    in
    List.filterMap id maybes



{-
   Solution 1

   Use List.map to convert from List (Int, a) to List (Int, Maybe a).
   Use removeNothings to convert to List (Int, a).
   import List exposing (map, length, head)
   runLengths : List (List a) -> List ( Int, a )
   runLengths xss =
       map (\xs -> ( length xs, head xs )) xss |> removeNothings


   removeNothings : List ( Int, Maybe a ) -> List ( Int, a )
   removeNothings xs =
       case xs of
           [] ->
               []

           ( len, Nothing ) :: rest ->
               removeNothings rest

           ( len, Just y ) :: rest ->
               ( len, y ) :: removeNothings rest
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
            [ runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
            , runLengths [ [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                == [ ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
            , runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ] ]
                == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ) ]
            , runLengths [ [ 1, 1, 1, 1 ] ]
                == [ ( 4, 1 ) ]
            , runLengths [ [ "a", "a", "a", "a" ], [ "b" ], [ "c", "c" ], [ "b" ], [ "a" ] ]
                == [ ( 4, "a" ), ( 1, "b" ), ( 2, "c" ), ( 1, "b" ), ( 1, "a" ) ]
            , runLengths [ [] ] == []
            , runLengths [ [], [ "a", "a" ] ] == [ ( 2, "a" ) ]
            , runLengths [] == []
            ]
