module Problems.Prob18 exposing (main, sublist, test)

import Array exposing (..)
import Html
import Html.Attributes exposing (start)
import List



{-

   Extract a slice from a list.

   Given a list, return the elements between (inclusively) two indices.
   Start counting the elements with 1.
   Indices outside of the list bounds
   (i.e. negative number, or beyond the length of the list)
   should be clipped to the bounds of the list.


   Example

   slice 3 7 (List.range 1 10) == (List.range 3 7)
   slice -7 -3 (List.range 1 10) == []

-}


sublist : Int -> Int -> List a -> List a
sublist start end list =
    list |> List.take end |> List.drop (start - 1)



{-

   Solution 1

   sublist : Int -> Int -> List a -> List a
   sublist start end list =
       let
           s_ = max (start-1) 0
           e_ = max end 0
       in
           List.drop s_ list |> List.take (e_ - s_)


   Solution 2

   Use Array.slice. Note that Array.slice uses negative indices to count from the end of the array.

   sublist : Int -> Int -> List a -> List a
   sublist start end list =
       let
           s' = max (start-1) 0
           e' = max end 0
       in
           Array.fromList list
               |> Array.slice s' e'
               |> Array.toList
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
    [ True
    , sublist 3 7 (List.range 1 10) == List.range 3 7
    , sublist 2 100 [ 'a', 'b', 'c' ] == [ 'b', 'c' ]
    , sublist -1 2 (List.range 1 100) == [ 1, 2 ]
    , sublist -3 -2 [ -3, -2, -1, 0, 1, 2, 3 ] == []
    , sublist 5 3 [ "indices", " are", "inverted" ] == []
    , sublist 0 1 (List.range 1 10) == [ 1 ]
    , sublist -7 -3 (List.range 1 10) == []
    ]
        |> (List.filter <| (==) False)
        |> List.length
