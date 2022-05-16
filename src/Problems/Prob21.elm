{-
   Problem 21

   Insert an element at a given position into a list. Treat the first position as index 1.

   Example
   insertAt 2 'l' ['E', 'm'] == ['E', 'l', 'm']
-}


module Problems.Prob21 exposing (insertAt, main, test)

import Html
import List


insertAt : Int -> a -> List a -> List a
insertAt n v xs =
    List.take (n - 1) xs ++ (v :: List.drop (n - 1) xs)



{-
   Solution 1
   split : List a -> Int -> ( List a, List a )
   split list count =
       ( List.take count list, List.drop count list )


   insertAt : Int -> a -> List a -> List a
   insertAt n v xs =
       let
           ( front, back ) =
               split xs (n - 1)
       in
           front ++ (v :: back)

   Or
       let
           ( front, back ) =
               ( List.take (n - 1) xs, List.drop (n - 1) xs )
       in
          front ++ (v :: back)

   Solution 2 RECURSION
   insertAt : Int -> a -> List a -> List a
   insertAt n v xs =
       if n < 2 then
           v :: xs
       else
           case xs of
               [] ->
                   [ v ]

               y :: ys ->
                   y :: insertAt (n - 1) v ys
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
            [ insertAt 2 99 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 99, 2, 5, 5, 2, 1 ]
            , insertAt 3 99 (List.range 1 14) == [ 1, 2, 99, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
            , insertAt 6 99 (List.range 1 5) == [ 1, 2, 3, 4, 5, 99 ]
            , insertAt 0 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt -1 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt 1 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt 2 "x" [ "1", "2", "3", "4", "5" ] == [ "1", "x", "2", "3", "4", "5" ]
            ]
