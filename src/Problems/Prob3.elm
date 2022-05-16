module Problems.Prob3 exposing (..)

{-
   Problem 3
   Implement the function elementAt to return the n-th element of a list.
   The index is 1-relative, that is, the first element is at index 1.

   Example:
   elementAt [3, 4, 5, 6] 2 == Just 4
-}

import Html
import List
import Maybe


elementAt : List a -> Int -> Maybe a
elementAt xs n =
    if n > 0 then
        xs |> List.drop (n - 1) |> List.head

    else
        Nothing



{-
   elementAt : List a -> Int -> Maybe a
   elementAt list n =
       case list of
           [] ->
               Nothing

           x :: xs ->
               if n == 1 then
                   Just x
               else
                   elementAt xs (n - 1)

   elementAt : List a -> Int -> Maybe a
   elementAt list n =
       if n < 1 then
           Nothing
       else
           case List.drop (n - 1) list of
               [] ->
                   Nothing

               y :: ys ->
                   Just y
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
            [ elementAt [ 1, 2, 3, 4 ] 2 == Just 2
            , elementAt [ 1 ] 2 == Nothing
            , elementAt [ 1 ] 1 == Just 1
            , elementAt [] 2 == Nothing
            , elementAt [] -1 == Nothing
            , elementAt [] 0 == Nothing
            , elementAt [ 1, 2, 3, 4 ] -1 == Nothing
            , elementAt [ 1, 2, 3, 4 ] 0 == Nothing
            , elementAt [ 'a', 'b', 'c' ] 2 == Just 'b'
            ]
