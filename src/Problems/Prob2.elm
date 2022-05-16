module Problems.Prob2 exposing (..)

{-
   Problem 2

   Implement the function penultimate to find the next to last element of a list.

   Example

     penultimate [1, 2, 3, 4] == Just 3
-}

import Html
import List
import Maybe


penultimate : List a -> Maybe a
penultimate list =
    list |> List.reverse |> List.drop 1 |> List.head



{-
    Solution 1:
   Recursive search for the last element

   penultimate : List a -> Maybe a
   penultimate list =
     case list of
       [ ] -> Nothing
       [y] -> Nothing
       [y, z] -> Just y
       y::ys -> penultimate ys

   Solution 2:
   Reverse the list and take the head of the tail.

   penultimate : List a -> Maybe a
   penultimate list =
     case List.reverse list of
       [ ] -> Nothing
       y::ys -> List.head ys

   Solution 3:
   Reverse the list, drop one and take the head.

   penultimate : List a -> Maybe a
   penultimate list =
     case drop 1 (List.reverse list) of
       [ ] -> Nothing
       y::ys -> Just y
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
            [ penultimate [ 1, 2, 3, 4 ] == Just 3
            , penultimate [ 1, 2 ] == Just 1
            , penultimate [ 1 ] == Nothing
            , penultimate [] == Nothing
            , penultimate [ "a", "b", "c" ] == Just "b"
            , penultimate [ "a" ] == Nothing
            ]
