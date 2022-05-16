module Problems.Prob1 exposing (..)

{-
   Problem 1

   Write a function last that returns the last element of a list.
   An empty list doesn't have a last element, therefore last must return a Maybe.

   Example

   import Html exposing (text)
   import Maybe


   last : List a -> Maybe a
   last xs =
     -- your implementation goes here
     Nothing


   main =
       case last [ 1, 2, 3, 4 ] of
           Just a ->
               text (toString a)

           Nothing ->
               text "No element found"
   Result:

   4

   Unit Test

   Let's create a more complete test, checking last against an empty list and lists of different types.
   We will use assertEqual which takes a tuple, where the first element will hold a test condition
   and the second holds the expected result. This will allow us to test multiple conditions concisely.
-}

import Html
import List
import Maybe


lastC : List a -> Maybe a
lastC list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (List.foldl (\a b -> a) x list)


lastB : List a -> Maybe a
lastB xs =
    case xs of
        [] ->
            Nothing

        [ a ] ->
            Just a

        _ :: tail ->
            last tail


last : List a -> Maybe a
last xs =
    xs |> List.reverse |> List.head


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
            [ last (List.range 1 4) == Just 4
            , last [ 1 ] == Just 1
            , last [] == Nothing
            , last [ 'a', 'b', 'c' ] == Just 'c'
            ]
