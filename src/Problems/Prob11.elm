module Problems.Prob11 exposing (..)

{-
   Problem 11
   Write a function to run length encode a list,
   but instead of using a tuple as in problem 10, use a union data type.

   type RleCode a = Run Int a | Single a

   Example
   rleEncode [1, 1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5, 5, 5]
       == [Run 4 1, Single 2, Run 2 3, Single 4, Run 6 5]
-}

import Html
import List
import Maybe
import Problems.Prob15 exposing (repeatElements)


type RleCode a
    = Run Int a
    | Single a


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


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


rleEncode : List a -> List (RleCode a)
rleEncode list =
    case list of
        [] ->
            []

        x :: _ ->
            let
                first =
                    takeWhile ((==) x) list

                next =
                    dropWhile (\y -> y == x) list
            in
            case List.length first of
                1 ->
                    Single x :: rleEncode next

                n ->
                    Run n x :: rleEncode next


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
            [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
            , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
            , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
            , rleEncode [ 1 ] == [ Single 1 ]
            , rleEncode [] == []
            , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
            , rleEncode [ "aab", "b", "b", "aa" ]
                == [ Single "aab", Run 2 "b", Single "aa" ]
            ]
