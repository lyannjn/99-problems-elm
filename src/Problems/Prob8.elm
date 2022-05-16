module Problems.Prob8 exposing (..)

{-
   Problem 8
   Write a function to remove consecutive duplicates of list elements.

   Example

   noDupes [1, 1, 2, 2, 3, 3, 3, 4, 5, 4, 4, 4, 4]
       == [1, 2, 3, 4, 5, 4]
-}

import Html
import List
import Maybe


noDupes : List a -> List a
noDupes list =
    case list of
        [] ->
            []

        -- x :: xs
        -- noDupes (x :: dropWhile ((==) x) xs) == noDupes (x :: dropWhile (/z -> z == x) xs)
        x :: ((y :: _) as xs) ->
            if x == y then
                noDupes xs

            else
                x :: noDupes xs

        [ x ] ->
            [ x ]



{-
   Solution #1
   noDupes : List a -> List a
   noDupes xs =
       List.foldr noDupCons [] xs

   noDupCons : a -> List a -> List a
   noDupCons x xs =
       case List.head xs of
           Nothing -> [x]
           Just a  ->
               if x == a then
                   xs
               else
                   x :: xs

   Solution #2:
   Define a dropWhile function to remove duplicates from the head of a list, then apply it recursively.

   noDupes : List a -> List a
   noDupes list =
       case list of
           []    -> []
           x::xs -> x :: (noDupes <| dropWhile (\u -> u == x) xs)

   dropWhile : (a -> Bool) -> List a -> List a
   dropWhile predicate list =
       case list of
         []      -> []

         x::xs   ->
             if (predicate x) then
                 dropWhile predicate xs
             else
                 list
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
            [ noDupes [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ 1, 2, 5, 2, 1 ]
            , noDupes [ 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 2, 2, 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 1 ] == [ 1 ]
            , noDupes [] == []
            , noDupes [ "aa", "aa", "aa" ] == [ "aa" ]
            , noDupes [ "aab", "b", "b", "aa" ] == [ "aab", "b", "aa" ]
            ]
