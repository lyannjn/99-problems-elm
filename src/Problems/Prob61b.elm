module Problems.Prob61b exposing (..)

{-
   Problem 61b
   Extract the values of the leaves of a binary tree into a list.

   Example
   tree = Tree 1 (Tree 2 Empty (Tree 4 Empty Empty))
                    (Tree 2 Empty Empty)
   getLeaves aTree == [4, 2]
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


getLeaves : Tree comparable -> List comparable
getLeaves tree =
    case tree of
        Empty ->
            []

        Node a b c ->
            if b == Empty && c == Empty then
                [ a ]

            else
                getLeaves b ++ getLeaves c


main =
    Html.text
        (if test then
            "Your implementation passed all tests."

         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ getLeaves Empty == []
        , getLeaves (Node 1 Empty Empty) == [ 1 ]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) Empty)) == [ 2 ]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)))
            == [ 0, 2 ]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)))
            == [ 0, 2 ]
        , List.sort (getLeaves (Node "A" (Node "B" Empty Empty) (Node "C" Empty Empty)))
            == [ "B", "C" ]
        ]
