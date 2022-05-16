module Problems.Prob62b exposing (..)

{-
   Problem 62b
   Extract the internal nodes (those that have non-empty subtrees) of a binary tree into a list.

   Example
   tree = Tree 'a' (Tree 'b' Empty (Tree 'c' Empty Empty))
                    (Tree 'd' Empty (Tree 'e' Empty Empty))

   getInternals tree == ['a', 'b', 'd']
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


getInternals : Tree a -> List a
getInternals tree =
    case tree of
        Empty ->
            []

        Node a b c ->
            if b == Empty && c == Empty then
                []

            else
                a :: getInternals b ++ getInternals c


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
            [ getInternals Empty == []
            , getInternals (Node 1 Empty Empty) == []
            , List.sort (getInternals (Node 1 (Node 2 Empty Empty) Empty)) == [ 1 ]
            , List.sort (getInternals (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))) == [ 1 ]
            , List.sort (getInternals (Node "A" (Node "B" Empty Empty) (Node "C" Empty Empty))) == [ "A" ]
            , List.sort
                (getInternals
                    (Node 1
                        (Node 1 Empty (Node 4 Empty Empty))
                        (Node 1 (Node 2 Empty Empty) Empty)
                    )
                )
                == [ 1, 1, 1 ]
            ]
