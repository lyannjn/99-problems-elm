module Problems.Prob61a exposing (..)

{-
   Problem 61a
   Count the leaves of a binary tree.
   The leaves are the nodes that have empty nodes in both their right and left subtrees.

   Example
   tree = Tree 1 (Tree 2 Empty (Tree 4 Empty Empty))
                    (Tree 2 Empty Empty)

   countLeaves tree == 2
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


countLeaves : Tree a -> Int
countLeaves tree =
    case tree of
        Empty ->
            0

        Node _ b c ->
            if b == Empty && c == Empty then
                1

            else
                countLeaves b + countLeaves c


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
        [ countLeaves Empty == 0
        , countLeaves (Node 1 Empty Empty) == 1
        , countLeaves (Node 1 (Node 2 Empty Empty) Empty) == 1
        , countLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)) == 2
        , countLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)) == 2
        ]
