module Problems.Prob62a exposing (..)

{-
   Problem 62a
   Count internal nodes (those that have non-empty subtrees) of a binary tree.

   Example
   tree = Tree 'a' (Tree 'b' Empty (Tree 'c' Empty Empty))
                    (Tree 'd' Empty (Tree 'e' Empty Empty))
   countInternals tree == 3
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


countInternals : Tree a -> Int
countInternals tree =
    case tree of
        Empty ->
            0

        Node _ b c ->
            if b == Empty && c == Empty then
                0

            else
                1 + countInternals b + countInternals c


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
        [ countInternals Empty == 0
        , countInternals (Node 1 Empty Empty) == 0
        , countInternals (Node 1 (Node 2 Empty Empty) Empty) == 1
        , countInternals (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)) == 1
        , countInternals (Node "A" (Node "B" Empty Empty) (Node "C" Empty Empty)) == 1
        , countInternals (Node 1 (Node 1 Empty (Node 4 Empty Empty)) (Node 1 (Node 2 Empty Empty) Empty)) == 3
        ]
