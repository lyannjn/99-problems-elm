module Problems.Prob63 exposing (..)

{-
   Problem 63
   We define a left-justified, complete binary tree with height H as a tree where:

   levels 1,...,H-1 contain the maximum number of nodes,
   and in level H all the nodes are "left-justified", that is, as far to the left is possible.
   Construct such a complete tree of unknown height, with n nodes containing v as label.

   Example for v='x' and n=4:
   completeTree 'x' 4 ==
   Node 'x' (Node 'x' (Node 'x' Empty Empty) Empty) (Node 'x' Empty Empty)
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


completeTree : a -> Int -> Tree a
completeTree v n =
    -- your implementation here
    Empty


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
        [ completeTree 'x' 0 == Empty
        , completeTree 'x' 1 == Node 'x' Empty Empty
        , completeTree 'x' 3 == Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty)
        , completeTree 'x' 5
            == Node 'x'
                (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
                (Node 'x' Empty Empty)
        ]
