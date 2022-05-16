module Problems.Prob68a exposing (..)

{-
   Problem 68a
   There are three commonly used traversal forms, pre-order, in-order and post-order.

   Pre-order traversal of a binary tree displays the data value of the node, then traverses the left subtree
   then traverses the right subtree.
   For example, pre-order traversal of the tree from Problem 67 gives us a, b, d, e, c, f, g.
   In-order traversal traverses the left subtree, then shows the data value,
   then traverses the right subtree.
   Post-order traverses the left subtree, then the right subtree, then shows the data value.

   Write a function to traverse a tree and return the values in a list in pre-order.

   Example
   tree67 =
       Node 'a'
           (Node 'b'
               (Node 'd' Empty Empty)
               (Node 'e' Empty Empty))
           (Node 'c'
               Empty
               (Node 'f'
                   (Node 'g' Empty Empty)
                   Empty))

   preorder tree67 == ['a', 'b', 'd', 'e', 'c', 'f', 'g']
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty ->
            []

        Node a b c ->
            a :: preorder b ++ preorder c


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
        [ preorder Empty == []
        , preorder tree67 == [ 'a', 'b', 'd', 'e', 'c', 'f', 'g' ]
        , preorder tree68 == List.range 1 9
        , preorder tree == [ 6, 2, 1, 4, 3, 5, 7 ]
        ]


tree67 =
    Node 'a'
        (Node 'b'
            (Node 'd' Empty Empty)
            (Node 'e' Empty Empty)
        )
        (Node 'c'
            Empty
            (Node 'f'
                (Node 'g' Empty Empty)
                Empty
            )
        )


tree68 =
    Node 1
        (Node 2
            (Node 3 Empty Empty)
            (Node 4 Empty Empty)
        )
        (Node 5
            Empty
            (Node 6
                (Node 7
                    Empty
                    (Node 8
                        (Node 9 Empty Empty)
                        Empty
                    )
                )
                Empty
            )
        )


tree =
    Node 6
        (Node 2
            (Node 1 Empty Empty)
            (Node 4
                (Node 3 Empty Empty)
                (Node 5 Empty Empty)
            )
        )
        (Node 7 Empty Empty)
