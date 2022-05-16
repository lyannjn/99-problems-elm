module Problems.Prob68b exposing (..)

{-
   Problem 68b
   Write a function to traverse a tree and return the values in a list in in-order.

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
   inorder tree67 == ['d', 'b', 'e', 'a', 'c', 'g', 'f']
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty ->
            []

        Node a b c ->
            inorder b ++ a :: inorder c


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
        [ inorder Empty == []
        , inorder tree == List.range 1 7
        , inorder tree67 == [ 'd', 'b', 'e', 'a', 'c', 'g', 'f' ]
        , inorder tree68 == [ 3, 2, 4, 1, 5, 7, 9, 8, 6 ]
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
