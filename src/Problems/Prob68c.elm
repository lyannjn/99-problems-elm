module Problems.Prob68c exposing (..)

{-
   Problem 68c
   Write a function to traverse a tree and return the values in a list in post-order.

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

   postorder tree67 == ['d', 'e', 'b', 'g', 'f', 'c', 'a']
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty ->
            []

        Node a b c ->
            postorder b ++ postorder c ++ [ a ]


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
        [ postorder Empty == []
        , postorder tree67 == [ 'd', 'e', 'b', 'g', 'f', 'c', 'a' ]
        , postorder tree68 == [ 3, 4, 2, 9, 8, 7, 6, 5, 1 ]
        , postorder tree == [ 1, 3, 5, 4, 2, 7, 6 ]
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
