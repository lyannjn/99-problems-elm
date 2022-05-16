module Problems.Prob57 exposing (..)

{-
   Problem 57
   Build a binary search tree from a list. Place lower values the right.
   By definition, duplicate values are omitted.

   Example
   toBSTree [6, 2, 4, 20, 1, 11, 12, 14] ==
               Node 6
                 (Node 2
                   (Node 1 Empty Empty)
                   (Node 4 Empty Empty))
                 (Node 20
                   (Node 11
                       Empty
                       (Node 12
                           Empty
                           (Node 14 Empty Empty)))
                   Empty)
-}

import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)



{-
   toBSTree : List comparable -> Tree comparable
   toBSTree list =
       case list of
           [] ->
               Empty

           _ ->
               List.foldl addBSNode Empty list


   addBSNode : comparable -> Tree comparable -> Tree comparable
   addBSNode v tree =
       case tree of
           Empty ->
               Node v Empty Empty

           Node v_ left right ->
               if v > v_ then
                   Node v_ left (addBSNode v right)
               else if v < v_ then
                   Node v_ (addBSNode v left) right
               else
                   tree
   Solution 2

   Here's an modification using Basics.compare.

   toBSTree : List comparable -> Tree comparable
   toBSTree list =
       case list of
           [] ->
               Empty

           _ ->
               List.foldl addBSNode Empty list


   addBSNode : comparable -> Tree comparable -> Tree comparable
   addBSNode v tree =
       case tree of
           Empty ->
               Node v Empty Empty

           Node v_ left right ->
               case compare v v_ of
                   GT ->
                       Node v_ left (addBSNode v right)

                   LT ->
                       Node v_ (addBSNode v left) right

                   EQ ->
                       tree
-}


toBSTree : List comparable -> Tree comparable
toBSTree list =
    case list of
        [] ->
            Empty

        x :: xs ->
            let
                leftList =
                    dropWhile ((<) x) (List.filter (\y -> y < x) xs)

                -- drop xs while n > x
                rightList =
                    dropWhile ((>) x) (List.filter (\y -> y > x) xs)

                -- drop xs while n < x
            in
            Node x (toBSTree leftList) (toBSTree rightList)


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
        [ toBSTree [] == Empty
        , toBSTree [ 1 ] == Node 1 Empty Empty
        , toBSTree [ 1, 1, 1 ] == Node 1 Empty Empty
        , toBSTree (List.range 1 5) == Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))))
        , toBSTree (List.reverse (List.range 1 5)) == Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) Empty) Empty
        , toBSTree [ 6, 2, 4, 20, 1, 11, 12, 14, 6 ]
            == Node 6
                (Node 2
                    (Node 1 Empty Empty)
                    (Node 4 Empty Empty)
                )
                (Node 20
                    (Node 11
                        Empty
                        (Node 12
                            Empty
                            (Node 14 Empty Empty)
                        )
                    )
                    Empty
                )
        ]
