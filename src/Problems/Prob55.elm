module Problems.Prob55 exposing (..)

{-
   Problem 55
   In a balanced binary tree the difference between the number of nodes in the
   left and right subtrees of every node is less than one.

   Write a function to construct a balanced binary tree for a given number of nodes.
   Put the letter 'x' as data value for all nodes of the tree.

   Example
   balancedTree 3 ==
       (Node 'x'
           (Node 'x' (Empty) (Empty))
           (Node 'x' (Empty) (Empty)))
-}

import Html
import List
import String exposing (left)


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


balancedTree : Int -> Tree Char
balancedTree n =
    if n <= 0 then
        Empty

    else
        let
            left =
                --ceiling (toFloat (n - 1) / 2)
                --ceiling <| (/) 2 <| toFloat <| n - 1
                n - 1 |> toFloat |> (/) 2 |> ceiling

            right =
                (n - 1) - left
        in
        --Node 'x' is the #1 Node in every tree, that is why in left & right we write n - 1
        Node 'x' (balancedTree <| left) (balancedTree <| right)



{-
   Solution 1
   Use List.foldl, adds one node at a time.

   balancedTree : Int -> Tree Char
   balancedTree n =
        List.foldl (addBalancedNode) Empty <| List.repeat n 'x'


   addBalancedNode : a -> Tree a -> Tree a
   addBalancedNode v tree =
       case tree of
           Empty ->
               Node v Empty Empty

           Node v' left right ->
               if count left > count right then
                   Node v' left (addBalancedNode v right)
               else
                   Node v' (addBalancedNode v left) right


   -- count number of Nodes in a Tree
   count : Tree a -> Int
   count tree =
       case tree of
           Empty -> 0

           Node n left right ->
               1 + (count left) + (count right)

   Solution 2
   A more efficient solution adds many nodes at once.


   import Html
   import List
   import String


   type Tree a
       = Empty
       | Node a (Tree a) (Tree a)


   balancedTree : Int -> Tree Char
   balancedTree n =
       if n < 1 then
           Empty
       else
           case n of
               1 ->
                   Node 'x' Empty Empty

               n ->
                   let
                       n' = (n-1) // 2
                   in
                       if n % 2 == 1 then
                           Node 'x' (balancedTree n') (balancedTree n')
                       else
                           Node 'x' (balancedTree (n' + 1)) (balancedTree n')
-}


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
        [ count Empty == 0
        , count (Node 'x' Empty Empty) == 1
        , count (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty)) == 3
        , count
            (Node 'x'
                (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
                (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
            )
            == 7
        , balancedTree 0 == Empty
        , balancedTree -1 == Empty
        , balancedTree 1 == Node 'x' Empty Empty
        , balancedTree 3 == Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty)
        , balancedTree 7
            == Node 'x'
                (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
                (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
        , testBalance (balancedTree 3) 3 == True
        , testBalance (balancedTree 4) 4 == True
        , testBalance (balancedTree 5) 5 == True
        , testBalance (balancedTree 7) 7 == True
        , testBalance (balancedTree 21) 21 == True
        , testBalance (balancedTree 31) 31 == True
        , testBalance (balancedTree 32) 32 == True
        , testBalance (balancedTree 33) 33 == True
        , testBalance (balancedTree 59) 59 == True
        , testBalance (balancedTree 64) 64 == True
        , testBalance (balancedTree 73) 73 == True
        , testBalance (balancedTree 5000) 5000 == True
        ]


testBalance : Tree a -> Int -> Bool
testBalance tree n =
    case tree of
        Empty ->
            n == 0

        Node node left right ->
            List.all ((==) True)
                [ (abs (count left) - count right) < 2
                , count tree == n
                ]



-- count number of Nodes in a Tree


count : Tree a -> Int
count tree =
    case tree of
        Empty ->
            0

        Node n left right ->
            1 + count left + count right
