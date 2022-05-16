module Problems.Prob7 exposing (..)

{-
   Problem 7
   Flatten a nested lists into a single list.
   Because Lists in Elm are homogeneous we need to define what a nested list is.

   type NestedList a = Elem a | List [NestedList a]

   Example
   nl1 =
       SubList
           [ Elem 1
           , SubList
               [ SubList
                   [ Elem 2
                   , SubList [ Elem 3, Elem 4 ]
                   ]
               , Elem 5
               ]
           , Elem 6
           , SubList [ Elem 7, Elem 8, Elem 9 ]
           ]
   flatten nl1 == List.range 1 9
-}

import Html
import List exposing (concat)
import Problems.Prob18 exposing (sublist)


type NestedList a
    = Elem a
    | SubList (List (NestedList a))


flatten : NestedList a -> List a
flatten nl =
    case nl of
        Elem x ->
            [ x ]

        SubList a ->
            a |> List.concatMap flatten


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


nl0 =
    SubList [ Elem 1, Elem 2 ]


nl1 =
    SubList
        [ Elem 1
        , SubList
            [ SubList
                [ Elem 2
                , SubList [ Elem 3, Elem 4 ]
                ]
            , Elem 5
            ]
        , Elem 6
        , SubList [ Elem 7, Elem 8, Elem 9 ]
        ]


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ flatten nl1 == List.range 1 9
            , flatten (SubList [ Elem 1, Elem 2 ]) == [ 1, 2 ]
            , flatten (SubList [ Elem "a", Elem "b" ]) == [ "a", "b" ]
            , flatten (SubList []) == []
            ]
