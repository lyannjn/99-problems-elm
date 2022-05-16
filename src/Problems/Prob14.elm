module Problems.Prob14 exposing (..)

{-
   Problem 14
   Duplicate each element of a list.

   Example
   duplicate [1, 2, 3, 5, 8, 8] == [1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8]
-}

import Html exposing (text)
import List


duplicate : List a -> List a
duplicate list =
    case list of
        [] ->
            []

        x :: xs ->
            [ x ] :: [ x ] :: duplicate xs



{-
   Solution 2 - concatMap
   duplicate : List a -> List a
   duplicate list =
       List.concatMap (\x -> [x, x]) list

   Solution 3 - fold
   duplicate : List a -> List a
   duplicate list =
       List.foldl (\x y -> y ++ [x, x]) [] list
-}


main =
    text
        (if test then
            "Your implementation passed all tests."

         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [ ( duplicate [ 1, 2, 3, 5, 8, 8 ], [ 1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8 ] )
        , ( duplicate [], [] )
        , ( duplicate [ 1 ], [ 1, 1 ] )
        ]
        && List.all (\( result, expect ) -> result == expect)
            [ ( duplicate [ "1", "2", "5" ]
              , [ "1", "1", "2", "2", "5", "5" ]
              )
            ]
