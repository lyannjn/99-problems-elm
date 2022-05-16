{-
   Problem 6

   Determine if a list is a palindrome, that is, the list is identical when read forward or backward.

   Example
   isPalindrome [1,2,3,2,1] == True
-}


module Problems.Prob6 exposing (isPalindrome, main)

import Html
import List
import Maybe


isPalindrome : List a -> Bool
isPalindrome xs =
    List.reverse xs == xs


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


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ isPalindrome [ 1, 3, 5, 8, 5, 3, 1 ] == True
            , isPalindrome [ 2, 1 ] == False
            , isPalindrome [ 1 ] == True
            , isPalindrome [] == True
            , isPalindrome [ "aa", "bb", "aa" ] == True
            , isPalindrome [ "aab", "b", "aa" ] == False
            ]
