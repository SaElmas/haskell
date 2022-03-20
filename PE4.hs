module PE4 where

import Data.Maybe -- up to you if you want to use it or not

-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------


----------
-- Part I:
-- Some Maybe fun! Basic practice with an existing custom datatype.

-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
toDigit c 
        | c == '0'      = Just (Digit c)
        | c == '1'      = Just (Digit c)
        | c == '2'      = Just (Digit c)
        | c == '3'      = Just (Digit c)
        | c == '4'      = Just (Digit c)
        | c == '5'      = Just (Digit c)
        | c == '6'      = Just (Digit c)
        | c == '7'      = Just (Digit c)
        | c == '8'      = Just (Digit c)
        | c == '9'      = Just (Digit c)
        | otherwise     = Nothing


-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
toDigits :: String -> Maybe PhoneNumber
toDigits [] = Nothing
toDigits (x:xs)
        | not (isAllDigit (x:xs))    = Nothing
        | otherwise                 = Just (digiList (x:xs))



------------------------ Part I Helper Functions--------------------------------

isAllDigit::String -> Bool
isAllDigit [] = True
isAllDigit (x:xs) = (toDigit x /= Nothing) && isAllDigit(xs)

digiList::String -> [Digit]
digiList [] = []
digiList (x:xs) = Digit x : digiList xs

--------------------------------------------------------------------------------



-----------
-- Part II:
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int
numContacts (Node []) = 0
numContacts (Leaf _) = 1
numContacts (Node xs) = sum [numContacts x | (_,x) <- xs]

    
-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts x = zip (listDigits x) (listLeaves x)

-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete s dt = iSolveForList (toDigits s) (getContacts dt)


------------------------ Part II Helper Functions--------------------------------

listLeaves::DigitTree -> [String]
listLeaves (Leaf x) = [x]
listLeaves (Node xs) = [a | (_,x) <- xs, a<-listLeaves x]

listDigits::DigitTree -> [[Digit]]
listDigits (Leaf _) = [[]]
listDigits (Node xs) = [ Digit a: b   | (Digit a, x) <- xs, b<-listDigits x ]

iSolveForOnePair::[Digit] -> (PhoneNumber, String) -> (PhoneNumber, String)
iSolveForOnePair [] x= x
iSolveForOnePair _ ([],name) = ([],"xxxx")
iSolveForOnePair (x:xs) (y:ys, name)
            | x==y      = (iSolveForOnePair xs (ys, name))
            | otherwise = ([],"xxxx")
            
iSolveForList:: Maybe [Digit] -> [(PhoneNumber, String)] -> [(PhoneNumber,String)]
iSolveForList Nothing _ = []
iSolveForList _ [] = []
iSolveForList (Just digits) (x:xs)
            | k == ([],"xxxx")      = iSolveForList (Just digits) xs
            | otherwise             = k: iSolveForList (Just digits) xs
                where
                    k = iSolveForOnePair digits x

-----------
-- Example Trees
-- Two example trees to play around with, including THE exampleTree from the text. 
-- Feel free to delete these or change their names or whatever!

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

