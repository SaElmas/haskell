module PE2 where

---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------

-- Note: undefined is a value that causes an error when evaluated. Replace it with
-- a viable definition! Name your arguments as you like by changing the holes: _

--------------------------
-- Part I: Time to inf up!

-- naturals: The infinite list of natural numbers. That's it!
naturals :: [Integer]
naturals = [0..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
integers = interleave naturals [-1,-2..]

--------------------------------
-- Part II: SJSON Prettification

-- splitOn: Split string on first occurence of character.
splitOn :: Char -> String -> (String, String)
splitOn c [] = ([], [])
splitOn c (x:xs)
            | c == x    = ([], xs)
            | otherwise = (x:fst(splitOn c xs), snd(splitOn c xs))




-- tokenizeS: Transform an SJSON string into a list of tokens.
tokenizeS :: String -> [String]
tokenizeS [] = []
tokenizeS (x:xs)
            | x == '{'      = "{" : tokenizeS xs
            | x == ':'      = ":" : tokenizeS xs
            | x == '}'      = "}" : tokenizeS xs
            | x == ','      = "," : tokenizeS xs
            | x == '\''     = fst (splitOn ('\'') xs) : tokenizeS (snd (splitOn ('\'') xs))
            | x==' '        = tokenizeS xs
            | x=='\n'       = tokenizeS xs
            | x=='\t'       = tokenizeS xs



-- prettifyS: Prettify SJSON, better tokenize first!
prettifyS :: String -> String
prettifyS str = pf_h 0 (tokenizeS str)
            

pf_h :: Int -> [String] -> String
pf_h _ [] = ""
pf_h n (x:xs)
            | x == "{"      ="{\n"  ++ genSpace (n+4)           ++ pf_h (n+4)   xs
            | x == "}"      = "\n" ++  genSpace (n-4) ++ "}"    ++ pf_h (n-4)   xs
            | x == ":"      =": "                               ++ pf_h n       xs
            | x == ","      =",\n"  ++ genSpace n               ++ pf_h n       xs
            | otherwise     = "\'" ++ x ++ "\'"                 ++ pf_h n       xs
       
genSpace :: Int -> String
genSpace n 
        | n<=0          = ""
        | otherwise     = " " ++ genSpace (n-1)
            
-- Good luck to you, friend and colleague!


