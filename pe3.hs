module PE3 where

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)

-------------------------------------------------------------------------------------------
--------------------------------- DO NOT CHANGE ABOVE -------------------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------
-------------------------------------- PART I ---------------------------------------------

isInGrid :: Grid -> Coordinate -> Bool
isInGrid grid (x,y) 
            | x >= 0 && x < xDim grid && y >= 0 && y < yDim grid    =True
            | otherwise                                             =False

-------------------------------------------------------------------------------------------

totalCount :: Grid -> Int
totalCount grid = totalCount_h (flatGrid grid)

-------------------------------------------------------------------------------------------

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits grid = qSort $ coordinatesOfPits_h (flatGrid grid) 0 (xDim grid) (yDim grid)

-------------------------------------------------------------------------------------------

tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath _ _ [] = []
tracePath grid (Robot nam loc cap ene sto) (mov:moves)
            
            | ene == 0                                                                              = loc : tracePath grid (Robot nam loc cap ene sto) moves
            
            | aPit loc grid && (mov == North || mov == South || mov == West || mov == East)         = loc : tracePath grid (Robot nam loc cap (max 0 (ene-1)) sto) moves

            | mov == North && isInGrid grid nCoord                                                  = nCoord : tracePath grid (Robot nam nCoord cap (ene -1) sto) moves
            | mov == South && isInGrid grid sCoord                                                  = sCoord : tracePath grid (Robot nam sCoord cap (ene -1) sto) moves
            | mov == East  && isInGrid grid eCoord                                                  = eCoord : tracePath grid (Robot nam eCoord cap (ene -1) sto) moves
            | mov == West  && isInGrid grid wCoord                                                  = wCoord : tracePath grid (Robot nam wCoord cap (ene -1) sto) moves            
            
            | mov == PutDown                                                                        = loc : tracePath grid (Robot nam loc cap (max 0 (ene-3)) sto) moves
            | mov == PickUp                                                                         = loc : tracePath grid (Robot nam loc cap (max 0 (ene-5)) sto) moves            
            
            where
                nCoord = (fst loc, (snd loc) -1)
                sCoord = (fst loc, (snd loc) +1)
                wCoord = ((fst loc)-1, snd loc)
                eCoord = ((fst loc)+1, snd loc)

------------------------------------- PART II ----------------------------------------------

energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots _ [] = []
energiseRobots grid (rb:rbs) = (energiseRobot rb scCoords ) : energiseRobots grid rbs
            where
                scCoords = coordinatesOfSC (flatGrid grid) 0 (xDim grid) (yDim grid)

-------------------------------------------------------------------------------------------

applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot [] = (grid, robot)
applyMoves grid robot (mov:moves) = applyMoves grid1 robot1 moves
            where (grid1, robot1) = applyMove grid robot mov



---------------------------------Helper Functions------------------------------------------
--------TODO Prepare a documentation for the solution. Try to simplify applyMoves.---------

applyMove :: Grid -> Robot -> Move -> (Grid, Robot)
applyMove grid (Robot nam loc cap ene sto) mov
            | ene == 0                                                                              = (grid, Robot nam loc cap ene sto)
            | aPit loc grid && (mov == North || mov == South || mov == West || mov == East)         = (grid, Robot nam loc cap (max 0 (ene-1)) sto)
            
            | aPit loc grid && mov == PutDown                                   = (grid, Robot nam loc cap (max 0 (ene-3)) sto)
            | aPit loc grid && mov == PickUp                                    = (grid, Robot nam loc cap (max 0 (ene-5)) sto)
            
            | mov == North && isInGrid grid nCoord                              = (grid, Robot nam nCoord cap (ene -1) sto)
            | mov == South && isInGrid grid sCoord                              = (grid, Robot nam sCoord cap (ene -1) sto)
            | mov == East  && isInGrid grid eCoord                              = (grid, Robot nam eCoord cap (ene -1) sto)
            | mov == West  && isInGrid grid wCoord                              = (grid, Robot nam wCoord cap (ene -1) sto)

            | mov == PutDown && ene >=3 && sto > 0                              = (putDownUpdate grid sto, Robot nam loc cap (max 0 (ene-3)) (sto-1) )
            | mov == PutDown && ene >=3 && sto <=0                              = (grid, Robot nam loc cap (ene-3) sto)
            | mov == PutDown && ene < 3                                         = (grid, Robot nam loc cap (max 0 (ene-3)) sto )
            | mov == PickUp &&  ene >=5 && cap > sto                            = (pickUpUpdate grid loc 0 0 , Robot nam loc cap  (ene-5) (sto+1) )
            | mov == PickUp && ene >=5 && cap <= sto                            = (grid, Robot nam loc cap (ene-5) sto)
            | mov == PickUp && ene < 5                                          = (grid, Robot nam loc cap (max 0 (ene-5)) sto)
            
            | otherwise                                                         = (grid, Robot nam loc cap ene sto)

            where
                nCoord = (fst loc, (snd loc) -1)
                sCoord = (fst loc, (snd loc) +1)
                wCoord = ((fst loc)-1, snd loc)
                eCoord = ((fst loc)+1, snd loc)



putDownUpdate :: Grid -> Int -> Grid
putDownUpdate [] _ = []
putDownUpdate (xs:xss)  n = (cellUpdate_1 xs  n) : putDownUpdate xss n


cellUpdate_1 :: [Cell] -> Int -> [Cell]
cellUpdate_1 [] _ = []
cellUpdate_1 (SpaceCraft spc :xs)  n    = (SpaceCraft (1+spc)) : cellUpdate_1 xs n
cellUpdate_1 (x:xs) n                   = x:cellUpdate_1 xs n  

pickUpUpdate :: Grid -> Coordinate -> Int -> Int-> Grid
pickUpUpdate [] _ _ _ = []
pickUpUpdate (xs:xss) loc xd yd  = (cellUpdate_2 xs loc 0 yd ) : pickUpUpdate xss loc 0 (yd +1)


cellUpdate_2 :: [Cell] -> Coordinate -> Int-> Int-> [Cell]
cellUpdate_2 [] _ _ _ = []
cellUpdate_2 (Rock spc : xs) loc xd yd
                | fst(loc) == xd && snd(loc) ==yd         = (Rock (spc -1)) : cellUpdate_2 xs loc (xd+1) yd
                | otherwise             = (Rock spc) : cellUpdate_2 xs loc (xd+1) yd
cellUpdate_2 (x:xs) loc xd yd = x: cellUpdate_2 xs  loc (xd+1) yd


xDim :: Grid-> Int
xDim grid = length (grid!!0)

yDim :: Grid->Int
yDim grid = length (grid)

rocksInCell :: Cell -> Int
rocksInCell (Rock x)    = x
rocksInCell _           = 0


flatGrid :: Grid -> [Cell]
flatGrid gr = [x | xs<-gr, x<-xs]

totalCount_h :: [Cell] -> Int
totalCount_h [] = 0
totalCount_h (x:xs) = rocksInCell x + totalCount_h xs

coordinatesOfPits_h :: [Cell] -> Int -> Int -> Int -> [Coordinate]
coordinatesOfPits_h [] _ _ _ = []
coordinatesOfPits_h (x:xs) n xd yd
            | x == Pit      = ((mod n xd), (div n xd)) : coordinatesOfPits_h xs (n+1) xd yd
            | otherwise     = coordinatesOfPits_h xs (n+1) xd yd


qSort :: [Coordinate] -> [Coordinate]
qSort [] = []
qSort (x:xs) = qSort (leftList)   ++ [x] ++ qSort (rightList)
            where
                leftList  = [(f,s) | (f,s) <- xs, f <  (fst x)]
                rightList = [(f,s) | (f,s) <- xs, f >= (fst x)]

aPit :: Coordinate -> Grid -> Bool
aPit coord grid = (elem coord (coordinatesOfPits grid))


coordinatesOfSC :: [Cell] -> Int -> Int -> Int -> Coordinate
coordinatesOfSC [] _ _ _ = (0,0)
coordinatesOfSC (x:xs) n xd yd
                | elem x spaceCrafts =  ((mod n xd), (div n xd))
                | otherwise         = coordinatesOfSC xs (n+1) xd yd
                where
                    spaceCrafts = [SpaceCraft number | number <- [0..1000000]]


energiseRobot :: Robot -> Coordinate -> Robot
energiseRobot (Robot nam loc cap ene sto) (sx, sy) = (Robot nam loc cap newEne sto)
                where
                    newEne = min 100 (ene + max 0 (100 - (abs (fst(loc) - sx) + abs ( snd(loc) - sy))*20 ))













