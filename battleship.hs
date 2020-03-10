{-# LANGUAGE MultiWayIf #-}

import Data.Char (ord)
import Data.List (permutations)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.List (minimumBy)
import Data.List (intersect)

newtype ShipLength = ShipLength Int
newtype Amount = Amount Int
newtype ShipName = ShipName String

type Point = (Int, Int)
type ShipPoints = [Point]
type Board = [[Char]]

data Ship = Ship Amount ShipLength ShipName

carrier :: Ship
carrier = Ship (Amount 1) (ShipLength 5) (ShipName "carrier")

battleship :: Ship
battleship = Ship (Amount 1) (ShipLength 4) (ShipName "battleship")

cruiser :: Ship
cruiser = Ship (Amount 2) (ShipLength 3) (ShipName "cruiser")

destroyer :: Ship
destroyer = Ship (Amount 1) (ShipLength 2) (ShipName "destroyer")

submarine :: Ship
submarine = Ship (Amount 1) (ShipLength 1) (ShipName "submarine")

getShipAmount :: Ship -> Amount
getShipAmount (Ship ship_amount _ _) = ship_amount

getShipLength :: Ship -> ShipLength
getShipLength (Ship _ ship_length _) = ship_length

getShipName :: Ship -> ShipName
getShipName (Ship _ _ ship_name) = ship_name

shipNameToString :: ShipName -> String
shipNameToString (ShipName i) = i

shipLengthToInt :: ShipLength -> Int
shipLengthToInt (ShipLength i) = i

fieldSize = 10
totalShipsAmount = 6

initializeBoard :: Board
initializeBoard = take fieldSize (repeat (replicate fieldSize '#'))


convertToCoordinates :: String -> Point
convertToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertToCoordinates _ = (-1, -1)

splitCoordinatePairsToString :: String -> [String]
splitCoordinatePairsToString [] = [[]]
splitCoordinatePairsToString (x:xs) = if x == ';' then
     [] : splitCoordinatePairsToString xs
    else 
        (x: head (splitCoordinatePairsToString xs)) : tail (splitCoordinatePairsToString xs)

enoughCoordinates :: ShipLength -> ShipPoints -> Bool
enoughCoordinates len shipCoordinates
 | shipLengthToInt len /= length shipCoordinates = False
 | otherwise = True

coordinatesWithinBoard :: Point -> Bool    
coordinatesWithinBoard shipCoordinate = 
    and [
        fst shipCoordinate >= 1,
        snd shipCoordinate >= 1,
        fst shipCoordinate <= fieldSize,
        snd shipCoordinate <= fieldSize
    ]

overlappingCoords :: (Eq a) => [a] -> [a] -> Bool      
overlappingCoords a = not . null . intersect a

isHorizontalNeighbourCoords :: ShipPoints  -> ShipLength -> Bool
isHorizontalNeighbourCoords shipCoordinates len 
 |  fst (maximumBy (comparing fst) shipCoordinates) - fst (minimumBy (comparing fst) shipCoordinates) + 1 /= (shipLengthToInt len) || 
    snd (maximumBy (comparing snd) shipCoordinates) - snd (minimumBy (comparing snd) shipCoordinates) /= 0 = False
 | otherwise = True

isVerticalNeighbourCoords :: ShipPoints -> ShipLength -> Bool
isVerticalNeighbourCoords shipCoordinates len
 | snd (maximumBy (comparing snd) shipCoordinates) - snd (minimumBy (comparing snd) shipCoordinates) + 1 /= (shipLengthToInt len) ||
   fst (maximumBy (comparing fst) shipCoordinates) - fst (minimumBy (comparing fst) shipCoordinates) /= 0 = False
 | otherwise = True

validateGivenCoordinates :: ShipLength -> ShipName -> ShipPoints -> [ShipPoints] -> Bool
validateGivenCoordinates len shipname shipCoordinates placedShips
 | enoughCoordinates len shipCoordinates == False = False                                               --Check there is not too much or too less coordinatepairs                             
 | not (and [coordinatesWithinBoard shipCoordinate | shipCoordinate <- shipCoordinates]) = False        --Check coordinates are inside board
 | or ([overlappingCoords shipCoordinates shipsCoords | shipsCoords <- placedShips])  = False           --Check coordinates don't overlap
 | and ([isHorizontalNeighbourCoords shipCoordinates len]) = True                                       --Check coordinates are horizontally neighbours
 | and ([isVerticalNeighbourCoords shipCoordinates len]) = True                                         --Check coordinates are vertically neighbours                       
 | otherwise = False

                                                                    
setShip :: Amount -> ShipLength -> ShipName -> [ShipPoints] -> IO ShipPoints
setShip amount len shipname placedShips = do
    putStrLn ("   Enter coordinates for your " ++ shipNameToString shipname ++ " (" ++ show (shipLengthToInt len) ++ " set of coordinates):")
    shipAllCoordinatesString <- getLine
    let stringCoordinates = splitCoordinatePairsToString shipAllCoordinatesString
    let shipCoordinates = map convertToCoordinates stringCoordinates
    if validateGivenCoordinates len shipname shipCoordinates placedShips
        then return shipCoordinates
        else
            setShip amount len shipname placedShips
    

setShips :: [ShipPoints] -> IO [ShipPoints]
setShips placedShips = do
    carrier_Ship <- setShip (getShipAmount carrier) (getShipLength carrier) (getShipName carrier) []
    let a = carrier_Ship : placedShips 
    battle_Ship <- setShip (getShipAmount battleship) (getShipLength battleship) (getShipName battleship) a
    let b = battle_Ship : a
    cruiser_Ship <- setShip (getShipAmount cruiser) (getShipLength cruiser) (getShipName cruiser) b
    let c = cruiser_Ship : b
    cruiser_Ship2 <- setShip (getShipAmount cruiser) (getShipLength cruiser) (getShipName cruiser) c 
    let d = cruiser_Ship : c
    destroyer_Ship <- setShip (getShipAmount destroyer) (getShipLength destroyer) (getShipName destroyer) d 
    let e = destroyer_Ship : d
    submarine_Ship <- setShip (getShipAmount submarine) (getShipLength submarine) (getShipName submarine) e
    let f = submarine_Ship : e

    return (f)

fireToCoordinate :: String -> Point -> Board -> [ShipPoints] -> IO([ShipPoints])
fireToCoordinate playerName fireToCoordinate playerBoard playerShips = do
    putStrLn (playerName ++ "'s " ++ "turn to shoot (insert one coordinate pair)")
    fireCoordinate <- getLine
    if validateGivenCoordinates fireCoordinate then
        


playGame :: String -> String -> Board -> Board -> [ShipPoints] -> [ShipPoints] -> IO ()
playGame player1 player2 player1Board player2Board player1Ships player2Ships = do
    player2CurrentShipList <- fireToCoordinate player1 player2Board player2Ships               --Player 1 firing turn
    player1CurrentShipList <- fireToCoordinate player2 player1Board player1Ships               --Player 2 firing turn
    if | length player2CurrentShipList == 0 -> putStrLn(player1 ++ " has won!")
       | length player1CurrentShipList == 0 -> putStrLn(player2 ++ " has won!")
       | otherwise -> playGame player1 player2 player1Board player2Board player1CurrentShipList player2CurrentShipList


    fireCoordinate <- getLine
    convertToCoordinates fireCoordinate
    if coordinatesWithinBoard fireCoordinate then
        (newBoard, newShipList) <- fireToCoordinate fireCoordinate player2Board player2Ships


askNames :: IO (String, String)
askNames = do
            putStrLn "Enter player 1 name:"
            player1 <- getLine
            putStrLn "Enter player 2 name:"
            player2 <- getLine
            return (player1,player2)

main :: IO ()
main = do
    (player1,player2) <- askNames
    putStrLn (player1 ++ "'s turn to place ships")
    player1Ships <- setShips []

    putStrLn (player2 ++ "'s turn to place ships")
    player2Ships <- setShips []

    playGame player1 player2 initializeBoard initializeBoard player1Ships player2Ships


