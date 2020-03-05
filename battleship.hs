import Data.Char (ord)
import Data.List (permutations)

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
splitCoordinatePairsToString (x:xs) = if x == ':' then
     [] : splitCoordinatePairsToString xs
    else 
        (x: head (splitCoordinatePairsToString xs)) : tail (splitCoordinatePairsToString xs)

enoughCoordinates :: ShipLength -> ShipPoints -> Bool
enoughCoordinates len shipCoordinates
 | shipLengthToInt len == length shipCoordinates = True
 | otherwise = False

coordinatesWithinBoard :: Point -> Bool    --Miten saisin tässä käytyä läpi ShipPoints-listan tuplet chekattua tässä läpi?
coordinatesWithinBoard shipCoords 
    | (and [
        fst shipCoords >= 1
      , snd shipCoords >= 1
      , fst shipCoords <= fieldSize
      , snd shipCoords <= fieldSize]) = True
    | otherwise = False  
-- noOverlappingCoords :: ShipPoints -> [ShipPoints] -> Bool
-- noOverlappingCoords shipCoordinates placedShips
--  | shipCoordinates /= placedShips = True
--  | otherwise = False  

-- loopShipPoints :: ShipPoints -> Point
-- loopShipPoints [] = (-1, -1)
-- loopShipPoints (x, y) : xs) = x : (loopShipPoints xs)

validateGivenCoordinates2 :: ShipLength -> ShipName -> ShipPoints -> [ShipPoints] -> Bool
validateGivenCoordinates2 len shipname shipCoordinates placedShips = if enoughCoordinates len shipCoordinates == True 
    && coordinatesWithinBoard shipCoordinates      
    then True
    else False

validateGivenCoordinates :: ShipLength -> ShipName -> ShipPoints -> [ShipPoints] -> Bool
validateGivenCoordinates len shipname shipCoordinates placedShips
    | length shipCoordinates /= shipLengthToInt len = False -- Check if ship was given enough coordinates
    | or [coord1 == coord2 | ship2 <- placedShips, coord1 <- shipCoordinates, coord2 <- ship2] = False -- The coordinates may not overlap with another ship
    | not (and [coordinatesWithinBoard coord | coord <- shipCoordinates]) = False -- Check if coordinates lie in the field
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- shipCoordinates, coord2 <- shipCoordinates]) -- Check if  coordinates are neighbors (vertical)
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- shipCoordinates, coord2 <- shipCoordinates]) * 3 == (shipLengthToInt len-1) * (shipLengthToInt len^2 + shipLengthToInt len)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- shipCoordinates, coord2 <- shipCoordinates]) -- Check if  coordinates are neighbors (horizontal)
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- shipCoordinates, coord2 <- shipCoordinates]) * 3 == (shipLengthToInt len-1) * (shipLengthToInt len^2 + shipLengthToInt len)
    | otherwise = False -- Coordinates are not on the same line

                                                                    
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

playGame :: String -> String -> IO ()
playGame player1 player2 = do
    putStrLn ("mo")


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

    playGame player1 player2


