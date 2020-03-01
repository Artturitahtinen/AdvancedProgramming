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

fieldSize = 10
totalShipsAmount = 6

initializeBoard :: Board
initializeBoard = take fieldSize (repeat (replicate fieldSize '#'))


convertToCoordinates :: String -> Point
convertToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertToCoordinates _ = (-1, -1)

splitCoordinatePairsToString :: String -> [String]
splitCoordinatePairsToString [] = [[]]
splitCoordinatePairsToString (x:xs) = if x ==";" then
     [] : splitCoordinatePairsToString
    else 
        (x: head (splitCoordinatePairsToString xs)) : tail (splitCoordinatePairsToString xs)

                                                                                
setCarrierShip :: Int -> Int -> IO ShipPoint
setCarrierShip amount len = do
    putStrLn ("   Enter coordinates for your " ++ getShipName carrier ++ " (" ++ len ++ " set of coordinates):")
    carrierStr <- getLine
    let stringCoordinates = splitCoordinatePairsToString carrierStr
    let coordinates = map convertToCoordinates stringCoordinates
    return coordinates

setShips :: [ShipPoints] -> IO [ShipPoints]
setShips placedShips = if placedShips <= totalShipsAmount then 
    do
    carrierShip <- setCarrierShip (getShipAmount carrier) (getShipLength carrier)
    listOfShips <- setShips (placedShips + 1)
    return (ship : listOfShips)

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
