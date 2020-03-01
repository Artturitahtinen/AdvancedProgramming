import Data.Char (ord)
import Data.List (permutations)

newtype ShipLength = ShipLength Int
newtype Amount = Amount Int

type Point = (Int, Int)
type ShipPoints = [Point]
type Board = [[Char]]

data Ship = Amount ShipLength name

carrier :: Ship
carrier = Ship 1 5 carrier

battleship :: Ship
battleship = Ship 1 4 "battleship"

cruiser :: Ship
cruiser = Ship 2 3 "cruiser"

destroyer :: Ship
destroyer = Ship 1 2 "destroyer"

submarine :: Ship
submarine = Ship 1 1 "submarine"

getShipAmount :: Ship -> Int
getShipLength (Ship ship_amount _ _) = ship_amount

getShipLength :: Ship -> Int
getShipLength (Ship _ ship_length _) = ship_length

getShipName :: Ship -> String
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
splitCoordinatePairsToString (x:xs) 
    | x == ";" = [] : splitCoordinatePairsToString
    | otherwise (x: head (splitCoordinatePairsToString xs)) : tail (splitCoordinatePairsToString xs)

                                                                                
setCarrierShip :: Int -> Int -> IO ShipPoints
setCarrierShip amount len = do
                               putStrLn ("   Enter coordinates for your " ++ getShipName carrier ++ " (" ++ len ++ " set of coordinates):")
                               carrierStr <- getLine
                               let stringCoordinates = splitCoordinatePairsToString carrierStr
                               let coordinates = map convertToCoordinates stringCoordinates
                               return coordinates




setShips :: [ShipPoints] -> IO [ShipPoints]
setShips placedShips = if placedShips <= totalShipsAmount then 
    do
    ship <- setCarrierShip (getShipAmount carrier) (getShipLength carrier)







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
