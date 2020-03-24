module Shiptypes
(   
    carrier
,   battleship
,   cruiser
,   destroyer
,   submarine
,   getShipAmount
,   getShipLength
,   getShipName
,   shipNameToString
,   shipLengthToInt 
,   Ship
,   ShipLength
,   Amount
,   ShipName
,   Point
,   ShipPoints
) where

data Ship = Ship Amount ShipLength ShipName

newtype ShipLength = ShipLength Int
newtype Amount = Amount Int
newtype ShipName = ShipName String

type Point = (Int, Int)
type ShipPoints = [Point]

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