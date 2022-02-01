
module Shapes 
(Point(..)
, Shape(..)
, Shape'(..)
, surface
, surface'
, move
, baseRect
, baseCircle
) where

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

--function takes a Shape and returns the surface
-- pattern matching can be used on value constructors
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2 --value constructors are functions 
surface (Rectangle x1 y1 x2 y2)  = (abs $ x2 - x1) * (abs $ y2 - y1)

-- using a map (takes a function and a list and applies the function to every member of the list to produce a list) and partial function application,  we can make a list of concentric circles
result = map (Circle 12 5) [2,3,4,5]
--[Circle 12.0 5.0 2.0,Circle 12.0 5.0 3.0,Circle 12.0 5.0 4.0,Circle 12.0 5.0 5.0]

-- refactor Shape to use Point types
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

--refactor the surface function
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2 
surface' (Rectangle' (Point x1 y1) (Point x2 y2))  = (abs $ x2 - x1) * (abs $ y2 - y1)
result2 = surface' $ Circle' (Point 3 7 ) 5
--78.53982

-- moving the shape
-- function takes the shape, amount to move in x-axis, amount to move in y-axis and returns a new shape located in the new space.
move :: Shape' -> Float -> Float -> Shape'
move (Circle' (Point x y) r ) a b = Circle' (Point (x+a) (y+b)) r
move (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- move (Rectangle' (Point 0 1) (Point 0 7)) 5 9
-- Rectangle' (Point 5.0 10.0) (Point 5.0 16.0)

-- abstracting points to a separate function so we do not have to use them creating shape. The base shapes start at point 0 0 on the x,y axis and move can be called on those points
baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point 0 0) (Point width height)

newShapeLocation = move (baseRect 30 70) 100 200
