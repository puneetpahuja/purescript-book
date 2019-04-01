module Data.Picture where

import Prelude

import Data.Foldable (foldl, length)
import Data.Maybe (Maybe(..))
import Global as Global
import Math (atan, cos, sin, sqrt)
import Math as Math


data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Shape

data Point = Point
  { x :: Number
  , y :: Number
  }

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point {x: 0.0, y: 0.0}

    p2 :: Point
    p2 = Point {x: 100.0, y: 50.0}

smallLine :: Shape
smallLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point {x: 1.0, y: 2.0}

    p2 :: Point
    p2 = Point {x: 4.0, y: 6.0}

exampleCircle :: Shape
exampleCircle = Circle c 10.0
  where
    c = Point {x: 0.0, y: 0.0}

showPoint :: Point -> String
showPoint (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r) = "Center: " <> showPoint c <> ", Radius: " <> show r
showShape (Rectangle c l w) = "Center: " <> showPoint c <> ", Length: " <> show l <> ", Width: " <> show w
showShape (Line p1 p2) = "Start: " <> showPoint p1 <> ", End: " <> showPoint p2
showShape (Text p m) = "Location: " <> showPoint p <> ", Message: " <> m
showShape (Clipped _ _) = "Clipped picture"

scaleDoubleAndCenter :: Shape -> Shape
scaleDoubleAndCenter (Circle _ r) = Circle (Point {x: 0.0, y: 0.0}) (2.0*r)
scaleDoubleAndCenter (Rectangle _ l w) = Rectangle (Point {x: 0.0, y: 0.0}) (2.0*l) (2.0*w)
scaleDoubleAndCenter (Line (Point {x: x1, y: y1}) (Point {x: x2, y: y2})) = 
  Line q1 q2
    where
      len = sqrt $ dx*dx + dy*dy
      dx = x2 - x1
      dy = y2 - y1
      theta = atan (dx / dy)
      newX = len * (cos theta)
      newY = len * (sin theta) 
      q1 = Point {x: newX, y: newY}
      q2 = Point {x: (- newX), y: (- newY)}
scaleDoubleAndCenter (Text _ m) = Text (Point {x: 0.0, y: 0.0}) m
scaleDoubleAndCenter (Clipped pic rec) = Clipped pic rec 

getText :: Shape -> Maybe String
getText (Text _ msg) = Just msg
getText _ = Nothing

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top :: Number
  , left :: Number 
  , bottom :: Number
  , right :: Number
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
    combine :: Bounds -> Shape -> Bounds
    combine b shape = union (shapeBounds shape) b

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped pic rec) = shapeBounds rec

area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ l w) = l * w
area (Line _ _) = 1.0
area (Text _ _) = 0.0
area (Clipped _ rec) = area rec

instance showShape' :: Show Shape where
  show = showShape