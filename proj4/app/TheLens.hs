-- | Working with Lenses

{-# LANGUAGE TemplateHaskell #-}

module TheLens where

import Control.Lens
import Control.Lens.TH

data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Eq,Show)

data Figure
  = Circle
    { _origin :: Point
    , _radius :: Double
    }
  | Polygon
    { _origin :: Point
    , _points :: [Point]
    }
  deriving (Eq,Show)

makeLenses ''Point

makeLenses ''Figure
makePrisms ''Figure

pt = Point

c1 = Circle (pt 1 2) 3
p2 = Polygon (pt 4 5) [pt 6 7, pt 8 9, pt 10 11]

figures =
  [ c1
  , p2
  , Polygon (pt 11 12) [pt 13 14, pt 15 16, pt 17 18]
  ]

-- lenses

-- view value
r1 = c1 ^. origin
r2 = c1 ^. origin . x
r3 = view (origin . x) p2
-- set value
r4 = p2 & origin . x .~ 5
r5 = set (origin . x) 5 p2
-- apply function over value
r6 = p2 & origin . x %~ (^2)
r7 = over (origin . x) (^2) p2
r8 = p2 & origin . x %~ (+2)
r9 = p2 & origin . x +~ 2

-- prisms

-- preview value
r10 = c1 ^? _Circle . _1 . x
r11 = preview (_Circle . _1 . x) c1
r12 = figures ^? ix 0
r13 = figures ^? ix 9

-- change works
r20 = figures & ix 0 . _Circle . _1 . x .~ 5.0
r21 = figures & ix 1 . _Circle . _1 . x .~ 5.0
r22 = figures & ix 0 . radius .~ 5.0
r23 = figures & ix 1 . radius .~ 5.0

-- folds

r14 = figures ^.. folded . origin
r15 = p2 ^.. points . folded . x
r16 = toListOf (folded . origin) figures
r17 = figures ^.. folded . points . folded . x

-- traverses

r18 = figures & traverse . origin .~ pt 0 0
r19 = figures & traverse . points . traverse . x +~ 100

------------------------------------

-- сдвигаем фигуру в точку (x0,y0)
move1 f x0 y0 = f & origin .~ pt x0 y0

move2 f x0 y0 = f & origin . x .~ x0
                  & origin . y .~ y0

-- увеличиваем радиус круга в k раз
expandCircle f k = f & radius *~ k

-- сдвигаем круг на (dx,dy)
moveCircle f dx dy = f
  & g . x +~ dx
  & g . y +~ dy
  where g = _Circle . _1

data Segment = Segment
  { _beg :: Point
  , _end :: Point
  } deriving (Eq,Show)

makeLenses ''Segment

segments =
  [ Segment (pt 0 0) (pt 100 0)
  , Segment (pt 100 0) (pt 100 100)
  , Segment (pt 100 100) (pt 0 0)
  ]

-- сдвигаем отрезки на d
moveSegments1 s d = s
  & traverse . beg . x +~ d
  & traverse . beg . y +~ d
  & traverse . end . x +~ d
  & traverse . end . y +~ d

-- сдвигаем отрезки на d,
-- используя линзы как первоклассные
-- значения в языке
moveSegments2 s d = foldr ($) s
  [ over (traverse . seg . coord) (+d)
  | seg <- [beg, end]
  , coord <- [x, y]
  ]
