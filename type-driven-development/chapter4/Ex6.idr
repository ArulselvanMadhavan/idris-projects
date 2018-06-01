module Ex6

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = ?biggestTriangle_rhs_1
biggestTriangle (Combine x y) = ?biggestTriangle_rhs_2
biggestTriangle (Rotate x y) = ?biggestTriangle_rhs_3
biggestTriangle (Translate x y z) = ?biggestTriangle_rhs_4
