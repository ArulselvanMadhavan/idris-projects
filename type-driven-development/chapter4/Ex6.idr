module Ex6

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

area : (x : Shape) -> Maybe Double
area (Triangle x y) = Just $ 0.5 * x * y
area (Rectangle x y) = Nothing
area (Circle x) = Nothing

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = area x
biggestTriangle (Combine x y) = max (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

-- pictureArea : Picture -> Double
-- pictureArea (Primitive x) = area x
-- pictureArea (Combine x y) = pictureArea x + pictureArea y
-- pictureArea (Rotate x pic) = pictureArea pic
-- pictureArea (Translate x y pic) = pictureArea pic

-- testPicture : Picture
-- testPicture = Combine (Translate 5 5 ?rect)
--               (Combine (Translate 35 5 ?circ)
--               (Translate 15 25 ?tri))
