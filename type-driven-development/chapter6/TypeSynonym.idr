module TypeSynonym

import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(0.0, 0.1), (0.2, 0.3), (0.4, 0.5)]