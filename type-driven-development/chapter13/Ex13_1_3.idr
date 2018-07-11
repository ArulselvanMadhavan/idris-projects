module Ex13_1_3

data Matter = Solid | Liquid | Gas

data MatterCmd : Type -> (inState : Matter) -> (outState : Matter) -> Type where
  Melt : MatterCmd () Solid Liquid
  Boil : MatterCmd () Liquid Gas
  Condense : MatterCmd () Gas Liquid
  Freeze : MatterCmd () Liquid Solid
  (>>=) : MatterCmd a c n -> (a -> MatterCmd b n f) -> MatterCmd b c f

iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt
