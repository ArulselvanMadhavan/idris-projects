module Vending

VendState : Type
VendState = (Nat, Nat)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data Fuel = Empty | More (Inf Fuel)

forever : Fuel
forever = More forever

data MachineCmd : Type -> (current: VendState) -> (next : VendState) -> Type where
     InsertCoin : MachineCmd () (pounds, chocs) (S pounds, chocs)
     Vend : MachineCmd () (pounds, S chocs) (pounds, chocs)
     GetCoins : MachineCmd () (pounds, chocs) (Z, chocs)
     Refill : (bars : Nat) -> MachineCmd () (Z, chocs) (Z, bars + chocs)
     Display : String -> MachineCmd () state state
     GetInput : MachineCmd (Maybe Input) state state
     Pure : ty -> MachineCmd ty state state
     (>>=) : MachineCmd a state1 state2 -> (a -> MachineCmd b state2 state3) -> MachineCmd b state1 state3

data MachineIO : VendState -> Type where
     Do : MachineCmd a state1 state2 -> (a -> Inf (MachineIO state2)) -> MachineIO state1

namespace MachineDo
  (>>=) : MachineCmd a state1 state2 -> (a -> Inf (MachineIO state2)) -> MachineIO state1
  (>>=) = Do

mutual
  vend : MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c}= do Vend
                                        Display "Enjoy!"
                                        machineLoop

  vend {pounds = Z} = do Display "Insert a coin"
                         machineLoop

  vend {chocs = Z} = do Display "Out of Stock"
                        machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do Refill num
                               machineLoop
  refill _ = do Display "Can't refill: Coins in machine"
                machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop = do Just x <- GetInput | Nothing => do Display "Invalid input"
                                                      machineLoop
                   case x of
                        COIN => do InsertCoin
                                   machineLoop
                        VEND => vend
                        CHANGE => do GetCoins
                                     machineLoop
                        (REFILL k) => refill k

  readInput : String -> Maybe Input
  readInput "coin" = Just COIN
  readInput "vend" = Just VEND
  readInput "refill" = Just (REFILL 10) -- TODO remove 10
  readInput "change" = Just CHANGE
  readInput _ = Nothing

  runCommand : MachineCmd a state1 state2 -> IO a
  runCommand GetInput = do x <- getLine
                           pure (readInput x)
  runCommand (Pure x) = do pure x
  runCommand (x >>= f) = do res <- runCommand x
                            runCommand (f res)

  run : (fuel : Fuel) -> MachineIO (pounds, chocs) -> IO VendState
  run {pounds = pounds} {chocs = chocs} Empty (Do x f) = do pure (pounds, chocs)
  run (More y) (Do x f) = do res <- runCommand x
                             run y ?finalParam

  main : IO ()
  main = do (_, result) <- run forever machineLoop {pounds = Z} {chocs = Z}
            putStrLn (show result)
