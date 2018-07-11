module Ex13_1_1

data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type -> (inState : DoorState) -> (outState : DoorState) -> Type where
  Open  : DoorCmd () DoorClosed DoorOpen
  Close : DoorCmd () DoorOpen DoorClosed
  RingBell : DoorCmd () inState outState
  Pure : ty -> DoorCmd () DoorClosed DoorClosed
  (>>=) : DoorCmd a st1 st2 -> (a -> DoorCmd b st2 st3) -> DoorCmd b st1 st3

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do RingBell
              Open
              RingBell
              Close
