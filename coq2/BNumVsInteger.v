From LPP Require Import PositiveNum.
From LPP Require Import PositiveNumVsInteger.
From LPP Require Import BNumFuncApp.


(** **** Conversioni. *)
Definition bN2Int (b: BNumFuncApp.BN) : nat :=
match b with
|  N0  _    => 0
| Npos _ b' => pos2Nat b'
end .

Definition int2BN (n: nat) : BNumFuncApp.BN := 
  fromInteger n.

