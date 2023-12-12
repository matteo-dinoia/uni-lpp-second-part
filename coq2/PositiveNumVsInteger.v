
From LPP Require Import PositiveNum.

Require Import Arith.
Require Import Nat.
Require Import Lia.

(** ** Conversioni tra [Positive] e [nat] *)
Fixpoint pos2Nat (p: PositiveNum.Positive) : nat :=
match p with
| PositiveNum.XH   => 1
| PositiveNum.XO p' => 2 * pos2Nat p'
| PositiveNum.XI p' => 1 + 2 * pos2Nat p'
end .

Fixpoint nat2Pos (n: nat) : PositiveNum.Positive :=
match n with
| O    => PositiveNum.XH 
| S O  => PositiveNum.XH (* base *)
| S n' => PositiveNum.succ (nat2Pos n')
end .


(** **** Test *)
(* 
Example nat2PosO: nat2Pos O = PositiveNum.XH.
Proof. auto. Qed.
Example nat2Pos1: nat2Pos 1 = PositiveNum.XH.
Proof. auto. Qed.
Example nat2Pos2: nat2Pos 2 = PositiveNum.XO PositiveNum.XH.
Proof. unfold nat2Pos. unfold even. reflexivity. Qed.
Example nat2Pos3: nat2Pos 3 = PositiveNum.XI PositiveNum.XH.
Proof. unfold nat2Pos. unfold even. reflexivity. Qed.
Example nat2Pos4: nat2Pos 4 = PositiveNum.XO (PositiveNum.XO PositiveNum.XH).
Proof. unfold nat2Pos. unfold even. simpl. reflexivity. Qed.
Example nat2Pos5: nat2Pos 5 = PositiveNum.XI (PositiveNum.XO PositiveNum.XH).
Proof. unfold nat2Pos. unfold even. simpl. reflexivity. Qed.
Example nat2Pos6: nat2Pos 6 = PositiveNum.XO (PositiveNum.XI PositiveNum.XH).
Proof. auto. Qed.
Example nat2Pos7: nat2Pos 7 = PositiveNum.XI (PositiveNum.XI PositiveNum.XH).
Proof. auto. Qed. 
*)
(* 
Example pos2Nat1: pos2Nat PositiveNum.XH = 1.
Proof. auto. Qed.
Example pos2Nat2: pos2Nat (PositiveNum.XO PositiveNum.XH) = 2.
Proof. auto. Qed.
Example pos2Nat3: pos2Nat (PositiveNum.XI PositiveNum.XH) = 3.
Proof. auto. Qed.
Example pos2Nat4: pos2Nat (PositiveNum.XO (PositiveNum.XO PositiveNum.XH)) = 4.
Proof. auto. Qed. *)