From LPP Require Import PositiveNum.
From LPP Require Import PositiveNumProperties00.
From LPP Require Import BNumVsInteger.

Lemma xI_succ_xO: forall (p: PositiveNum.Positive),
  BNumFuncApp.Npos _ (PositiveNum.XI p) =
      BNumFuncApp.succ (BNumFuncApp.Npos _ ( PositiveNum.XO p)).
Proof.
  simpl. reflexivity.
Qed.

(* [x |--> 2x + 1]  *)
Definition succ_of_double (b: BNumFuncApp.BN) : BNumFuncApp.BN :=
match b with
| BNumFuncApp.N0 _ => BNumFuncApp.Npos _ PositiveNum.XH
| BNumFuncApp.Npos _ b' => BNumFuncApp.Npos _ (PositiveNum.XI b')
end .

Lemma bN2Int_succ_of_double: forall a : BNumFuncApp.BN,
  bN2Int (succ_of_double a) = S (2 * bN2Int a).
Proof. induction a.
- simpl. reflexivity.
- unfold succ_of_double. simpl. reflexivity.
Qed.

(** Esempi della proprietà "simmetrica" di [bN2Int_succ_of_double] *)
Example succ_of_double_and_int2BN_0:
succ_of_double (int2BN 0) = int2BN (S (2 * 0)).
Proof. auto. Qed.
Example succ_of_double_and_int2BN_1:
succ_of_double (int2BN 1) = int2BN (S (2 * 1)).
Proof. simpl. reflexivity. Qed.
Example succ_of_double_and_int2BN_2:
succ_of_double (int2BN 2) = int2BN (S (2 * 2)).
Proof. simpl. reflexivity. Qed.
Example succ_of_double_and_int2BN_3:
succ_of_double (int2BN 3) = int2BN (S (2 * 3)).
Proof. simpl. reflexivity. Qed.

(** [succ_of_double_and_int2BN] "simmetrica" di [bN2Int_succ_of_double] *)
(** Molto più complessa *)
Lemma succ_of_double_and_int2BN: forall n : nat,
succ_of_double (int2BN n) = int2BN (S (2 * n)).
Proof.
induction n as [ | n' IH].
- simpl. reflexivity.
- unfold int2BN in IH.
  fold int2BN in IH.
  destruct n'.
  + simpl. reflexivity.
  + unfold int2BN.
    fold int2BN.
    unfold BNumFuncApp.fromInteger.
    unfold BNumFuncApp.fromInteger in IH.
    simpl. simpl in IH.
    rewrite PeanoNat.Nat.add_0_r. (* n + 0 = n *)
    rewrite PeanoNat.Nat.add_0_r in IH. (* n + 0 = n *)
    repeat rewrite PeanoNat.Nat.add_succ_r.  (* n + S m = S (n + m) *)
    repeat rewrite PeanoNat.Nat.add_succ_r in IH.  (* n + S m = S (n + m) *)
    simpl. simpl in IH.
Admitted.

(** Altre difficoltà, ad esempio relative a proprietà delle funzioni anche più semplici  *)
Lemma succ_discr: forall (b: BNumFuncApp.BN),
  b <> BNumFuncApp.succ b.
Proof.
(* Dimostrazione per casi. *)
destruct b.
- unfold not. (* ricodare che [A <> B] è [A = B -> False]*)
  simpl.
  discriminate. (* tattica che "scarta" goal dipendenti da
                   assunzioni palesemente false, come conseguenza
                   della iniettività dei costruttori di tipi induttivi *)
- unfold not.
  intro H.
  inversion H. (* simile a [discriminate], ma le assunzioni dei goal
                  rimanenti possono essere semplificate.               *)
  generalize H1. (* "freccia" introduzione *)
  apply PositiveNumProperties00.succ_discr. (* dimostratato altrove ... *)
Qed.
