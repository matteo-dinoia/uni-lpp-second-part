From LPP Require Import PositiveNum.
From LPP Require Import PositiveNumVsInteger.

Module PositiveNumProperties00.

(*************)
(** * INDICE *)
(** Le prime due sezioni contengono una carrellata di
proprietà che offrono l'occasione di introdurre un
insieme rielvante di tattiche.

  - intro, intros
  - reflexivity
  - destruct, destruc ... as (... , ... )
  - simpl, simpl in
  - inversion
  - unfold, unfold in
  - fold, fold in
  - rewrite, rewrite <-
  - inducion

L'ultima sezione offre alcuni esercizi.
*)

(*********************************************** *)
(** ** Proprietà di [succ] relative a [Positive] *)

Lemma xI_succ_xO: forall (p: PositiveNum.Positive),
  PositiveNum.XI p = PositiveNum.succ (PositiveNum.XO p).
Proof. intros. simpl. reflexivity.
Qed.

(** Per casi.
    - [discriminate]: elimina goal che dipendono da assunzioni
    palesemente false, grazie al fatto che costruttori diversi
    generano elementi diversi, qundi è impossibile che siani
    uguali
    - [inversion]: come [discriminate], ma può produrre nuove
    assunzioni, più essenziali, da cui può dipendere il goal
    da dimostrare.
*)
Lemma succ_discr: forall (p: PositiveNum.Positive),
  p <> PositiveNum.succ p.
Proof. intros. destruct p.
  - simpl. unfold not. discriminate.
  - simpl. unfold not. discriminate.
  - simpl. unfold not. discriminate.
Qed.

(** *** Esempi paradigmatici di uso della tattica [inversion] *)
Inductive day : Type :=
  | monday
  | tuesday
  | wednesday .

(** **** Giorni della settimana *)
Definition next_weekday (d:day) : day :=
  match d with
  | monday => tuesday
  | tuesday => wednesday
  | wednesday => monday
  end.

(** Per casi.
    - Ricordare che [x <> y] è [not (x = y)], cioè [x = y -> False].
    Quindi, di fronte a [x <> y] può servire [unfold not].
    - utile [inversion].
    *)
Theorem next_weekday_not_same : forall (d: day),
  next_weekday d <> d.
Proof.
  intros. unfold not. destruct d; discriminate.
Qed.

(** - [inversion] per semplificare una assunzione secondo la quale
    due termini ottenuti applicando un successore [S] sono identici.
    - Ragionamento forward tramite [apply] usata su assunzione
*)
Theorem not_eq_SnSm_nm : forall n m : nat,
  S n = S m -> n <> m -> False.
Proof. unfold not.
Admitted.

(** - [destruct] per scomporre la congiunzione in un'assunzione
    - [rewrite] per trasformare un termine in un altro, essendo nota una equivalenza *)
Theorem not_eq_with_and : forall n m : nat,
  S n = S m /\ n <> m -> False.
Proof.
Admitted.


(*********************************** *)
(** ** La funzione [succ] e "doppio" *)

(** *** Predecessore del doppio di [x] ([x -> 2*x-1]) *)
Fixpoint pred_of_double x :=
  match x with
    | PositiveNum.XI p => PositiveNum.XI (PositiveNum.XO p)
    | PositiveNum.XO p => PositiveNum.XI (pred_of_double p)
    |               XI => XI
  end.

(** - [induction]
    - [rewrite] dell'ipotesi induttiva *)
Lemma pred_double_specification: forall (p: PositiveNum.Positive),
  pred_of_double p = PositiveNum.pred (PositiveNum.XO p).
Admitted.

(******************** *)
(** ** ESERCIZI       *)

Lemma succ_pred_of_double: forall (p: PositiveNum.Positive),
  PositiveNum.succ (pred_of_double p) = PositiveNum.XO p.
Admitted.

Lemma pred_of_double_succ: forall (p: PositiveNum.Positive),
  pred_of_double (PositiveNum.succ p) = PositiveNum.XI p.
Admitted.

Lemma double_of_succ: forall (p: PositiveNum.Positive),
  PositiveNum.XO (PositiveNum.succ p) = PositiveNum.succ (PositiveNum.succ (PositiveNum.XO p)).
Admitted.

Lemma pred_of_double_xO_discr: forall (p: PositiveNum.Positive),
  pred_of_double p <> PositiveNum.XO p.
Admitted.

Lemma succ_not_1: forall (p: PositiveNum.Positive),
  PositiveNum.succ p <> PositiveNum.XH.
Admitted.
(** [] *)

End PositiveNumProperties00.
