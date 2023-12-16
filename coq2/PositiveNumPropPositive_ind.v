From LPP Require Import PositiveNum.
From LPP Require Import PositiveNumVsInteger.
From LPP Require Import PositiveNumProperties00.
From Coq Require Import Lia.

Print PositiveNum.Positive_ind.

(** ******************************* *)
(** *** Proprietà [pos2Nat_nat2Pos] *)

(** Dimostraiamo che, almeno in un verso, il
successore su [PositiveNum.Positive] corrisponde
al successore su [nat].

La dimostrazione procede per induzione, ma si basa
sull'applicazione del principio di induzione derivato
automaticamente da Coq al momento della definizione
induttiva di [Positive]:
*)

Check PositiveNum.Positive_ind.
(** ==>
Positive_ind
	 : forall (P: Positive -> Prop),
       (forall (p: Positive), P p -> P (XI p)) ->
       (forall (p: Positive), P p -> P (XO p)) ->
       (P XH -> forall (p: Positive), P p)

scritto senza riferimenti al name space
[PositiveNum] e con qualche parentesi in più, per
identificare meglio le componenti.
*)

(** Per applicare [Positive_ind] si procede in accordo
    con i seguenti passi:
    - Stabilire il predicato [P] per cui dimostrare
    [forall p :Positive, P p];
    - Dimostrare un primo lemma di tipo:
            [forall p:Positive, P p -> P (XI p)]
    che corrisponde ad uno dei due passi induttivi del
    principio di induzione [Positive_ind];
    - Dimostrare un secondo lemma di tipo:
            [forall p:Positive, P p -> P (XO p)]
    che corrisponde all'altro passo induttivo del
    principio di induzione [Positive_ind];
    - Dimostrare un lemma finale di tipo:
            [P PositiveNum.XH]
    che corrisponde al passo base del principio di
    induzione [Positive_ind].

  Dati i lemmi, [Positive_ind] può implicare [P] cui
  siamo interessati.
*)

(** *** Il predicato da dimostrare *)
Definition P_pos2Nat_nat2Pos (p: PositiveNum.Positive) :=
  pos2Nat (PositiveNum.succ p) = S (pos2Nat p).

Lemma pos2Nat_nat2Pos_XH:
  P_pos2Nat_nat2Pos PositiveNum.XH.
Proof. unfold P_pos2Nat_nat2Pos. reflexivity.
Qed.

Lemma pos2Nat_nat2Pos_XO: forall (p: PositiveNum.Positive),
  P_pos2Nat_nat2Pos p
    -> P_pos2Nat_nat2Pos (PositiveNum.XO p).
Proof. unfold P_pos2Nat_nat2Pos. intros. reflexivity.
Qed.

Lemma pos2Nat_nat2Pos_XI: forall (p: PositiveNum.Positive),
  P_pos2Nat_nat2Pos p
    -> P_pos2Nat_nat2Pos (PositiveNum.XI p).
Proof. unfold P_pos2Nat_nat2Pos. intros.
Admitted.

(** *** Una relazione tra [pos2Nat] e [nat2Pos] *)

(** **** [pos2Nat_nat2Pos]: 1ma versione di dimostrazione *)
(** [Positive_ind] è usato tramite _forward reasoning_,
usando la tattica [apply].
*)
Proposition pos2Nat_nat2Pos: forall (p: PositiveNum.Positive),
  P_pos2Nat_nat2Pos p.
Proof.
apply (PositiveNum.Positive_ind P_pos2Nat_nat2Pos).
  - apply pos2Nat_nat2Pos_XI.
  - apply pos2Nat_nat2Pos_XO.
  - apply pos2Nat_nat2Pos_XH.
Qed.

(** [pos2Nat_nat2Pos]: 2da versione di dimostrazione *)
(** [Postivie_ind] è visto come funzione. Lo si applca
alle funznioni che costituiscono le dimostrazioni dei
lemmi corrispondenti ai passi induttivi e al passo base
[Postivie_ind] stesso.
*)

Check P_pos2Nat_nat2Pos.
Check (PositiveNum.Positive_ind P_pos2Nat_nat2Pos).
Check (PositiveNum.Positive_ind
        P_pos2Nat_nat2Pos
          pos2Nat_nat2Pos_XI
            pos2Nat_nat2Pos_XO
              pos2Nat_nat2Pos_XH).

Proposition pos2Nat_nat2Pos': forall (p: PositiveNum.Positive),
  pos2Nat (PositiveNum.succ p) = S (pos2Nat p).
Proof
PositiveNum.Positive_ind
        P_pos2Nat_nat2Pos
        pos2Nat_nat2Pos_XI
        pos2Nat_nat2Pos_XO
        pos2Nat_nat2Pos_XH   .

(** ********* *)
(** ESERCIZIO *)
(** ********* *)

(** Abbiamo già visto in [PositiveNumProperties00] una relazione
tra le funzioni [pred_of_double] e [PositiveNum.succ].

La seguente definizione ribadisce la relazione già vista,
definendo un predicato corrispondente:
*)
Definition P_pred_of_double_succ (p: PositiveNum.Positive) : Prop :=
    PositiveNumProperties00.pred_of_double (PositiveNum.succ p)
      = PositiveNum.XI p.

(** Dimostare il predicato [pred_of_double_succ'], applicando il
principio di induzione [PositiveNum.Positive_ind], dopo aver
dimostrato i seguenti lemmi. *)

Lemma P_pred_of_double_succ_XH:
  P_pred_of_double_succ PositiveNum.XH.
Admitted.

Lemma P_pred_of_double_succ_XO: forall (p: PositiveNum.Positive),
  P_pred_of_double_succ p
    -> P_pred_of_double_succ (PositiveNum.XO p).
Admitted.

Lemma P_pred_of_double_succ_XI: forall (p: PositiveNum.Positive),
  P_pred_of_double_succ p
    -> P_pred_of_double_succ (PositiveNum.XI p).
Admitted.

Corollary pred_of_double_succ': forall (p: PositiveNum.Positive),
 P_pred_of_double_succ p.
Admitted.

(** ********* *)
(** ESERCIZIO *)
(** ********* *)

(** Ripetere il procedimento dell'esercizio precedente,
in relazione a [PositiveNumProperties00.succ_pred_of_double],
definendo il predicato da dimostrare tramite
[PositiveNum.Positive_ind], per cui è necessario definire
anche i prodicati corripsondenti ai passi induttie e al
passo base. *)