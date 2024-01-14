(** Sia data una definizione induttiva di numero
naturale unario [naturale] *)

(** Domanda 1 *)
(** Definire una funzione [meno] *)

(** Domanda 2 *)
(** Definire un tipo induttivo adatto a caratterizzare
la struttura degli elementi nel grafo di [meno] *)

(** Domanda 3 *)
(** Dimostrare che la definizione di [meno] è corretta
rispetto alla definizione di [meno_graph] *)

Inductive naturale : Set :=
| Zero : naturale
| Succ : naturale -> naturale .

Fixpoint meno (p q : naturale) : naturale :=
match p , q with
    | Zero, q      => Zero
    | p, Zero      => p
    | Succ p', Succ q' => meno p' q'
end.

Inductive meno_rel : naturale -> naturale -> naturale -> Prop :=
    | meno_rel_ZL : forall (q : naturale), meno_rel Zero q Zero
    | meno_rel_ZR : forall (p : naturale), meno_rel p Zero p
    | meno_rel_SR  : forall (p q d : naturale),
                        meno_rel p q d -> meno_rel (Succ p) (Succ q) d.

Proposition meno_corretto : forall (p q : naturale),
        meno_rel p q (meno p q).
Proof. intros p. induction p as [| p' IH].
    - apply meno_rel_ZL.
    - intros q. simpl. destruct q.
        * apply  meno_rel_ZR.
        * apply meno_rel_SR. apply IH.
Qed.