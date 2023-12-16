(** Sia data una definizione induttiva di numero
naturale unario [naturale] *)

Inductive naturale : Set :=
| Zero : naturale
| Succ : naturale -> naturale .

(** Domanda 1 *)
(** Definire una funzione [meno] *)
Fixpoint meno (p q : naturale) : naturale :=
match p , q with
    | Zero, Zero           => Zero
    | Zero, (Succ q')      => Zero
    | (Succ p'), Zero      => (Succ p')
    | (Succ p'), (Succ q') => meno p' q'
end.

(** Domanda 2 *)
(** Definire un tipo induttivo adatto a caratterizzare
la struttura degli elementi nel grafo di [meno] *)
Inductive meno_rel : naturale -> naturale -> naturale -> Prop :=
    | meno_rel_ZL : forall (q : naturale), meno_rel Zero q Zero
    | meno_rel_ZR : forall (p : naturale), meno_rel p Zero p
    | meno_rel_SR  : forall (p q d : naturale),
                        meno_rel p q (Succ d) -> meno_rel p (Succ q) d.

(** Domanda 3 *)
(** Dimostrare che la definizione di [meno] è corretta
rispetto alla definizione di [meno_graph] *)
Proposition meno_corretto : forall (p q : naturale),
        meno_rel p q (meno p q).
Proof. intros. induction q.
    - destruct p.
        * apply meno_rel_ZL.
        * apply meno_rel_ZR.
    - destruct p.
        * apply meno_rel_ZL.
