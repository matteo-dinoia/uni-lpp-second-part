(** Sia data una definizione induttiva di numero
naturale unario [naturale] *)

Inductive naturale : Set :=
| Zero : naturale
| Succ : naturale -> naturale .

(** Domanda 1 *)
(** Definire una funzione somma *)
Fixpoint piu (p q: naturale) : naturale :=
    match p with
    | Zero      => q
    | (Succ a)  => Succ (piu a q)
    end.

(** Domanda 2 *)
(** Definire un tipo induttivo adatto a caratterizzare
la struttura degli elementi nel grafo di [piu] *)
Inductive piu_rel : naturale -> naturale -> naturale -> Prop :=
    | piu_rel_0 : forall (q : naturale), piu_rel Zero q q
    | piu_rel_S : forall (p q r : naturale),
                        piu_rel p q r -> piu_rel (Succ p) q (Succ r)
    .


(** Domanda 3 *)
(** Dimostrare che la definizione di [piu] è corretta
rispetto alla definizione di [piu_graph] *)
Proposition piu_corretto : forall (p q: naturale),
        piu_rel p q (piu p q).
Proof. intros. induction p.
    - apply piu_rel_0.
    - apply piu_rel_S. apply IHp.
Qed.
