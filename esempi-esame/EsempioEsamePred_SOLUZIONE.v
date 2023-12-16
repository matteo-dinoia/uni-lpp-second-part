(** Sia data una definizione induttiva di numero 
naturale unario [naturale] *)

Inductive naturale : Set :=
| Zero : naturale
| Succ : naturale -> naturale .

(** Domanda 1 *)
(** Definire una funzione predecessore *)
Definition pred (n: naturale) : naturale :=
match n with
| Zero => Zero
| Succ n' => n'
end .

(** Domanda 2 *)
(** Definire un tipo induttivo adatto a caratterizzare
la struttura degli elementi nel grafo di [pred] *)
Inductive pred_graph : naturale -> naturale -> Prop :=
| pred_graph_Z : pred_graph Zero Zero
| pred_graph_S : forall (n: naturale),
                        pred_graph (Succ n) n .

(** Domanda 3 *)
(** Dimostrare che la definizione di [pred] è corretta 
rispetto alla definizione di [pred_graph] *)
Proposition pred_corretto : forall (n: naturale),
    pred_graph n (pred n).
Proof.
intros.
destruct n.
- simpl. apply pred_graph_Z. 
- simpl. apply pred_graph_S. 
Qed.
