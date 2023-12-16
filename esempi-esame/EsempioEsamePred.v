(** Sia data una definizione induttiva di numero
naturale unario [naturale] *)

Inductive naturale : Set :=
| Zero : naturale
| Succ : naturale -> naturale .

(** Domanda 1 *)
(** Definire una funzione predecessore *)
Definition pred (p: naturale) : naturale :=
    match p with
    | Zero     => Zero
    | Succ (x) => x
    end.

(** Domanda 2 *)
(** Definire un tipo induttivo adatto a caratterizzare
la struttura degli elementi nel grafo di [pred] *)
Inductive pred_graph : naturale -> naturale -> Prop :=
 | pred_graph_zero : pred_graph Zero Zero
 | pred_graph_S : forall (n : naturale), pred_graph (Succ n) n.

(** Domanda 3 *)
(** Dimostrare che la definizione di [pred] è corretta
rispetto alla definizione di [pred_graph] *)
Proposition pred_corretto : forall (p : naturale),
    pred_graph p (pred p).
Proof. intros. induction p as [ | q IH].
    - apply pred_graph_zero.
    - unfold pred. apply pred_graph_S.
Qed.