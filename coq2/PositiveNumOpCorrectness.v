(** * Correttezza tramite relazioni *)

(** ** PREMESSA *)
(** Questo sorgente è autocontenuto: definizioni
che possiamo trovare in [PositiveNum.v] sono
ripetute qui per comodità.
*)

(** ** OBIETTIVO  *)
(** Illustare una tecnica per fornire un livello
di confidenza maggiore sulla correttezza della
definizione di funzioni.
*)

(** ** METODO *)
(** Ogni funzione [f] scritta in Gallina, linguaggio
di programmazione di Coq, descrive una procedura
effettiva per trasformare istanze degli argomenti in
un risultato.

Ad esempio, se [f] è unaria, [f] costruisce ogni
coppia [(n, f n)] del grafo di [f].

Il grafo di una funzione [f] può esser rappresentato
in Coq da un tipo induttivo, diciamo [f_rel], la cui
struttura può affermare se una tupla appartiene al
grafo di [f], oppure no.

Illustriamo il metodo, usando [Positive].
*)


(** ** [Positive] e operazioni *)

Inductive Positive: Set :=
| XI : Positive -> Positive
| XO : Positive -> Positive
| XH : Positive .

Fixpoint succ (p: Positive) : Positive :=
match p with
| XH   => XO XH
| XO b => XI b
| XI b => XO (succ b)
end.

Fixpoint pred (p: Positive) : Positive :=
match p with
| XH     => XH
| XO XH  => XH
| XI p'  => XO p'
| XO p'  => XI (pred p')
end.

Fixpoint add (p p': Positive) : Positive :=
match p, p' with
| XH,     XH     => XO XH
| XH,     (XO n) => XI n
| XH,     (XI n) => XO (succ n)
| (XO m), XH     => XI m
| (XI m), XH     => XO (succ m)
| (XO m), (XO n) => XO (add m n)
| (XO m), (XI n) => XI (add m n)
| (XI m), (XI n) => XO (succ (add m n))
| (XI m), (XO n) => XI (add m n)
end.

(** ** Correttezza di [pred] *)

(** Il grafo di [pred] è un tipo di dato induttivo
che _dovrebbe_ descrivere la struttura delle coppie
nel prodotto cartesiano [Positive * Positive] in cui
il primo elemento è l'argomento [n] di [pred] e il
secondo  argomento il valore che [pred n].
*)
Inductive pred_graph :  Positive -> Positive -> Prop :=
| pred_graph_H  : pred_graph XH  XH
| pred_graph_OH : pred_graph (XO XH) XH
| pred_graph_I : forall(p : Positive), pred_graph (XI p) (XO p)
| pred_graph_O : forall(p r : Positive),
          pred_graph p r -> pred_graph (XO p) (XI r).

(** Siccome ogni istanza di [pred_graph] è un
predicato, possiamo verificare la sua verità, cioè
se descive coppie [(n,m)] col significato inteso:
[m] è sempre l'immagine di [pred n].
*)
Theorem pred_correct : forall (p : Positive),
    pred_graph p (pred p).
Proof. intros. induction p as [p IH | p IH | ].
  - unfold pred. apply pred_graph_I.
  - unfold pred. fold pred. destruct p as [ p' | p' | ].
    * apply pred_graph_O. apply IH.
    * apply pred_graph_O. apply IH.
    * apply pred_graph_OH.
  - apply pred_graph_H.
Qed.


(** ** ESERCIZI *)

(** *** Correttezza di [succ] *)
(** **** Definire la struttura del grafo di [succ] *)
Inductive succ_graph : Positive -> Positive -> Prop :=
| succ_graph_H  : succ_graph XH  (XO XH)
| succ_graph_O : forall(p : Positive), succ_graph (XO p) (XI p)
| succ_graph_I : forall(p r : Positive),
          succ_graph p r -> succ_graph (XI p) (XO r).

(** **** Dimostrare la correttezza di [succ] rispetto al grafo *)
Theorem succ_correct:  forall (p : Positive),
    succ_graph p (succ p).
Proof. intros. induction p as [p IH | p IH | ].
- unfold succ. fold succ. destruct p as [ p' | p' | ].
  * apply succ_graph_I. apply IH.
  * apply succ_graph_I. apply IH.
  * apply succ_graph_I. apply succ_graph_H.
- unfold succ. apply succ_graph_O.
- apply succ_graph_H.
Qed.

(** *** Correttezza di [add] *)
(** **** Definire la struttura del grafo di [add] *)
Inductive add_graph: Positive -> Positive -> Positive -> Prop :=
| add_graph_HL: forall(p r : Positive), succ_graph p r -> add_graph XH p r
| add_graph_HR: forall(p r : Positive), succ_graph p r -> add_graph p XH r
| add_graph_OO: forall(p q r : Positive), add_graph p q r -> add_graph (XO p) (XO q) (XO r)
| add_graph_OI: forall(p q r : Positive), add_graph p q r -> add_graph (XO p) (XI q) (XI r)
| add_graph_IO: forall(p q r : Positive), add_graph p q r -> add_graph (XI p) (XO q) (XI r)
| add_graph_II: forall(p q r s : Positive),
    add_graph p q r -> succ_graph r s -> add_graph (XI p) (XI q) (XO s).

(** **** Dimostrare la correttezza di [add] rispetto al grafo *)
Theorem add_correct: forall (m n: Positive),
  add_graph m n (add m n).
Proof. intros. induction m as [m IH | m IH | ].
  -
Qed.