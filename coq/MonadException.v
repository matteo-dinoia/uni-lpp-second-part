Require Import String.
Require Import Ascii.
Require Import List.
Import Notations ListNotations.

Module MonadException.

Notation "f ; g" :=
  (fun x =>  g (f x))
  (at level 60, right associativity) .
  (* : scope. *)

(** * Tipo di dato astratto *)
(** Per definizione, fissato un insieme [a], costruisce un
nuovo insieme in cui gli elementi hanno forma [Rise "qualche msg"]
o, per un qualsiasi elemento [x:a] fissato, [Return x] *)

Definition Exception := string.
Inductive E (a : Set) : Set :=
|   Rise : Exception -> E a
|   Return : a -> E a
.

(** * E è un funtore *)
(** È possibile definire le funzioni:
    - [id] che funge da identità;
    - [fmap] che, ad ogni morfismo [f: a -> b], associa un
    morfismo di tipo [E a -> E b].
*)
Definition id {a : Set}(x : a) : a := x.

Definition fmap {a b : Set} (f : a -> b)(x: E a) : E b :=
  match x with
  | Rise _ e => Rise _ e
  | Return _ e => Return _ (f e)
  end.

(** [id] e [fmap: E a -> E b] soddisfano le due proprietà
funtoriali: *)
Lemma idIsIdentity: forall (a : Set)(x : E a),
  x = id x.
Proof. intros A X.
unfold id. reflexivity.
Qed.

Lemma fmapComposes: forall(a b c : Set)(f : a -> b) (g : b -> c)(x : E a),
  ((fmap f) ; (fmap g)) x = fmap (f ; g) x.
Proof. intros.
destruct x as [nok | ok].
  -unfold fmap. reflexivity. (** First case *)
  -unfold fmap. reflexivity. (** Second case *)
Qed.
(** * E è anche un funtore "Applicative" *)
(** Significa che [E] ammette le funzioni [pure] e [<*>]
che soddisfano specifiche proprietà che lo rendono una
struttura "Applicative". *)

(** **** Funzione [pure] *)
(** Incapsula un qualsiasi valore in [E]: *)
Definition pure  {a : Set} (x : a): E a := Return _ x.

(** **** Funzione [<*>] *)
(** Applica (da qui il nome "Applicative"?) una funzione
incapsulata ad un valore incapsulato: *)
Definition star {a b : Set} (f: E (a -> b))(x : E a) : E b :=
  match f, x with
  | Rise _ er, _ => Rise _ er
  | _, Rise _ er => Rise _ er
  | Return _ vf, Return _ vx => Return _ (vf vx)
  end.


Notation "x <*> y" :=
  (star x y)
  (at level 60, right associativity) .  (* in BNumFuncApp
                                           è left
                                           ASSOCIATIVE! *)
  (* : scope. *)

(** ** Proprietà di [pure] e [<*>] *)
(** Sono le proprietà per cui possiamo affermare che [E]
è un funtore applicativo. Sono elencate una ad una qui di
seguito e dimostrate.
*)

(** **** [pure] è un Omomorfismo rispetto a [<*>] *)
(** Usare [<*>] per applicare una funzione a un valore,
entrambi incassati in [E] tramite [pure], equivale ad
applicare la funzione all'argomento, per poi incassare
in [ID] il risultato. *)
Proposition pureHomomorphic: forall{A B: Set}(a : A)(f: A->B),
    pure f <*> pure a = pure (f a).
Proof. intros.
  unfold star. simpl.
  reflexivity.
Qed.

(** **** [pure] e [<*>] soddisfano l'Identità applicativa *)
(** L'Identità applicativa afferma che, usando [<*>] per
applicare un'identità [id: a -> a], inglobata da [pure],
ad un qualunque argomento, il comportamento di [id: a -> a]
non è alterato da [pure]: *)
Proposition pureIdentity: forall{A: Set}(a : A),
pure id <*> pure a = pure (id a).
Proof.
  intros.
  apply pureHomomorphic.
Qed.

(** **** Legge della composizione/associatività *)
(* Applicando [pure] alla composizione di funzioni,
l'applicazione [<*>] è associativa. *)
Proposition starComposition: forall{A B C: Set}(x : E A)(f: A->B)(g: B->C),
    ((pure (fun g => fun f => fun v => g (f v)) <*> pure g) <*> pure f) <*> x = pure g <*> (pure f  <*> x).
Proof. intros.
  unfold star. simpl. destruct x as [nok | ok].
    -reflexivity.
    -reflexivity.
Qed.

(** **** [<*>] e [pure] si "scambiano" rispettto a [<*>] *)
Proposition starInterchange: forall{A B: Set}(a : A)(f: E (A -> B)),
    f <*> pure a = pure (fun g => g a) <*> f.
Proof. intros.
  unfold star. simpl. destruct f as [nok | ok].
  -reflexivity.
  -reflexivity.
Qed.

(** ** [fmap] di un funtore "Applicative" *)
(** [fmap] può essere definita come composizione di [<*>] e [pure]  *)
Lemma fmapIsStarpure: forall {A B: Set} (x : E A) (f : A -> B),
  fmap f x = pure f <*> x.
Proof. intros.
  unfold star. unfold fmap.
  simpl. reflexivity.
Qed.
(** * E è un è anche una Monade *)
(** Per il funtore [E] è possibile definire le funzioni [Return] e
[bind] che soddisfano specifiche proprietà. *)

(** **** Funzione [Return] *)
Definition returN {a : Set}(x : a) : E a := Return _ x.

(** **** Funzione [bind] *)
Definition bind {A B : Set}(x : E A)(f : (A -> E B)) : E B :=
  match x with
  | Rise _ e => Rise _ e
  | Return _ e => f e
  end.

Notation "x >>= y" :=
  (bind x y)
  (at level 60, right associativity) .
  (** : scope. *)

(** ** Proprietà di [Return] e [bind] *)
Proposition returNLeftIdentity: forall{A B: Set}(a : A) (f: A -> E B),
    returN a >>= f = f a.
Proof. intros. unfold bind. simpl. reflexivity.
Qed.


Proposition returNRightIdentity: forall{A B: Set}(a : A) (m: E (A -> B)),
    m>>= returN = m.
Proof. intros. unfold bind. destruct m as [nok | ok].
  -reflexivity.
  -unfold returN. reflexivity.
Qed.

Proposition bindAssoc: forall{A B C: Set}(x : E A) (f: A -> E B) (g: B -> E C),
    (x >>= f) >>= g = x >>= (fun y => f y >>= g).
Proof. intros. unfold bind. destruct x as [nok | ok].
  -reflexivity.
  -destruct (f ok) as [nok2 | ok2]; reflexivity.
Qed.

(** ** Relazioni tra strutture "Functor", "Applicative" e "Monad" *)
(**  Le operazioni associate alle due struture
menzionate devono relazionarsi come segue.

Da un lato [pure] e [returN] devono essere funzioni
equivalenti:
*)
Proposition pureEqreturN: forall{A : Set} (a : A),
    returN a = pure a.
Proof. intros.
  unfold returN. unfold pure. reflexivity.
Qed.


(** Dall'altro, la composizione "Applicative" [<*>] deve
essere definibile per mezzo delle due funzioni [returN] e [>>=]
([bind]) che caratterizzano la monade: *)
Proposition starEqBindreturN: forall{A B: Set} (f : E (A -> B)) (x : E A),
  f <*> x = f >>= (fun g => x >>= (fun a => returN (g a))).
Proof. intros.
  unfold star. unfold bind. reflexivity.
Qed.

(** Anche la funzione [fmap] del funtore alla base della monade è
definibile tramite [returN] e [>>=] ([bind]) *)
Proposition fmapEqBindReturn: forall {A B : Set} (x : E A) (f: A -> B),
    fmap f x = x >>= (fun y => returN (f y)).
Proof. intros.
  unfold fmap. unfold bind.
  reflexivity.
Qed.

End MonadException.