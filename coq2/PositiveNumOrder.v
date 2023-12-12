From LPP Require Import PositiveNum.

(* propedeutico alla definizione di (<):: BNum -> BNum -> Bool *)
Inductive Comp: Set :=
|  Lt : Comp
|  Eq : Comp
|  Gt : Comp .

Fixpoint comp (p p': PositiveNum.Positive) : Comp :=
match p, p' with
| PositiveNum.XH   , PositiveNum.XH    => Eq
| PositiveNum.XH   , PositiveNum.XO _  => Lt
| PositiveNum.XH   , PositiveNum.XI _  => Lt
| PositiveNum.XO _ , PositiveNum.XH    => Gt
| PositiveNum.XI _ , PositiveNum.XH    => Gt
| PositiveNum.XO p , PositiveNum.XO p' => comp p p'
| PositiveNum.XI p , PositiveNum.XI p' => comp p p'
| PositiveNum.XO p , PositiveNum.XI p' => match (comp p p') with
                                          | Lt => Lt
                                          | Eq => Lt
                                          | Gt => Gt
                                          end
| PositiveNum.XI p, PositiveNum.XO p' => match comp p p' with
                                          | Lt => Lt
                                          | Eq => Gt
                                          | Gt => Gt
                                          end
end .

Definition lePositive (p p': PositiveNum.Positive) : bool :=
match comp p p' with
| Lt => true
| _  => false
end .

Definition leqPositive (p p': PositiveNum.Positive) : bool :=
  orb (lePositive p p') (PositiveNum.eqPositive p p').
