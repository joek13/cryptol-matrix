-- Type definitions

inductive Shape : (rank : Nat) -> Type
| singleton : Nat → Shape 1
| cons : Nat -> Shape n -> Shape (n+1)

def Shape.toList (shape : Shape rank) : List Nat
  := match shape with
  | .singleton d => [d]
  | .cons d ds => .cons d ds.toList

def Shape.elemCount (shape : Shape rank) : Nat
  := match shape with
  | .singleton d => d
  | .cons d ds => d * ds.elemCount

def Shape.indexSpace (shape : Shape rank) : Type :=
  match shape with
  | .singleton d => Fin d
  | .cons k rest => (Fin k) × rest.indexSpace

def Tensor (α : Type u) (shape : Shape rank) : Type u
  := shape.indexSpace → α

def Vec (α : Type u) (length : Nat) : Type u :=
  Tensor α (.singleton length)

-- Initialization

def Vec.fromList (xs : List α) : Vec α (List.length xs)
  := xs.get

def Vec.toList (vec : Vec α length) : List α
  := (List.finRange length).map vec

def Tensor.zero [Zero α] : Tensor α shape
  := Function.const shape.indexSpace 0

-- Transformations

def Tensor.flatten (t : Tensor α shape) : Vec α shape.elemCount
  := match shape with
  | .singleton _ => t
  | .cons k rest => sorry
