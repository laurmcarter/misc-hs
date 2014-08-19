{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MagicHash #-}

module STLC where

import Prelude hiding (fst,snd)

data Term
  = Set Term
  | (-->) Term Term
  | ℕ
  | Z
  | S Term
  | Λ Term
  | (&) Term Term
  | V Term

data Cx
  = E
  | (:>) Term Cx

type x ⊢ f = f x
infixr 5 ⊢

data (∈) :: Term -> Term -> Cx -> * where

  TSet  ::       E ⊢ n ∈ ℕ
           -----------------------
        ->  γ ⊢ Set n ∈ Set (S n)


  TNat  :: -----------------------
               γ ⊢ ℕ ∈ Set Z


  TZ    :: -----------------------
                 γ ⊢ Z ∈ ℕ


  TS    ::       γ ⊢ n ∈ ℕ
           -----------------------
        ->      γ ⊢ S n ∈ ℕ


  TLam  ::     σ :> γ ⊢ l ∈ τ
           -----------------------
        ->   γ ⊢ Λ l ∈ (σ --> τ)


  TApp  ::  γ ⊢ f ∈ (σ --> τ)  ->  γ ⊢ x ∈ σ
           ----------------------------------
        ->          γ ⊢ f & x ∈ τ


  TVTop :: -----------------------
              τ :> γ ⊢ V Z ∈ τ


  TVPop ::  E ⊢ i ∈ ℕ  ->  γ ⊢ V i ∈ σ
           ----------------------------
        ->     τ :> γ ⊢ V (S i) ∈ σ

class Infer (x :: Term) (τ :: Term) (γ :: Cx) | x γ -> τ where
  infer :: γ ⊢ x ∈ τ

instance (Infer l ℕ E) => Infer (Set l) (Set (S l)) γ where
  infer = TSet infer

instance Infer Z ℕ γ where
  infer = TZ

instance (Infer n ℕ γ) => Infer (S n) ℕ γ where
  infer = TS infer

instance (Infer l τ (σ :> γ)) => Infer (Λ l) (σ --> τ) γ where
  infer = TLam infer

instance (Infer f (σ --> τ) γ, Infer x σ γ) => Infer (f & x) τ γ where
  infer = TApp infer infer

instance Infer (V Z) τ (τ :> γ) where
  infer = TVTop

instance (Infer i ℕ E, Infer (V i) σ γ) => Infer (V (S i)) σ (τ :> γ) where
  infer = TVPop infer infer

type Closed e = (Infer e τ E) => E ⊢ e ∈ τ

type PlusOne = Λ (S (V Z))
type IComb = Λ (V Z)
type KComb = Λ (Λ (V (S Z)))
type SComb = Λ (Λ (Λ (((V Z) & (V (S (S Z)))) & ((V (S Z)) & (V (S (S Z)))))))

plusOne :: Closed PlusOne
plusOne = infer

fst :: Closed (Λ (Λ (V (S Z))))
fst = infer

snd :: Closed (Λ (Λ (V Z)))
snd = infer

i :: Closed IComb
i = infer

k :: Closed KComb
k = infer

s :: Closed SComb
s = infer

skk :: Closed ((SComb & KComb) & KComb)
skk = infer

instance Show (γ ⊢ x ∈ τ) where
  show (TSet l)    = "Set" ++ (show . Sub . natToInt $ l)
  show TNat        = "ℕ"
  show TZ          = "Z"
  show (TS n)      = "S (" ++ show n ++ ")"
  show (TLam l)    = "λ. " ++ show l
  show (TApp f x)  = "(" ++ show f ++ " " ++ show x ++ ")"
  show TVTop       = "v" ++ (show . Sub . varToInt $ TVTop)
  show (TVPop e i) = "v" ++ (show . Sub . varToInt $ TVPop e i)

natToInt :: γ ⊢ n ∈ ℕ -> Int
natToInt TZ     = 0
natToInt (TS i) = succ $ natToInt i
natToInt _      = error "non-canonical natural number"

varToInt :: γ ⊢ V i ∈ σ -> Int
varToInt TVTop       = 0
varToInt (TVPop _ i) = succ $ varToInt i

newtype Subscript = Sub Int
instance Show Subscript where
  show (Sub n) = fmap trans (show n)
    where
    trans = \case
      '0' -> '₀'
      '1' -> '₁'
      '2' -> '₂'
      '3' -> '₃'
      '4' -> '₄'
      '5' -> '₅'
      '6' -> '₆'
      '7' -> '₇'
      '8' -> '₈'
      '9' -> '₉'
      d   ->  d

