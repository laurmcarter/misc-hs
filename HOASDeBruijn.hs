{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module HOASDeBruijn where

import Prelude hiding (Eq(..),Num(..),Ord(..),(&&),(||),not)
import qualified Prelude as P
import Data.Typeable
import GHC.Exts (Constraint)
import Prelude.Extras (Show1(..))
import Data.Bits

-- HOAS

type C = Typeable :*: Show

-- Prog {{{

data Prog r where
  P  :: Convert f r => f -> Prog r

instance Show (Prog r) where
  showsPrec = showsPrecF

instance ShowF Prog where
  showsPrecF d (P p :: Prog r) = showParen (d P.> 10)
    $ showString "P "
    . showsPrecF 11 (convert_ p)

data OpenProg t where
  OP :: OFun t -> OpenProg t

instance Show (OpenProg r) where
  showsPrec = showsPrecF

instance ShowF OpenProg where
  showsPrecF d (OP f) = showParen (d P.> 10)
    $ showString "OP "
    . showsPrecF 11 f

-- }}}

-- Exp {{{

data Exp (t :: *) where
  Tag     :: Int -> Exp t
  TagFn   :: C a => Int -> Exp a -> Exp b
  Const   :: C a => a -> Exp a
  Tuple   :: Tuple Exp (TupleRep t) -> Exp t
  Cond    :: Exp Bool -> Exp t -> Exp t -> Exp t
  PrimApp :: C a => PrimFun (a -> b) -> Exp a -> Exp b

instance Show (Exp t) where
  showsPrec = showsPrecF

instance ShowF Exp where
  showsPrecF d expr = showParen (d P.> 10) $ case expr of
    Tag i       ->
        showString "Tag "
      . shows i
    TagFn i arg -> 
        showString "TagFn "
      . shows i
      . showsPrecF 11 arg
    Const v     ->
        showString "Const "
      . shows v
    Tuple tup   ->
        showString "Tuple "
      . showsPrec 11 tup
    Cond t c a  ->
        showString "Cond "
      . showsPrecF 11 t
      . showChar ' '
      . showsPrecF 11 c
      . showChar ' '
      . showsPrecF 11 a
    PrimApp p x ->
        showString "PrimApp "
      . showsPrec 11 p
      . showsPrecF 11 x

-- }}}

-- Conversion {{{

convert_ :: Convert f r => f -> OpenFun '[] r
convert_ = convert MT

class Convert f r | f -> r where
  convert :: Layout env env -> f -> OpenFun env r

instance Typeable t => Convert (Exp t) t where
  convert lt = OBody . convertOpenExp lt

instance (Typeable t, Convert f r) => Convert (Exp t -> f) (t -> r) where
  convert lt f = OLam $ convert lt' $ f a
    where
    a   = Tag $ size lt
    lt' = VZ :** inc lt

instance (All C '[a,b], Convert f r)
  => Convert ((Exp a -> Exp b) -> f) ((a -> b) -> r) where
  convert lt f = OLam $ convert lt' $ f g
    where
    g   = TagFn $ size lt
    lt' = VZ :** inc lt

convertOpenExp :: forall env t. (Typeable t)
  => Layout env env -> Exp t -> OpenExp env t
convertOpenExp lt = go
  where
  go :: Typeable t' => Exp t' -> OpenExp env t'
  go = \case
    Tag i       -> Var    $ prjVar (n P.- i) lt
    TagFn i arg -> VarFn  ( prjVar (n P.- i) lt ) $ go arg
    Const v     -> OConst v
    Tuple tup   -> OTuple $ convertTuple lt tup
    Cond t c a  -> OCond (go t) (go c) (go a)
    PrimApp p x -> OPrimApp p $ go x
  n = size lt P.- 1

convertTuple :: Layout env env -> Tuple Exp t -> Tuple (OpenExp env) t
convertTuple lt = \case
  Nil      -> Nil
  a :* tup -> convertOpenExp lt a :* convertTuple lt tup

prjVar :: forall t env env'. Typeable t => Int -> Layout env env' -> Var env t
prjVar n lt = case (n,lt) of
  (0,(x :: Var env u) :** _) -> case gcast x of
    Just v -> v
    _      -> error $ unlines
      [ "EDSL Compiler Type Error."
      , "Couldn't match expected type `" ++ show (typeOf (undefined :: t))
           ++ "' against inferredtype `" ++ show (typeOf (undefined :: u))
           ++ "'"
      ]
  (n,(x :: Var env u) :** lt') -> prjVar (n P.- 1) lt'
  (_,MT) -> error "prjVar: inconsistent valuation"

convertProg :: Prog t -> OpenProg t
convertProg (P f) = OP $ convert_ f

-- }}}

-- DeBruijn

-- Var Layout {{{

data Var (env :: [*]) (t :: *) where
  VZ ::              Var (t ': env) t
  VS :: Var env t -> Var (s ': env) t

instance ShowF (Var env) where
  showsPrecF d = \case
    VZ   -> showString "VZ"
    VS x -> showParen (d P.> 10)
      $ showString "VS "
      . showsPrecF 11 x

instance Show (Var env t) where
  showsPrec = showsPrecF

data Layout (env :: [*]) (env' :: [*]) where
  MT    :: Layout env '[]
  (:**) :: Typeable t => Var env t -> Layout env env' -> Layout env (t ': env')
infixr 4 :**

deriving instance Show (Layout env env')

(**:) :: (Typeable t1, Typeable t2) => Var env t1 -> Var env t2 -> Layout env '[t1,t2]
x **: y = x :** y :** MT
infix 5 **:

size :: Layout env env' -> Int
size = \case
  MT       -> 0
  _ :** lt -> 1 P.+ size lt

inc :: Layout env env' -> Layout (t ': env) env'
inc = \case
  MT       -> MT
  x :** lt -> VS x :** inc lt

-- }}}

-- OpenExp OpenFun {{{

data OpenExp (env :: [*]) (t :: *) where
  Var      :: Var env t
           -> OpenExp env t
  VarFn    :: Var env (a -> b)
           -> OpenExp env a
           -> OpenExp env b
  OConst   :: (Show t, Typeable t) => t -> OpenExp env t
  OTuple   :: Tuple (OpenExp env) (TupleRep t)
           -> OpenExp env t
  OCond    :: OpenExp env Bool
           -> OpenExp env t
           -> OpenExp env t
           -> OpenExp env t
  OPrimApp :: PrimFun (a -> b)
           -> OpenExp env a
           -> OpenExp env b

instance Show (OpenExp env t) where
  showsPrec = showsPrecF

instance ShowF (OpenExp env) where
  showsPrecF d oe = showParen (d P.> 10) $
    case oe of
      Var x          ->
          showString "Var "
        . showsPrec 11 x
      VarFn f x      ->
          showString "VarFn "
        . showsPrec 11 f
        . showChar ' '
        . showsPrecF 11 x
      OConst c       ->
          showString "OConst "
        . showsPrec 11 c
      OTuple t       ->
          showString "OTuple "
        . showsPrecF 11 t
      OCond  t c a   ->
          showString "OCond "
        . showsPrecF 11 t
        . showChar ' '
        . showsPrecF 11 c
        . showChar ' '
        . showsPrecF 11 a
      OPrimApp f x   ->
          showString "OPrimApp "
        . showsPrec 11 f
        . showChar ' '
        . showsPrecF 11 x

data OpenFun (env :: [*]) (t :: *) where
  OBody :: OpenExp env t        -> OpenFun env t
  OLam  :: OpenFun (a ': env) t -> OpenFun env (a -> t)

instance Show (OpenFun env t) where
  showsPrec = showsPrecF

instance ShowF (OpenFun env) where
  showsPrecF d f = showParen (d P.> 10) $
    case f of
      OBody e -> 
          showString "OBody "
        . showsPrecF 11 e
      OLam  e ->
          showString "OLam "
        . showsPrecF 11 e

type OExp = OpenExp '[]
type OFun = OpenFun '[]

-- }}}

-- Prim {{{

data PrimFun (t :: *) where
  PrimEq   :: Eq_  a => PrimFun ((a,a)       -> Bool)
  ----
  PrimLe   :: Ord_ a => PrimFun ((a,a)       -> Bool)
  PrimGe   :: Ord_ a => PrimFun ((a,a)       -> Bool)
  PrimLt   :: Ord_ a => PrimFun ((a,a)       -> Bool)
  PrimGt   :: Ord_ a => PrimFun ((a,a)       -> Bool)
  ----
  PrimAdd  :: Num_ a => PrimFun ((a,a)       ->    a)
  PrimSub  :: Num_ a => PrimFun ((a,a)       ->    a)
  PrimMul  :: Num_ a => PrimFun ((a,a)       ->    a)
  ----
  PrimNot  ::           PrimFun (Bool        -> Bool)
  PrimAnd  ::           PrimFun ((Bool,Bool) -> Bool)
  PrimOr   ::           PrimFun ((Bool,Bool) -> Bool)

instance Show (PrimFun t) where
  show = showF

instance ShowF PrimFun where
  showF = \case
    PrimEq  -> "PrimEq"
    PrimLe  -> "PrimLe"
    PrimGe  -> "PrimGe"
    PrimLt  -> "PrimLt"
    PrimGt  -> "PrimGt"
    PrimAdd -> "PrimAdd"
    PrimSub -> "PrimSub"
    PrimMul -> "PrimMul"
    PrimNot -> "PrimNot"
    PrimAnd -> "PrimAnd"
    PrimOr  -> "PrimOr"

-- }}}

-- Tuple {{{

type family   TupleRep (ts :: *) :: [*]
type instance TupleRep (a,b)     = '[a,b]
type instance TupleRep (a,b,c)   = '[a,b,c]
type instance TupleRep (a,b,c,d) = '[a,b,c,d]

data Tuple (f :: * -> *) (ts :: [*]) where
  Nil  ::                                    Tuple f '[]
  (:*) :: (Typeable t) => f t -> Tuple f ts -> Tuple f (t ': ts)
infixr 4 :*

instance ShowF f => Show (Tuple f ts) where
  showsPrec = showsPrecF

instance ShowF f => ShowF (Tuple f) where
  showsPrecF d = \case
    Nil     -> showString "Nil"
    a :* as -> showParen (d P.> 4)
      $ showsPrecF 5 a
      . showString " :* "
      . showsPrecF 4 as

(*:) :: All Typeable '[a,b] => f a -> f b -> Tuple f '[a,b]
x *: y = x :* y :* Nil
infix 5 *:

data TupleVar (ts :: [*]) (t :: *) where
  TZ ::                  TupleVar (t ': ts) t
  TS :: TupleVar ts t -> TupleVar (s ': ts) t

instance Show (TupleVar ts t) where
  showsPrec = showsPrecF

instance ShowF (TupleVar ts) where
  showsPrecF d = \case
    TZ   -> showString "TZ"
    TS x -> showParen (d P.> 10)
      $ showString "TS "
      . showsPrecF 11 x

-- }}}

-- Language

-- Classes {{{

class C a => Eq_ a where
  (==) :: Exp a -> Exp a -> Exp Bool
  (==) = op2 PrimEq
  (/=) :: Exp a -> Exp a -> Exp Bool
  x /= y = op1 PrimNot $ x == y

class Eq_ a => Ord_ a where
  (<=) :: Exp a -> Exp a -> Exp Bool
  (<=) = op2 PrimLe
  (>=) :: Exp a -> Exp a -> Exp Bool
  (>=) = op2 PrimGe
  (<)  :: Exp a -> Exp a -> Exp Bool
  (<)  = op2 PrimLt
  (>)  :: Exp a -> Exp a -> Exp Bool
  (>)  = op2 PrimGt

class C a => Num_ a where
  (+) :: Exp a -> Exp a -> Exp a
  (+) = op2 PrimAdd
  (-) :: Exp a -> Exp a -> Exp a
  (-) = op2 PrimSub
  (*) :: Exp a -> Exp a -> Exp a
  (*) = op2 PrimMul

-- }}}

-- Instances {{{

instance Eq_  Bool
instance Ord_ Bool

instance Eq_  Char
instance Ord_ Char

instance Eq_  Int
instance Ord_ Int
instance Num_ Int

-- }}}

-- Ops {{{

not :: Exp Bool -> Exp Bool
not = op1 PrimNot

(&&) :: Exp Bool -> Exp Bool -> Exp Bool
(&&) = op2 PrimAnd
infixr 3 &&

(||) :: Exp Bool -> Exp Bool -> Exp Bool
(||) = op2 PrimOr
infixr 2 ||

-- }}}

-- Util {{{

tup2 :: (All C '[a,b])
  => Exp a -> Exp b -> Exp (a,b)
tup2 x y = Tuple $ x *: y

op1 :: All C '[a,b] => PrimFun (a -> b) -> Exp a -> Exp b
op1 = PrimApp

op2 :: (All C '[a,b,r])
  => PrimFun ((a,b) -> r) -> Exp a -> Exp b -> Exp r
op2 f x y = PrimApp f $ x `tup2` y

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint where
  All c '[]       = ()
  All c (a ': as) = (c a, All c as)

class    (c a, d a) => ((c :: * -> Constraint) :*: (d :: * -> Constraint)) (a :: *)
instance (c a, d a) => (c :*: d) a
infixr 6 :*:

class    None (a :: k)
instance None (a :: k)

class    c (f a) => ((c :: * -> Constraint) :.: (f :: * -> *)) (a :: *)
instance c (f a) => (c :.: f) a
infixl 8 :.:

type family   Quant (c :: * -> Constraint) :: (* -> *) -> Constraint
type instance Quant Show = Show1

class ShowF (f :: k -> *) where
  showsPrecF :: Int -> f a -> ShowS
  showsPrecF _ x _ = showF x
  showF :: f a -> String
  showF a = showsPrecF 0 a ""

-- }}}

-- Basic Types

data AType a where
  AFunctionType :: [ScalarType t] -> ScalarType r -> AType (t -> r)
  AScalarType   :: ScalarType a -> AType a

data ScalarType a where
  NumScalarType    :: NumType a    -> ScalarType a
  NonNumScalarType :: NonNumType a -> ScalarType a

data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

data NonNumType a where
  TypeBool :: NonNumDict Bool -> NonNumType Bool
  TypeChar :: NonNumDict Char -> NonNumType Char

data IntegralType a where
  TypeInt :: IntegralDict Int -> IntegralType Int

data FloatingType a where
  TypeFloat :: FloatingDict Float -> FloatingType Float
  TypeDouble :: FloatingDict Double -> FloatingType Double

data NonNumDict a where
  NonNumDict :: (P.Eq a, P.Ord a, Show a) => NonNumDict a

data IntegralDict a where
  IntegralDict :: ( Bounded a, Enum a, P.Eq a, P.Ord a, Show a
                  , Bits a, Integral a, P.Num a, Real a)
    => IntegralDict a

data FloatingDict a where
  FloatingDict :: ( Enum a, P.Eq a, P.Ord a, Show a
                  , Floating a, Fractional a, P.Num a, Real a
                  , RealFrac a, RealFloat a
                  ) => FloatingDict a

