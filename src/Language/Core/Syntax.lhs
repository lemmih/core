%
% (c) The University of Glasgow 2001-2006
%
\begin{code}
module Language.Core.Syntax where

import qualified Data.ByteString.Lazy.Char8 as L

data Module
 = Module (Pkgname, Mname) [Tdef] [Vdefg]
   deriving (Show,Read,Eq)

data Tdef 
  = Data (Qual Tcon) [Tbind] [Cdef]
  | Newtype (Qual Tcon) (Qual Tcon) [Tbind] Ty
   deriving (Show,Read,Eq)

data Cdef 
  = Constr (Qual Dcon) [Tbind] [Ty]
  | GadtConstr (Qual Dcon) Ty
   deriving (Show,Read,Eq)

data Vdefg 
  = Rec [Vdef]
  | Nonrec Vdef
   deriving (Show,Read,Eq)

-- Top-level bindings are qualified, so that the printer doesn't have to pass
-- around the module name.
data Vdef = Vdef { vdefLocal :: Bool
                 , vdefName  :: Qual Var
                 , vdefType  :: Ty
                 , vdefExp   :: Exp }
    deriving (Show,Read,Eq)

data Exp 
  = Var (Qual Var)
  | Dcon (Qual Dcon)
  | Lit Lit
  | App Exp Exp
  | Appt Exp Ty
  | Lam Vbind Exp
  | Lamt Tbind Exp
  | Let Vdefg Exp
  | Case Exp Vbind Ty [Alt] {- non-empty list -}
  | Cast Exp Ty
  | Note String Exp
  | External String String Ty {- target name, convention, and type -} 
  | DynExternal String Ty {- convention and type (incl. Addr# of target as first arg) -} 
  | Label String
   deriving (Show,Read,Eq)

data Alt 
  = Acon (Qual Dcon) [Tbind] [Vbind] Exp
  | Alit Lit Exp
  | Adefault Exp
   deriving (Show,Read,Eq)

type Vbind = (Qual Var,Ty)
type Tbind = (Tvar,Kind)

data Ty 
  = Tvar Tvar
  | Tcon (Qual Tcon)
  | Tapp Ty Ty
  | Tarrow Ty Ty
  | Tforall Tbind Ty 
-- We distinguish primitive coercions
-- (represented in GHC by wired-in names), because
-- External Core treats them specially, so we have
-- to print them out with special syntax.
  | TransCoercion Ty Ty
  | SymCoercion Ty
  | UnsafeCoercion Ty Ty
  | InstCoercion Ty Ty
  | LeftCoercion Ty
  | RightCoercion Ty
   deriving (Show,Read,Eq)

data Kind 
  = Klifted
  | Kunlifted
  | Kunboxed
  | Kopen
  | Karrow Kind Kind
  | Keq Ty Ty
   deriving (Show,Read,Eq)

data Lit 
  = Lint Integer Ty
  | Lrational Rational Ty
  | Lchar Char Ty
  | Lstring String Ty
   deriving (Show,Read,Eq)
  

type Pkgname = Id
type Mname = Id
type Var = Id
type Tvar = Id
type Tcon = Id
type Dcon = Id

type Qual t = (Pkgname, Mname,t)

type Id = L.ByteString


\end{code}




