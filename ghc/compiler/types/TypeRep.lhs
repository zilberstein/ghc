%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[TypeRep]{Type - friends' interface}

\begin{code}
module TypeRep (
	Type(..), TyNote(..), PredType(..), UsageAnn(..),	-- Representation visible to friends
	
 	Kind, ThetaType, RhoType, TauType, SigmaType,		-- Synonyms
	TyVarSubst,

	superKind, superBoxity,				-- KX and BX respectively
	boxedBoxity, unboxedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	boxedTypeKind, unboxedTypeKind, openTypeKind, 	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX

	funTyCon
    ) where

#include "HsVersions.h"

-- friends:
import Var	( TyVar, UVar )
import VarEnv
import VarSet

import Name	( Name, Provenance(..), ExportFlag(..),
		  mkWiredInTyConName, mkGlobalName, mkKindOccFS, tcName,
		)
import OccName	( mkSrcOccFS, tcName )
import TyCon	( TyCon, KindCon,
		  mkFunTyCon, mkKindCon, mkSuperKindCon,
		)
import Class	( Class )

-- others
import SrcLoc		( mkBuiltinSrcLoc )
import PrelNames	( pREL_GHC, kindConKey, boxityConKey, boxedConKey, unboxedConKey, 
			  typeConKey, anyBoxConKey, funTyConKey
			)
\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types cannot instantiate a type variable.
			Unboxed types are always unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.
			(NOTE: previously "pointed").			

	*algebraic*	A type with one or more constructors, whether declared
			with "data" or "newtype".   
			An algebraic type is one that can be deconstructed
			with a case expression.  
			*NOT* the same as lifted types,  because we also 
			include unboxed tuples in this classification.

	*data*		A type declared with "data".  Also boxed tuples.

	*primitive*	iff it is a built-in type that can't be expressed
			in Haskell.

Currently, all primitive types are unlifted, but that's not necessarily
the case.  (E.g. Int could be primitive.)

Some primitive types are unboxed, such as Int#, whereas some are boxed
but unlifted (such as ByteArray#).  The only primitive types that we
classify as algebraic are the unboxed tuples.

examples of type classifications:

Type		primitive	boxed		lifted		algebraic    
-----------------------------------------------------------------------------
Int#,		Yes		No		No		No
ByteArray#	Yes		Yes		No		No
(# a, b #)	Yes		No		No		Yes
(  a, b  )	No		Yes		Yes		Yes
[a]		No		Yes		Yes		Yes

%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
type SuperKind = Type
type Kind      = Type

type TyVarSubst = TyVarEnv Type

data Type
  = TyVarTy TyVar

  | AppTy
	Type		-- Function is *not* a TyConApp
	Type

  | TyConApp		-- Application of a TyCon
	TyCon		-- *Invariant* saturated appliations of FunTyCon and
			-- 	synonyms have their own constructors, below.
	[Type]		-- Might not be saturated.

  | FunTy		-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	Type
	Type

  | ForAllTy		-- A polymorphic type
	TyVar
	Type	

  | PredTy		-- A Haskell predicate
	PredType

  | NoteTy 		-- A type with a note attached
	TyNote
	Type		-- The expanded version

data TyNote
  = SynNote Type	-- The unexpanded version of the type synonym; always a TyConApp
  | FTVNote TyVarSet	-- The free type variables of the noted expression
  | UsgNote UsageAnn    -- The usage annotation at this node
  | UsgForAll UVar      -- Annotation variable binder

data UsageAnn
  = UsOnce		-- Used at most once
  | UsMany		-- Used possibly many times (no info; this annotation can be omitted)
  | UsVar    UVar	-- Annotation is variable (unbound OK only inside analysis)


type ThetaType 	  = [PredType]
type RhoType   	  = Type
type TauType   	  = Type
type SigmaType    = Type
\end{code}


-------------------------------------
 		Predicates

Consider these examples:
	f :: (Eq a) => a -> Int
	g :: (?x :: Int -> Int) => a -> Int
	h :: (r\l) => {r} => {l::Int | r}

Here the "Eq a" and "?x :: Int -> Int" and "r\l" are all called *predicates*
Predicates are represented inside GHC by PredType:

\begin{code}
data PredType  = Class  Class [Type]
	       | IParam Name  Type
\end{code}

(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using 
	* a FunTy for the double arrow
	* with a PredTy as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (PredTy p) then the predicate p is
represented by evidence (a dictionary, for example, of type (predRepTy p).


%************************************************************************
%*									*
\subsection{Kinds}
%*									*
%************************************************************************

Kinds
~~~~~
kind :: KX = kind -> kind
           | Type boxity	-- (Type *) is printed as just *
				-- (Type #) is printed as just #

           | OpenKind		-- Can be boxed or unboxed
				-- Printed '?'

           | kv			-- A kind variable; *only* happens during kind checking

boxity :: BX = *	-- Boxed
	     | #	-- Unboxed
	     | bv	-- A boxity variable; *only* happens during kind checking

There's a little subtyping at the kind level:  
	forall b. Type b <: OpenKind

That is, a type of kind (Type b) is OK in a context requiring an OpenKind

OpenKind, written '?', is used as the kind for certain type variables,
in two situations:

1.  The universally quantified type variable(s) for special built-in 
    things like error :: forall (a::?). String -> a. 
    Here, the 'a' can be instantiated to a boxed or unboxed type.  

2.  Kind '?' is also used when the typechecker needs to create a fresh
    type variable, one that may very well later be unified with a type.
    For example, suppose f::a, and we see an application (f x).  Then a
    must be a function type, so we unify a with (b->c).  But what kind
    are b and c?  They can be boxed or unboxed types, so we give them kind '?'.

    When the type checker generalises over a bunch of type variables, it
    makes any that still have kind '?' into kind '*'.  So kind '?' is never
    present in an inferred type.


\begin{code}
mk_kind_name key str = mkGlobalName key pREL_GHC (mkKindOccFS tcName str)
				    (LocalDef mkBuiltinSrcLoc NotExported)
	-- mk_kind_name is a bit of a hack
	-- The LocalDef means that we print the name without
	-- a qualifier, which is what we want for these kinds.
	-- It's used for both Kinds and Boxities
\end{code}

------------------------------------------
Define  KX, the type of a kind
	BX, the type of a boxity

\begin{code}
superKind :: SuperKind 		-- KX, the type of all kinds
superKindName = mk_kind_name kindConKey SLIT("KX")
superKind = TyConApp (mkSuperKindCon superKindName) []

superBoxity :: SuperKind		-- BX, the type of all boxities
superBoxityName = mk_kind_name boxityConKey SLIT("BX")
superBoxity = TyConApp (mkSuperKindCon superBoxityName) []
\end{code}

------------------------------------------
Define boxities: @*@ and @#@

\begin{code}
boxedBoxity, unboxedBoxity :: Kind		-- :: BX

boxedConName = mk_kind_name boxedConKey SLIT("*")
boxedBoxity  = TyConApp (mkKindCon boxedConName superBoxity) []

unboxedConName = mk_kind_name unboxedConKey SLIT("#")
unboxedBoxity  = TyConApp (mkKindCon unboxedConName superBoxity) []
\end{code}

------------------------------------------
Define kinds: Type, Type *, Type #, and OpenKind

\begin{code}
typeCon :: KindCon	-- :: BX -> KX
typeConName = mk_kind_name typeConKey SLIT("Type")
typeCon     = mkKindCon typeConName (superBoxity `FunTy` superKind)

boxedTypeKind, unboxedTypeKind, openTypeKind :: Kind	-- Of superkind superKind

boxedTypeKind   = TyConApp typeCon [boxedBoxity]
unboxedTypeKind = TyConApp typeCon [unboxedBoxity]

openKindConName = mk_kind_name anyBoxConKey SLIT("?")
openKindCon     = mkKindCon openKindConName superKind
openTypeKind    = TyConApp openKindCon []
\end{code}

------------------------------------------
Define arrow kinds

\begin{code}
mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = k1 `FunTy` k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds
\end{code}


%************************************************************************
%*									*
\subsection{Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
funTyConName = mkWiredInTyConName funTyConKey pREL_GHC (mkSrcOccFS tcName SLIT("(->)")) funTyCon
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [boxedTypeKind, boxedTypeKind] boxedTypeKind)
\end{code}


