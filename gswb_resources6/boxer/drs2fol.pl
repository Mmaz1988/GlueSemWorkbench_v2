/*************************************************************************

         name: drs2fol.pl (Chapter 7)
      version: June 8, 1998
  description: From DRSs to First-Order Logic 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(drs2fol,[drs2fol/2]).

   use_module(comsemPredicates,[compose/3,simpleTerms/1]).

/*========================================================================

   Translation Predicates
   ----------------------

drs2fol(+Drs,?F)
   converts DRS to formula F. DRS is a term drs(Dom,Cond), where
   Dom is a list of discourse referents, and Cond is a non empty
   list of DRS-conditions. 

========================================================================*/

drs2fol(drs([],[Cond]),Formula):-
   cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):-
   cond2fol(Cond1,Formula1),
   drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([X|Referents],Conds),some(X,Formula)):-
   drs2fol(drs(Referents,Conds),Formula).

cond2fol(not(Drs),not(Formula)):-
   drs2fol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):-
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([],Conds),Drs2), imp(Formula1,Formula2)):-
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([X|Referents],Conds),Drs2), all(X,Formula)):-
   cond2fol(imp(drs(Referents,Conds),Drs2),Formula).

cond2fol(eq(X,Y),eq(X,Y)).

cond2fol(BasicCondition,AtomicFormula):-
   BasicCondition =.. [_|[Symbol|Args]],
   AtomicFormula =.. [Symbol|Args],
   %findall avoids reordering arguments compared to setof/3
   findall(X,(member(X,Args),(var(X);atom(X))),Tested),
   Args = Tested.


