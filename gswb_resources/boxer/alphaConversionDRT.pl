/*************************************************************************

    File: alphaConversionDRT.txt
    
    Copyright (C) 2020 Johan Bos (johan.bos@rug.nl)

    This file is part of Boxer, version October 2020. It is derivative
    from the BB2 software by Blackburn & Bos (2004, 2006).

    This component is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This file is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this module; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA
    
    Small modifications 2018-2020 by Matthew Gotham, noted in comments
	
*************************************************************************/


:- module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(library(lists),[member/2]).
:- use_module(errors,[warning/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
   alphaConvertDRS(B1,[]-_,[]-_,B2), !.


/*========================================================================
   Variable
========================================================================*/

variable(X):- var(X), !.
variable(X):- functor(X,'$VAR',1), !.


/*========================================================================
   Alpha Conversion (terms)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   variable(X), !,
   (
      member(sub(Z,Y),Vars),           %%% BOUND
      X==Z, !,
      New=Y
   ;
      New=X                            %%% FREE
   ).

alphaConvertVar(X,_,X).


/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Var-Var,Ptr-Ptr,X2):-
   variable(X1), !,
   alphaConvertVar(X1,Var,X2).

alphaConvertDRS(lam(X,B1),Vars-Vars,Ptrs,lam(Y,B2)):- !,
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,Ptrs,B2).

alphaConvertDRS(B1:drs(D,C),Vars,Ptr1-Ptr2,B2:Drs):- !, 
   alphaConvertDRS(drs(D,C),Vars,[sub(B1,B2)|Ptr1]-Ptr2,Drs).

alphaConvertDRS(drs([],[]),Vars-Vars,Ptr-Ptr,drs([],[])):- !.

alphaConvertDRS(drs([],[C1|Conds1]),Vars1-Vars2,Ptr1-Ptr3,drs([],[C2|Conds2])):- !,
   alphaConvertCondition(C1,Vars1,Ptr1-Ptr2,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,Ptr2-Ptr3,drs([],Conds2)).

alphaConvertDRS(drs([Ref|L1],C1),Var1-Var2,Ptr1-Ptr2,drs([New|L2],C2)):- !,
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Var1]-Var2,Ptr1-Ptr2,drs(L2,C2)).

alphaConvertDRS(alfa(Type,B1,B2),Var1-Var3,Ptr1-Ptr3,alfa(Type,B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(merge(B1,B2),Var1-Var3,Ptr1-Ptr3,merge(B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(app(E1,E2),Var-Var,Ptr1-Ptr3,app(E3,E4)):- !,
   alphaConvertDRS(E1,Var-_,Ptr1-Ptr2,E3),
   alphaConvertDRS(E2,Var-_,Ptr2-Ptr3,E4).

alphaConvertDRS(sdrs([],[]),Var-Var,Ptr-Ptr,sdrs([],[])):- !.

alphaConvertDRS(sdrs([],[I:C1|L1]),Var1-Var2,Ptr1-Ptr3,sdrs([],[I:C2|L2])):- !,
   alphaConvertCondition(C1,Var1,Ptr1-Ptr2,C2),
   alphaConvertDRS(sdrs([],L1),Var1-Var2,Ptr2-Ptr3,sdrs([],L2)).

alphaConvertDRS(sdrs([B1|L1],C1),Var1-Var3,Ptr1-Ptr3,sdrs([B2|L2],C2)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B2),
   alphaConvertDRS(sdrs(L1,C1),Var2-Var3,Ptr2-Ptr3,sdrs(L2,C2)).

alphaConvertDRS(lab(Ref,B1),Var1-[sub(Ref,New)|Var2],Ptr,lab(New,B2)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr,B2).

alphaConvertDRS(sub(B1,B2),Var1-Var3,Ptr1-Ptr3,sub(B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(Sym,Vars-Vars,Ptr-Ptr,Sym):- atomic(Sym), !.

alphaConvertDRS(U,_,_,_):- !,
   warning('Unknown DRS expression: ~p',[U]), fail.


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(nec(B1),Vars,Ptr,nec(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(pos(B1),Vars,Ptr,pos(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(not(B1),Vars,Ptr,not(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(prop(X,B1),Vars,Ptr,prop(Y,B2)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(imp(B1,B2),Var,Ptr1-Ptr3,imp(B3,B4)):- !,
   alphaConvertDRS(B1,Var -Var1,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var1-_,   Ptr2-Ptr3,B4).

alphaConvertCondition(duplex(Type,B1,X,B2),Var,Ptr1-Ptr3,duplex(Type,B3,Y,B4)):- !,
   alphaConvertDRS(B1,Var-Var1,Ptr1-Ptr2,B3),
   alphaConvertVar(X,Var1,Y),
   alphaConvertDRS(B2,Var1-_,Ptr2-Ptr3,B4).

alphaConvertCondition(or(B1,B2),Var,Ptr1-Ptr3,or(B3,B4)):- !,
   alphaConvertDRS(B1,Var-_,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var-_,Ptr2-Ptr3,B4).

alphaConvertCondition(pred(Sym,Arg1),Var,Ptr-Ptr,pred(Sym,Arg2)):- !, % MG modified 2018
   alphaConvertVar(Arg1,Var,Arg2).

alphaConvertCondition(rel(Sym,Arg1,Arg2),Var,Ptr-Ptr,rel(Sym,Arg3,Arg4)):- !, % MG modified 2018
   alphaConvertVar(Arg1,Var,Arg3),
   alphaConvertVar(Arg2,Var,Arg4).

alphaConvertCondition(rel(Sym,Arg1,Arg2,Arg3),Var,Ptr-Ptr,rel(Sym,Arg4,Arg5,Arg6)):- !, %MG added 2020
   alphaConvertVar(Arg1,Var,Arg4),
   alphaConvertVar(Arg2,Var,Arg5),
   alphaConvertVar(Arg3,Var,Arg6).

% The above modify previous definitions of pred/2, pred/4, rel/3 and rel/4, which had a differen syntax

alphaConvertCondition(role(Arg1,Arg2,Sym,Dir),Var,Ptr-Ptr,role(Arg3,Arg4,Sym,Dir)):- !,
   alphaConvertVar(Arg1,Var,Arg3),
   alphaConvertVar(Arg2,Var,Arg4).

alphaConvertCondition(named(X,Sym),Var,Ptr-Ptr,named(Y,Sym)):- !,
   alphaConvertVar(X,Var,Y).

alphaConvertCondition(comp(X1,X2,CMP),Vars,Ptr-Ptr,comp(Y1,Y2,CMP)):- !,
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).

alphaConvertCondition(eq(X1,X2),Vars,Ptr-Ptr,eq(Y1,Y2)):- !, % MG modified 2018
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).
   
alphaConvertCondition(leq(X1,X2),Vars,Ptr-Ptr,leq(Y1,Y2)):- !, % MG modified 2018
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).

alphaConvertCondition(sub(X1,X2),Vars,Ptr-Ptr,sub(Y1,Y2)):- !, % MG modified 2018
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).

% Replace earlier definitions using comp/3

alphaConvertCondition(U,_,_,_):- !,
   warning('Unknown condition: ~p',[U]), fail.
