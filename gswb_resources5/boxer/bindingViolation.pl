/*************************************************************************

    File: bindingViolation.pl
    Copyright (C) 2004,2006 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 2.0 (November 2006).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
	
	editiert: Binding Violation hinzugefügt (Z. 79)

*************************************************************************/

:- module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(comsemPredicates,[memberList/2]).


/*========================================================================
   DRS with no Binding Violation
========================================================================*/

bindingViolationDrs(drs(_,C)):-
   bindingViolationConds(C), !.

bindingViolationDrs(merge(B,_)):-
   bindingViolationDrs(B), !.

bindingViolationDrs(merge(_,B)):-
   bindingViolationDrs(B), !.

bindingViolationConds(C):-
   memberList(not(B),C),
   bindingViolationDrs(B), !.

bindingViolationConds(C):-
   memberList(imp(B,_),C), 
   bindingViolationDrs(B), !.

bindingViolationConds(C):-
   memberList(imp(_,B),C), 
   bindingViolationDrs(B), !.


bindingViolationConds(C):-
   memberList(or(B,_),C), 
   bindingViolationDrs(B), !.

bindingViolationConds(C):-
   memberList(or(_,B),C), 
   bindingViolationDrs(B), !.



bindingViolationConds(C):-
   member(pred(reflexive,E0),C),
   member(rel(agent,E1,X),C), E0==E1,
   member(rel(patient,E2,Y),C), E0==E2, 
   \+ X==Y.

bindingViolationConds(C):-
   member(pred(nonreflexive,E0),C),
   member(rel(agent,E1,X),C), E0==E1,
   member(rel(patient,E2,Y),C), E0==E2, 
   X==Y.

%binding violation für transitive Verben -> Verben können nicht Antezedens für Pronomen sein
bindingViolationConds(C):-
   member(pred(event,X),C),
   member(pred(neuter,Y),C), Y == X;
   member(pred(event,X),C),
   member(pred(male,Y),C), Y == X;
   member(pred(event,X),C),
   member(pred(female,Y),C), Y == X.



   

