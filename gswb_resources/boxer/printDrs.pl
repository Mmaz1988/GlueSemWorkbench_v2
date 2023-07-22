/*************************************************************************

    File: printDrs.pl
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

    Small modifications 2018-2020 by Matthew Gotham, noted in comments
	
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(printDrs,[printDrs/1]).

:- use_module(library(lists),[append/3]). % MG added 2018


/*========================================================================
     Counter for discourse referents
========================================================================*/

:- dynamic counter/1.

counter(0).
   

/*========================================================================
     Print Predicates
========================================================================*/

printDrs(Drs):- 
   retract(counter(_)), 
   assert(counter(1)),
   \+ \+ (formatDrs(Drs,Lines,_), 
          printDrsLines(Lines)).


/*========================================================================
     Print DRS Lines
========================================================================*/

printDrsLines([]):- nl.

printDrsLines([Line|Rest]):-
   name(L,Line), 
   nl, write(L),
   printDrsLines(Rest).


/*========================================================================
     Format DRSs
========================================================================*/

formatDrs(drs(Dom,Cond),[[32|Top],Refs2,[124|Line]|CondLines2],Length):-
   formatConds(Cond,[]-CondLines1,0-CondLength),
   formatRefs(Dom,Refs),
   length([_,_|Refs],RefLength),
   (RefLength > CondLength, !, Length = RefLength ; Length = CondLength), 
   closeConds(CondLines1,CondLines2,Length),
   Dif is (Length - RefLength) + 1,
   closeLine([124|Refs],Refs2,Dif,[124]),
   formatLine(95,Length,[32]-Top),
   formatLine(45,Length,[124]-Line).

formatDrs(merge(Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,';',M1,M2),
   Length is N1 + N2 + 4.

formatDrs(alfa(_,Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,'A',M1,M2),
   Length is N1 + N2 + 4.


/*========================================================================
     Format Discourse Referents
========================================================================*/

formatRefs([],[]).

formatRefs([Ref|Rest],Out):-
   makeConstant(Ref,Code), 
   formatRefs(Rest,Temp),
   append([32|Code],Temp,Out).


/*========================================================================
   Turn a discourse referent into a Prolog constant
========================================================================*/

makeConstant(X,Code):- 
   atomic(X),
   name(X,Code).

makeConstant(X,[120|Codes]):-
   nonvar(X),
   X =.. ['$VAR',Number],
   atomic(Number),
   name(Number,Codes).

makeConstant(X,[C|Odes]):-
   nonvar(X),
   X =.. ['$VAR',[C|Odes]].

makeConstant(X,[120|Number]):- 
   var(X),
   retract(counter(N)),
   name(N,Number), 
   name(X,[120|Number]),
   M is N+1,
   assert(counter(M)).


/*========================================================================
     Format a Line
========================================================================*/

formatLine(_,1,L-L):- !.

formatLine(Code,N,In-[Code|Out]):-
   M is N - 1, 
   formatLine(Code,M,In-Out).


/*========================================================================
     Formatting Conditions
========================================================================*/

formatConds([],L-L,N-N).

formatConds([imp(Drs1,Drs2)|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combLinesConds(Lines1,Lines2,Lines3,' ==> ',M), % Unicode decimal: [32,8658,32]
   append(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([or(Drs1,Drs2)|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 5,
   combLinesConds(Lines1,Lines2,Lines3,' V ',M), % Unicode decimal: [32,8744,32]
   append(Lines3,Lines,L2),
   Length is N1 + N2 + 8,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([duplex(Det,Drs1,Var,Drs2)|Rest],L1-L2,N0-N4):-!, % MG added 2020
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   atom_concat(' <',Det,Op1),
   atom_concat(Op1,' ',Op2),
   makeConstant(Var,VarCodes),
   atom_codes(Marker,VarCodes),
   atom_concat(Op2,Marker,Op3),
   atom_concat(Op3,'> ',Op),
   write_length(Op,Len,[]),
   M is N1 + Len + 2,
   combLinesConds(Lines1,Lines2,Lines3,Op,M),
   append(Lines3,Lines,L2),
   Length is N1 + N2 + Len + 5,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([not(Drs)|Rest],L1-L2,N0-N3):-!, % MG modified 2020
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,3,''),
   append([[124,32,32,32|A],
                [124,32,32,32|B],
                [124,32,32,32|C],
                [124,32,172,32|D]|Lines2],Lines,L2), % ¬ is 172, ~ is 126
   Length is N2 + 6,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([nec(Drs)|Rest],L1-L2,N0-N3):-!, % MG added 2020
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,4,''),
   append([[124,32,32,32,32|A],
                [124,32,32,32,32|B],
                [124,32,32,32,32|C],
                [124,32,91,93,32|D]|Lines2],Lines,L2),
   Length is N2 + 7,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([pos(Drs)|Rest],L1-L2,N0-N3):-!, % MG added 2020
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,4,''),
   append([[124,32,32,32,32|A],
                [124,32,32,32,32|B],
                [124,32,32,32,32|C],
                [124,32,60,62,32|D]|Lines2],Lines,L2),
   Length is N2 + 7,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([prop(Marker,Drs)|Rest],L1-L2,N0-N3):-!, % MG added 2018
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,6,''),
   makeConstant(Marker,Var),
   append([[124,32,32,32,32,32,32|A],		% | ...
                [124,32,32,32,32,32,32|B],	% | ...
                Prelist|Lines2],Lines,L2),	% | xN : ...
	append([124,32|Var],[32,58,32|C],Prelist), % (makes the line above)
   Length is N2 + 9,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([eq(A,B)|Rest],In-[[124,32|Line]|Out],N0-N2):-!,
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A,L1),
   makeConstant(B,L2),
   append(L1,[32,61,32|L2],Line), % =
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([leq(A,B)|Rest],In-[[124,32|Line]|Out],N0-N2):-!,	% MG added 2018, modified 2020
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A,L1),
   makeConstant(B,L2),
   append(L1,[32,8804,32|L2],Line), % Unicode decimal, =< is 61,60
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([sub(A,B)|Rest],In-[[124,32|Line]|Out],N0-N2):-!, %  MG added 2018, modified 2020
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A,L1),
   makeConstant(B,L2),
   append(L1,[32,8838,32|L2],Line), % Unicode decimal, C is 67
   length([_,_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic=pred(_,_),
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic=rel(_,_,_),
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):- % MG added 2020
   Basic=rel(_,_,_,_),
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic=named(_,_),
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).


/*========================================================================
     Formatting Basic Conditions
========================================================================*/

formatBasic(pred(Functor,Arg),Line):-
   name(Functor,F),
   makeConstant(Arg,A),   
   append(F,[40|A],T),
   append(T,[41],Line).
   
formatBasic(rel(Functor,Arg1,Arg2),Line):-
   name(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append(F,[40|A1],T1),
   append(T1,[44|A2],T2),
   append(T2,[41],Line).
 
formatBasic(rel(Functor,Arg1,Arg2,Arg3),Line):- % MG added 2020
   name(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   makeConstant(Arg3,A3),
   append(F,[40|A1],T1),
   append(T1,[44|A2],T2),
   append(T2,[44|A3],T3),
   append(T3,[41],Line).
 
formatBasic(named(Var,Name),Line):- % MG added 2018
	string_codes("Named",NamedList),
   name(Name,N),
   makeConstant(Var,V),
   append(NamedList,[40|V],T1),
   append(T1,[44|N],T2),
   append(T2,[41],Line).

/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/
    
combLinesConds([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N):-
   combLinesConds2([A1,B1,C1],[A2,B2,C2],Firsts,N,Op),
   name(Op,Code),
   append(Code,D2,T),
   append([124,32|D1],T,D),
   combLinesConds2(Rest1,Rest2,Rest,N,Op),
   append(Firsts,[D|Rest],Result).


/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/

times(0,_,[]). % MG added 2020

times(M,X,[X|L]) :- M > 0, N is M - 1, times(N,X,L). % MG added 2020

combLinesConds2([],[],[],_,_).

combLinesConds2([],[A2|Rest2],[A|Rest],N,Op):-
   closeLine([124],A1,N,[]),
   append(A1,A2,A),
   combLinesConds2([],Rest2,Rest,N,Op).

combLinesConds2([A1|Rest1],[],[[124,32|A1]|Rest],N,Op):-
   combLinesConds2(Rest1,[],Rest,N,Op).

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,Op):- % MG modified 2020
   write_length(Op,Len,[]),
   times(Len,32,Blanks),
   append(Blanks,A2,A3),
   append([124,32|A1],A3,A),
   combLinesConds2(Rest1,Rest2,Rest,N,Op).

% The above replaces special case definitions for ==> and V 

/*========================================================================
   Combining Lines of Characters (Complex DRSs)
========================================================================*/
    
combLinesDrs([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N1,N2):-
   combLinesDrs([A1,B1,C1],[A2,B2,C2],Firsts,N1,N2),
   name(Op,Code),
   append(Code,D2,T1),
   append(T1,[41],T2),
   append([40|D1],T2,D),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2),
   append(Firsts,[D|Rest],Result).

combLinesDrs([],[],[],_,_).

combLinesDrs([],[A2|Rest2],[A3|Rest],N1,N2):-
   closeLine([],A1,N1,[]),
   append(A1,A2,A0),
   append(A0,[32],A3),
   combLinesDrs([],Rest2,Rest,N1,N2).

combLinesDrs([A1|Rest1],[],[Closed|Rest],N1,N2):-
   combLinesDrs(Rest1,[],Rest,N1,N2),
   closeLine([32|A1],Closed,N2,[]).

combLinesDrs([A1|Rest1],[A2|Rest2],[A3|Rest],N1,N2):-
   append([32|A1],[32|A2],A0),
   append(A0,[32],A3),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2).


/*========================================================================
     Close Conditions (add '|')
========================================================================*/

closeConds([],[[124|Bottom]],Length):-
   formatLine(95,Length,[124]-Bottom).

closeConds([Line|Rest1],[New|Rest2],Length):-
   length(Line,L),
   R is Length - L,
   closeLine(Line,New,R,[124]),
   closeConds(Rest1,Rest2,Length).


/*========================================================================
     Close Line
========================================================================*/

closeLine(Line,New,N,Accu):- 
   N < 1, !, 
   append(Line,Accu,New).

closeLine(Line,New,N,Accu):- 
   M is N - 1, 
   closeLine(Line,New,M,[32|Accu]).

