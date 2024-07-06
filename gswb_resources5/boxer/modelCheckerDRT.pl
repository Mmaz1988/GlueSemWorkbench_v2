/*************************************************************************

         name: modelDRT.pl (Chapter 1)
      version: July 24, 1997
  description: Model Evaluation for DRSs
      authors: Patrick Blackburn & Johan Bos
 
 
	editiert: angepasste Darstellung der TestSuite Ergebnisse (Z. 104)
 
*************************************************************************/

:- module(modelCheckerDRT,[evaluate/2,modelCheckerTestSuite/0]).
:- use_module(comsemPredicates,[compose/3,member/2,printRepresentations/1]),
   use_module(exampleModels,[example/2]).
:- use_module(modelDRTTestSuite,[test/5]).


/*========================================================================
   Semantic Interpretation
========================================================================*/



satisfy(Drs,Model,pos) :- satisfyDrs(Drs,Model).
satisfy(Drs,Model,neg) :- \+ satisfyDrs(Drs,Model).


satisfyDrs(drs(Dom,C),Model):-
   assignReferents(Dom,Model),
   satisfyConditions(C,Model).

assignReferents([],_).
assignReferents([Referent|Others],model(D,_)):-
   member(Referent,D),
   assignReferents(Others,model(D,_)).
   
   
satisfyConditions([],_).
satisfyConditions([Condition|Others],Model):-
   satisfyCondition(Condition,Model),
   satisfyConditions(Others,Model).

satisfyCondition(not(Drs),Model):-
  \+ satisfyDrs(Drs,Model).

satisfyCondition(imp(Drs1,Drs2),Model):-
   (
    satisfyDrs(Drs1,Model),
    \+ satisfyDrs(Drs2,Model), 
    !, 
    fail
   ;
    true
   ).

satisfyCondition(or(Drs1,Drs2),Model):-
   (
    satisfyDrs(Drs1,Model)
   ;
    satisfyDrs(Drs2,Model)
   ).

satisfyCondition(eq(X,Y),model(_,F)):-
   (atom(Y),
   member(f(0,Y,X),F))
   ,!
   ;
   X=Y.

satisfyCondition(pred(Sym,Arg),model(_,F)):-
   member(f(1,Sym,Entities),F),
   member(Arg,Entities).

 satisfyCondition(rel(Sym,Arg1,Arg2),model(_,F)):-
   member(f(2,Sym,Entities),F),
   member((Arg1,Arg2),Entities).




/*========================================================================
   Evaluation
========================================================================*/

evaluate(Drs,Example):-
   example(Example,Model),
   satisfy(Drs,Model,Res),
   printStatus(Res).

/*========================================================================
   Print status of a testsuite example
========================================================================*/

printStatus(pos):- write('Satisfied in model. ').
printStatus(neg):- write('Not satisfied in model. ').




/*========================================================================
   Test Suite
========================================================================*/
%angepasste Darstellung der TestSuite Ergebnisse:

modelCheckerTestSuite:-
   format('~n>>>>> DRT modelChecker TEST SUITE <<<<<~n',[]),
   test(Discourse,Formula,Reading,Example,Status),
   format('~n~nDicourse:',[]),
   write(Discourse),
   format('~nDRS:',[]),  
   write(Formula),
   format('~nReading:',[]),
   write(Reading),
   format('~nExample Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   format('~nModel Checker says: ',[]),
   evaluate(Formula,Example),
   fail.

modelCheckerTestSuite.


/*========================================================================
   Info
========================================================================*/
/*
info:-
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n> modelDRT.pl, by Patrick Blackburn and Johan Bos                      <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- evaluate(F,E).          - evaluate a formula in a model                <',[]),
   format('~n> ?- modelCheckerTestSuite.  - run the test suite                           <',[]),
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).
*/

/*========================================================================
   Display info at start
========================================================================*/
/*
:- info.
*/
