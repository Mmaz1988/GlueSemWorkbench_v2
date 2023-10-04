/*************************************************************************

    File: lexicalKnowledge.pl
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


	editiert: eigenes Vokabular hinzugefügt (gekennzeichnet)

*************************************************************************/

:- module(lexicalKnowledge,[lexicalKnowledge/3]).

/*========================================================================
   Axioms for Lexical Knowledge: Nouns
========================================================================*/

lexicalKnowledge(event,1,Axiom):-
   Axiom = all(A,imp(pred(event,A),pred(thing,A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(pred(entity,A),pred(thing,A))).

lexicalKnowledge(object,1,Axiom):- 
   Axiom = all(A,imp(pred(object,A),pred(entity,A))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(pred(organism,A),pred(entity,A))).

lexicalKnowledge(food,1,Axiom):- 
   Axiom = all(A,imp(pred(food,A),pred(object,A))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(pred(artifact,A),pred(object,A))).

lexicalKnowledge(building,1,Axiom):- 
   Axiom = all(A,imp(pred(building,A),pred(artifact,A))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(pred(instrument,A),pred(artifact,A))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(pred(animal,A),pred(organism,A))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(pred(person,A),pred(organism,A))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(pred(plant,A),pred(organism,A))).

lexicalKnowledge(man,1,Axiom):- 
   Axiom = all(A,imp(pred(man,A),pred(person,A))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(pred(woman,A),pred(person,A))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(pred(beverage,A),pred(food,A))).

lexicalKnowledge(foodstuff,1,Axiom):- 
   Axiom = all(A,imp(pred(foodstuff,A),pred(food,A))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(pred(container,A),pred(instrument,A))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(pred(device,A),pred(instrument,A))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(pred(cup,A),pred(container,A))).

lexicalKnowledge(glass,1,Axiom):- 
   Axiom = all(A,imp(pred(glass,A),pred(container,A))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(pred(burger,A),pred(foodstuff,A))).

lexicalKnowledge(qpwc,1,Axiom):- 
   Axiom = all(A,imp(pred(qpwc,A),pred(foodstuff,A))).

lexicalKnowledge(boxer,1,Axiom):- 
   Axiom = all(A,imp(pred(boxer,A),pred(person,A))).

lexicalKnowledge(boss,1,Axiom):- 
   Axiom = all(A,imp(pred(boss,A),pred(person,A))).

lexicalKnowledge(criminal,1,Axiom):- 
   Axiom = all(A,imp(pred(criminal,A),pred(person,A))).

lexicalKnowledge(customer,1,Axiom):- 
   Axiom = all(A,imp(pred(customer,A),pred(person,A))).

lexicalKnowledge(owner,1,Axiom):- 
   Axiom = all(A,imp(pred(owner,A),pred(person,A))).

lexicalKnowledge(robber,1,Axiom):- 
   Axiom = all(A,imp(pred(robber,A),pred(person,A))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(pred(vehicle,A),pred(instrument,A))).

lexicalKnowledge(car,1,Axiom):- 
   Axiom = all(A,imp(pred(car,A),pred(vehicle,A))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(pred(chainsaw,A),pred(device,A))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(pred(drug,A),pred(artifact,A))).

lexicalKnowledge(episode,1,Axiom):- 
   Axiom = all(A,imp(pred(episode,A),pred(event,A))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(pred(footmassage,A),pred(event,A))).

lexicalKnowledge(fdshake,1,Axiom):- 
   Axiom = all(A,imp(pred(fdshake,A),pred(beverage,A))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(pred(weapon,A),pred(instrument,A))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(pred(gun,A),weapon(A))).

lexicalKnowledge(hammer,1,Axiom):- 
   Axiom = all(A,imp(pred(hammer,A),pred(device,A))).

lexicalKnowledge(hashbar,1,Axiom):- 
   Axiom = all(A,imp(pred(hashbar,A),pred(building,A))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(pred(restaurant,A),pred(building,A))).

lexicalKnowledge(husband,1,Axiom):- 
   Axiom = all(A,imp(pred(husband,A),pred(man,A))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(pred(joke,A),pred(event,A))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(pred(needle,A),pred(device,A))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(pred(piercing,A),pred(artifact,A))).

lexicalKnowledge(radio,1,Axiom):- 
   Axiom = all(A,imp(pred(radio,A),pred(instrument,A))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(pred(suitcase,A),pred(container,A))).

lexicalKnowledge(shotgun,1,Axiom):- 
   Axiom = all(A,imp(pred(shotgun,A),pred(gun,A))).

lexicalKnowledge(sword,1,Axiom):- 
   Axiom = all(A,imp(pred(sword,A),pred(weapon,A))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(pred(wife,A),pred(woman,A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(pred(entity,A),not(pred(event,A)))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(pred(organism,A),not(pred(object,A)))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(pred(artifact,A),not(pred(food,A)))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(pred(person,A),not(pred(animal,A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(pred(plant,A),not(pred(animal,A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(pred(plant,A),not(pred(person,A)))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(pred(instrument,A),not(pred(building,A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(pred(drug,A),not(pred(building,A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(pred(piercing,A),not(pred(building,A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(pred(drug,A),not(pred(instrument,A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(pred(piercing,A),not(pred(instrument,A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(pred(piercing,A),not(drug(A)))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(pred(woman,A),not(pred(man,A)))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(pred(device,A),not(radio(A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(pred(container,A),not(pred(radio,A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(pred(vehicle,A),not(pred(radio,A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(pred(weapon,A),not(pred(radio,A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(pred(container,A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(pred(vehicle,A),not(device(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(pred(weapon,A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(pred(vehicle,A),not(pred(container,A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(pred(weapon,A),not(pred(container,A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(pred(weapon,A),not(pred(vehicle,A)))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(pred(beverage,A),not(pred(foodstuff,A)))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(pred(footmassage,A),not(pred(episode,A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(pred(joke,A),not(pred(episode,A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(pred(joke,A),not(pred(footmassage,A)))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(pred(cup,A),not(pred(glass,A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(pred(suitcase,A),not(pred(glass,A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(pred(suitcase,A),not(pred(cup,A)))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(pred(chainsaw,A),not(pred(hammer,A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(pred(needle,A),not(pred(hammer,A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(pred(needle,A),not(pred(chainsaw,A)))).

lexicalKnowledge(unmarried,1,Axiom):- 
   Axiom = all(A,imp(pred(unmarried,A),not(pred(married,A)))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(pred(burger,A),not(pred(qpwc,A)))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(pred(restaurant,A),not(pred(hashbar,A)))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(pred(gun,A),not(pred(sword,A)))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(pred(wife,A),pred(married,A))).

lexicalKnowledge(man,1,Axiom):- 
   Axiom = all(A,imp(pred(man,A),pred(male,A))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(pred(plant,A),pred(neuter,A))).

lexicalKnowledge(object,1,Axiom):- 
   Axiom = all(A,imp(pred(object,A),pred(neuter,A))).

lexicalKnowledge(event,1,Axiom):- 
   Axiom = all(A,imp(pred(event,A),pred(neuter,A))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(pred(woman,A),pred(female,A))).

lexicalKnowledge(male,1,Axiom):- 
   Axiom = all(A,imp(pred(male,A),not(pred(female,A)))).
   


%hinzugefügt:

lexicalKnowledge(dragon,1,Axiom):- 
   Axiom = all(A,imp(pred(dragon,A),pred(animal,A))).

lexicalKnowledge(viking,1,Axiom):- 
   Axiom = all(A,imp(pred(viking,A),pred(person,A))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(pred(animal,A),pred(neuter,A))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(pred(animal,A),not(pred(male,A)))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(pred(animal,A),not(pred(event,A)))).


lexicalKnowledge(cave,1,Axiom):- 
   Axiom = all(A,imp(pred(cave,A),pred(place,A))).

lexicalKnowledge(place,1,Axiom):- 
   Axiom = all(A,imp(pred(place,A),pred(artifact,A))).

lexicalKnowledge(place,1,Axiom):- 
   Axiom = all(A,imp(pred(place,A),not(pred(instrument,A)))).

lexicalKnowledge(place,1,Axiom):- 
   Axiom = all(A,imp(pred(place,A),not(pred(building,A)))).

lexicalKnowledge(place,1,Axiom):- 
   Axiom = all(A,imp(pred(place,A),not(pred(drug,A)))).



/*========================================================================
   Axioms for Lexical Knowledge: Proper Names
========================================================================*/

lexicalKnowledge(mia,1,Axiom):- 
   Axiom = all(A,imp(pred(mia,A),pred(woman,A))).

lexicalKnowledge(vincent,1,Axiom):- 
   Axiom = all(A,imp(pred(vincent,A),pred(man,A))).


/*========================================================================
   Axioms for Lexical Knowledge: Adjectives
========================================================================*/

lexicalKnowledge(red,1,Axiom):- 
   Axiom = all(A,imp(pred(red,A),not(pred(blue,A)))).

lexicalKnowledge(big,1,Axiom):- 
   Axiom = all(A,imp(pred(big,A),not(pred(small,A)))).

lexicalKnowledge(sad,1,Axiom):- 
   Axiom = all(A,imp(pred(sad,A),not(pred(happy,A)))).




/*========================================================================
   Axioms for Lexical Knowledge: Intransitive Verbs
========================================================================*/

lexicalKnowledge(collapse,1,Axiom):-
   Axiom = all(X,all(E,imp(and(pred(collapse,E),rel(agent,E,X)),
                           or(pred(person,X),pred(building,X))))).

lexicalKnowledge(dance,1,Axiom):- 
   Axiom = all(X,all(E,imp(and(pred(dance,E),rel(agent,E,X)),
                           pred(person,X)))).

lexicalKnowledge(die,1,Axiom):- 
   Axiom = all(X,all(E,imp(and(pred(die,E),rel(agent,E,X)),
                           pred(organism,X)))).

lexicalKnowledge(growl,1,Axiom):- 
   Axiom = all(X,all(E,imp(and(pred(growl,E),rel(agent,E,X)),
                           or(pred(animal,X),pred(person,X))))).

%hinzugefügt:

lexicalKnowledge(sleep,1,Axiom):- 
   Axiom = all(X,all(E,imp(and(pred(sleep,E),rel(agent,E,X)),
                           or(pred(animal,X),pred(person,X))))).

lexicalKnowledge(dream,1,Axiom):- 
   Axiom = all(X,all(E,imp(and(pred(dream,E),rel(agent,E,X)),
                           or(pred(animal,X),pred(person,X))))).




/*========================================================================
   Axioms for Lexical Knowledge: Transitive Verbs
========================================================================*/

lexicalKnowledge(clean,2,Axiom):- 
   Axiom = all(X,all(Y,all(E,imp(and(pred(clean,E),
                                     and(rel(agent,E,X),rel(patient,E,Y))),
                                 and(pred(person,X),pred(artifact,Y)))))).

lexicalKnowledge(drink,2,Axiom):- 
   Axiom = all(X,all(Y,all(E,imp(and(pred(drink,E),
                                     and(rel(agent,E,X),rel(patient,E,Y))),
                                 and(pred(person,X),pred(beverage,Y)))))).

lexicalKnowledge(eat,2,Axiom):- 
   Axiom = all(X,all(Y,all(E,imp(and(pred(eat,E),
                                     and(rel(agent,E,X),rel(patient,E,Y))),
                                 and(pred(person,X),pred(food,Y)))))).

%hinzugefügt

lexicalKnowledge(meet,2,Axiom):- 
   Axiom = all(X,all(Y,all(E,imp(and(pred(meet,E),
                                     and(rel(agent,E,X),rel(patient,E,Y))),
                                 or(and(pred(person,X),pred(animal,Y)),and(pred(person,X),pred(person,Y))))))).

lexicalKnowledge(tame,2,Axiom):- 
   Axiom = all(X,all(Y,all(E,imp(and(pred(tame,E),
                                     and(rel(agent,E,X),rel(patient,E,Y))),
                                and(pred(person,X),pred(animal,X)))))).







