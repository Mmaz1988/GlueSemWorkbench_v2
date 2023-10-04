/*************************************************************************

    File: modelCheckerTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
	
	
	editiert: eigene Testsätze

*************************************************************************/

:- module(modelDRTTestSuite,[test/5]).


/*========================================================================
   Check given formula in given model. 
   Correct answer recorded as third argument.
========================================================================*/

%A dragon sleeps in a cave. The dragon dreams
	
	%Lesart 1 (accomodation)
		test([a,dragon,sleeps,in,a,cave,the,dragon,dreams],
				drs([A,B,C,D,E],[pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(cave,C),rel(in,B,C),pred(event,B),
					pred(dragon,D),pred(dream,E),rel(agent,E,D),pred(event,E)]),
				accommodation,1,pos).

		
	%Lesart 2 (binding)
		test([a,dragon,sleeps,in,a,cave,the,dragon,dreams],
				drs([A,B,C,D],[pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(cave,C),rel(in,B,C),pred(event,B),
					pred(dream,D),rel(agent,D,A),pred(event,D)]),
				binding,1,pos).


%The dragon sleeps.

		test([the,dragon,sleeps],
				drs([A,B],[pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(event,B)]),
				accommodation,1,pos).


%The dragon does not sleep.

	%Lesart 1 (accommodation on local level) keine passende Interpretation -> Falsches Ergebnis im Präsuppositions-Test
		%drs([],[not(drs([A,B],[pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(event,B)]))])
		
	%Lesart 2 (accommodation on global level)
		test([the,dragon,does,not,sleep],
				drs([A],[pred(dragon,A),not(drs([B],[pred(sleep,B),rel(agent,B,A),pred(event,B)]))]),
				accommodation_on_global_level,1,pos).


%A dragon sleeps in a cave. It dreams.

	%Lesart 1 (binding von "it" mit "cave" -> ergibt keinen Sinn) 
		%drs([A,B,C,D],[pred(neuter,C),pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(cave,C),rel(in,B,C),
		%pred(event,B),pred(dream,D),rel(agent,D,C),pred(event,D)])
		
	%Lesart 2 (binding von "it" mit "dragon")
		test([a,dragon,sleeps,in,a,cave,it,dreams],
				drs([A,B,C,D],[pred(neuter,A),pred(dragon,A),pred(sleep,B),rel(agent,B,A),pred(cave,C),rel(in,B,C),pred(event,B),
					pred(dream,D),rel(agent,D,A),pred(event,D)]),
				binding,1,pos).


%It sleeps.
		test([it,sleeps],drs([],[]),uninterpretable,1,undef).


%If a viking meets a dragon, he tames the dragon.

	%Lesart 1 (local accommodation) ist semantisch sinnlos
		%drs([],[imp(drs([A,B,C],[pred(male,A),pred(viking,A),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),
		%pred(event,C)]),drs([D,E],[pred(dragon,D),pred(tame,E),rel(agent,E,A),rel(patient,E,D),pred(nonreflexive,E),pred(event,E)]))])
	
	%Lesart 2 (intermediate accommodation) ist semantich sinnlos
		%drs([],[imp(drs([A,B,C,D],[pred(male,A),pred(viking,A),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),
		%pred(event,C),pred(dragon,D)]),drs([E],[pred(tame,E),rel(agent,E,A),rel(patient,E,D),pred(nonreflexive,E),pred(event,E)]))])

	%Lesart 3 (Binding):
		test([if,a,viking,meets,a,dragon,he,tames,the,dragon],
				drs([],[imp(drs([A,B,C],[pred(male,A),pred(viking,A),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),
					pred(nonreflexive,C),pred(event,C)]),drs([D],[pred(tame,D),rel(agent,D,A),rel(patient,D,B),pred(nonreflexive,D),pred(event,D)]))]),
				binding,1,pos).

	%Lesart 4 (accomodation on global level):
		test([if,a,viking,meets,a,dragon,he,tames,the,dragon],
				drs([A],[pred(dragon,A),imp(drs([B,C,D],[pred(male,B),pred(viking,B),pred(dragon,C),pred(meet,D),rel(agent,D,B),rel(patient,D,C),
					pred(nonreflexive,D),pred(event,D)]),drs([E],[pred(tame,E),rel(agent,E,B),rel(patient,E,A),pred(nonreflexive,E),pred(event,E)]))]),
				accommodation_on_global_level,1,neg).
	
	
%If a viking meets a dragon, he tames it.

		test([if,a,viking,meets,a,dragon,he,tamess,it],
				drs([],[imp(drs([A,B,C],[pred(neuter,B),pred(male,A),pred(viking,A),pred(dragon,B),pred(meet,C),
					rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),pred(event,C)]),drs([D],[pred(tame,D),rel(agent,D,A),
					rel(patient,D,B),pred(nonreflexive,D),pred(event,D)]))]),
				binding,1,pos).


%Every viking who meets a dragon, tames the dragon.

	%Lesart 1 (local accommodation) ist semantisch sinnlos
		%drs([],[imp(drs([A,B,C],[pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),pred(event,C),pred(viking,A)]),
		%drs([D,E],[pred(dragon,D),pred(tame,E),rel(agent,E,A),rel(patient,E,D),pred(nonreflexive,E),pred(event,E)]))])
	
	%Lesart 2 (intermediate accommodation) ist semantich sinnlos
		%drs([],[imp(drs([A,B,C,D],[pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),pred(event,C),pred(viking,A),
		%pred(dragon,D)]),drs([E],[pred(tame,E),rel(agent,E,A),rel(patient,E,D),pred(nonreflexive,E),pred(event,E)]))])

	%Lesart 3 (Binding):
		test([every,viking,who,meets,a,dragon,tames,the,dragon],
				drs([],[imp(drs([A,B,C],[pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),pred(nonreflexive,C),pred(event,C),
					pred(viking,A)]),drs([D],[pred(tame,D),rel(agent,D,A),rel(patient,D,B),pred(nonreflexive,D),pred(event,D)]))]),
				binding,1,pos).

	%Lesart 4 (accomodation on global level):
		test([every,viking,who,meets,a,dragon,tames,the,dragon],
				drs([A],[pred(dragon,A),imp(drs([B,C,D],[pred(dragon,C),pred(meet,D),rel(agent,D,B),rel(patient,D,C),pred(nonreflexive,D),
					pred(event,D),pred(viking,B)]),drs([E],[pred(tame,E),rel(agent,E,B),rel(patient,E,A),pred(nonreflexive,E),pred(event,E)]))]),
				accommodation_on_global_level,1,neg).



%A viking meets a big dragon and a small dragon. The dragon roars.

	%Lesart 1 (accommodation)
		test([a,viking,meets,a,big,dragon,and,a,small,dragon,the,dragon,roars],
				drs([A,B,C,D,E,F,G],[pred(viking,A),pred(big,B),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),
					pred(nonreflexive,C),pred(event,C),pred(small,D),pred(dragon,D),pred(meet,E),rel(agent,E,A),rel(patient,E,D),
					pred(nonreflexive,E),pred(event,E),pred(dragon,F),pred(roar,G),rel(agent,G,F),pred(event,G)]),
				accommodation,1,pos).
	
	%Lesart 2 (binding, small dragon roars)
		test([a,viking,meets,a,big,dragon,and,a,small,dragon,the,dragon,roars],
				drs([A,B,C,D,E,F],[pred(viking,A),pred(big,B),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),
					pred(nonreflexive,C),pred(event,C),pred(small,D),pred(dragon,D),pred(meet,E),rel(agent,E,A),rel(patient,E,D),
					pred(nonreflexive,E),pred(event,E),pred(roar,F),rel(agent,F,D),pred(event,F)]),
				binding_small_dragon,1,neg).
		
	%Lesart 3 (binding, big dragon roars)
		test([a,viking,meets,a,big,dragon,and,a,small,dragon,the,dragon,roars],
				drs([A,B,C,D,E,F],[pred(viking,A),pred(big,B),pred(dragon,B),pred(meet,C),rel(agent,C,A),rel(patient,C,B),
					pred(nonreflexive,C),pred(event,C),pred(small,D),pred(dragon,D),pred(meet,E),rel(agent,E,A),rel(patient,E,D),
					pred(nonreflexive,E),pred(event,E),pred(roar,F),rel(agent,F,B),pred(event,F)]),
				binding_big_dragon,1,pos).
	


