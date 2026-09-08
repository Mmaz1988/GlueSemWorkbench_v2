/*************************************************************************

         name: exampleModels.pl (Chapter 1)
      version: March 9, 1999
  description: Some example models defined over a vocabulary
      authors: Patrick Blackburn & Johan Bos
	  
	  
	  editiert: Modell selbst erstellt
 
*************************************************************************/

:- module(exampleModels,[example/2]).



/*========================================================================
   Example Models
========================================================================*/

example(1,model([d1,d2,d3,d4,d5,d6,d7,s1,s2,s3,s4,s5],
		[%individuals
		f(1,viking,[d1,d2]),
		f(1,dragon,[d3,d4,d5,d6]),
		f(1,cave,[d7]),
		%properties
		f(1,big,[d4]),
		f(1,small,[d5]),
		f(1,neuter,[d3,d4,d5,d6,d7]),
		f(1,male,[d1,d2]),
		%situations
		f(1,sleep,[s1]),
		f(1,dream,[s1]),
		f(1,meet,[s2,s4,s5]),
		f(1,tame,[s2,s4,s5]),
		f(1,roar,[s4]),
		f(1,event,[s1,s2,s3,s4,s5]),
		f(1,nonreflexive,[s2,s3,s4,s5]),
		%relations
		f(2,patient,[(s2,d6),(s5,d3),(s4,d4)]),
		f(2,agent,[(s1,d3),(s2,d1),(s5,d2),(s4,d1),(s4,d4)]),
		f(2,in,[(s1,d7)])
		])).





