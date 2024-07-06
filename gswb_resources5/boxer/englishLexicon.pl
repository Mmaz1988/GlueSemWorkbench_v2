/*************************************************************************

    File: englishLexicon.pl
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

/*========================================================================
   Determiners
========================================================================*/

lexEntry(det,[syntax:[every],mood:decl,num:sg,type:uni]).
lexEntry(det,[syntax:[a],mood:decl,num:sg,type:indef]).
lexEntry(det,[syntax:[an],mood:decl,num:sg,type:indef]).
lexEntry(det,[syntax:[one],mood:decl,num:sg,type:indef]).
lexEntry(det,[syntax:[some],mood:decl,num:sg,type:indef]).
lexEntry(det,[syntax:[no],mood:decl,num:sg,type:neg]).
lexEntry(det,[syntax:[the],mood:decl,num:sg,type:def]).
lexEntry(det,[syntax:[his],mood:decl,num:sg,type:poss(male)]).
lexEntry(det,[syntax:[her],mood:decl,num:sg,type:poss(female)]).
lexEntry(det,[syntax:[its],mood:decl,num:sg,type:poss(neuter)]).

lexEntry(det,[syntax:[some],mood:decl,num:pl,type:atleast(one)]).
lexEntry(det,[syntax:[several],mood:decl,num:pl,type:atleast(two)]).
lexEntry(det,[syntax:[two],mood:decl,num:pl,type:atleast(two)]).
lexEntry(det,[syntax:[three],mood:decl,num:pl,type:atleast(three)]).
lexEntry(det,[syntax:[four],mood:decl,num:pl,type:atleast(four)]).
lexEntry(det,[syntax:[five],mood:decl,num:pl,type:atleast(five)]).
lexEntry(det,[syntax:[no],mood:decl,num:pl,type:neg]).
lexEntry(det,[syntax:[all],mood:decl,num:pl,type:uni]).
lexEntry(det,[syntax:[most],mood:decl,num:pl,type:most]).
lexEntry(det,[syntax:[the],mood:decl,num:pl,type:def]).



/*========================================================================
   Nouns
========================================================================*/

lexEntry(noun,[symbol:animal,num:sg,syntax:[animal]]).
lexEntry(noun,[symbol:animal,num:pl,syntax:[animals]]).
lexEntry(noun,[symbol:beverage,num:sg,syntax:[beverage]]).
lexEntry(noun,[symbol:beverage,num:pl,syntax:[beverages]]).
lexEntry(noun,[symbol:building,num:sg,syntax:[building]]).
lexEntry(noun,[symbol:building,num:pl,syntax:[buildings]]).
lexEntry(noun,[symbol:cup,num:sg,syntax:[cup]]).
lexEntry(noun,[symbol:cup,num:pl,syntax:[cups]]).
lexEntry(noun,[symbol:burger,num:sg,syntax:[burger]]).
lexEntry(noun,[symbol:burger,num:pl,syntax:[burgers]]).
lexEntry(noun,[symbol:boxer,num:sg,syntax:[boxer]]).
lexEntry(noun,[symbol:boxer,num:pl,syntax:[boxers]]).
lexEntry(noun,[symbol:boss,num:sg,syntax:[boss]]).
lexEntry(noun,[symbol:boss,num:pl,syntax:[bosses]]).
lexEntry(noun,[symbol:car,num:sg,syntax:[car]]).
lexEntry(noun,[symbol:car,num:pl,syntax:[cars]]).
lexEntry(noun,[symbol:chainsaw,num:sg,syntax:[chainsaw]]).
lexEntry(noun,[symbol:chainsaw,num:pl,syntax:[chainsaws]]).
lexEntry(noun,[symbol:criminal,num:sg,syntax:[criminal]]).
lexEntry(noun,[symbol:criminal,num:pl,syntax:[criminals]]).
lexEntry(noun,[symbol:customer,num:sg,syntax:[customer]]).
lexEntry(noun,[symbol:customer,num:pl,syntax:[customers]]).
lexEntry(noun,[symbol:drug,num:sg,syntax:[drug]]).
lexEntry(noun,[symbol:drug,num:pl,syntax:[drugs]]).
lexEntry(noun,[symbol:episode,num:sg,syntax:[episode]]).
lexEntry(noun,[symbol:episode,num:pl,syntax:[episodes]]).
lexEntry(noun,[symbol:fdshake,num:sg,syntax:[five,dollar,shake]]).
lexEntry(noun,[symbol:fdshake,num:pl,syntax:[five,dollar,shakes]]).
lexEntry(noun,[symbol:footmassage,num:sg,syntax:[foot,massage]]).
lexEntry(noun,[symbol:footmassage,num:pl,syntax:[foot,massages]]).
lexEntry(noun,[symbol:gimp,num:sg,syntax:[gimp]]).
lexEntry(noun,[symbol:gimp,num:pl,syntax:[gimps]]).
lexEntry(noun,[symbol:glass,num:sg,syntax:[glass]]).
lexEntry(noun,[symbol:glass,num:pl,syntax:[glasses]]).
lexEntry(noun,[symbol:gun,num:sg,syntax:[gun]]).
lexEntry(noun,[symbol:gun,num:pl,syntax:[guns]]).
lexEntry(noun,[symbol:hammer,num:sg,syntax:[hammer]]).
lexEntry(noun,[symbol:hammer,num:pl,syntax:[hammers]]).
lexEntry(noun,[symbol:hashbar,num:sg,syntax:[hash,bar]]).
lexEntry(noun,[symbol:hashbar,num:pl,syntax:[hash,bars]]).
lexEntry(noun,[symbol:person,num:sg,syntax:[person]]).
lexEntry(noun,[symbol:person,num:pl,syntax:[persons]]).
lexEntry(noun,[symbol:husband,num:sg,syntax:[husband]]).
lexEntry(noun,[symbol:husband,num:pl,syntax:[husbands]]).
lexEntry(noun,[symbol:joke,num:sg,syntax:[joke]]).
lexEntry(noun,[symbol:joke,num:pl,syntax:[jokes]]).
lexEntry(noun,[symbol:man,num:sg,syntax:[man]]).
lexEntry(noun,[symbol:man,num:pl,syntax:[men]]).
lexEntry(noun,[symbol:needle,num:sg,syntax:[needle]]).
lexEntry(noun,[symbol:needle,num:pl,syntax:[needles]]).
lexEntry(noun,[symbol:owner,num:sg,syntax:[owner]]).
lexEntry(noun,[symbol:owner,num:pl,syntax:[owners]]).
lexEntry(noun,[symbol:piercing,num:sg,syntax:[piercing]]).
lexEntry(noun,[symbol:piercing,num:pl,syntax:[piercings]]).
lexEntry(noun,[symbol:plant,num:sg,syntax:[plant]]).
lexEntry(noun,[symbol:plant,num:pl,syntax:[plants]]).
lexEntry(noun,[symbol:qpwc,num:sg,syntax:[quarter,pounder,with,cheese]]).
lexEntry(noun,[symbol:qpwc,num:pl,syntax:[quarter,pounders,with,cheese]]).
lexEntry(noun,[symbol:radio,num:sg,syntax:[radio]]).
lexEntry(noun,[symbol:radio,num:pl,syntax:[radioes]]).
lexEntry(noun,[symbol:restaurant,num:sg,syntax:[restaurant]]).
lexEntry(noun,[symbol:restaurant,num:pl,syntax:[restaurants]]).
lexEntry(noun,[symbol:robber,num:sg,syntax:[robber]]).
lexEntry(noun,[symbol:robber,num:pl,syntax:[robbers]]).
lexEntry(noun,[symbol:suitcase,num:sg,syntax:[suitcase]]).
lexEntry(noun,[symbol:suitcase,num:pl,syntax:[suitcases]]).
lexEntry(noun,[symbol:shotgun,num:sg,syntax:[shotgun]]).
lexEntry(noun,[symbol:shotgun,num:pl,syntax:[shotguns]]).
lexEntry(noun,[symbol:sword,num:sg,syntax:[sword]]).
lexEntry(noun,[symbol:sword,num:pl,syntax:[swords]]).
lexEntry(noun,[symbol:vehicle,num:sg,syntax:[vehicle]]).
lexEntry(noun,[symbol:vehicle,num:pl,syntax:[vehicles]]).
lexEntry(noun,[symbol:weapon,num:sg,syntax:[weapon]]).
lexEntry(noun,[symbol:weapon,num:pl,syntax:[weapons]]).
lexEntry(noun,[symbol:wife,num:sg,syntax:[wife]]).
lexEntry(noun,[symbol:wife,num:pl,syntax:[wifes]]).
lexEntry(noun,[symbol:woman,num:sg,syntax:[woman]]).
lexEntry(noun,[symbol:woman,num:pl,syntax:[women]]).


%hinzugefügtes Vokabular

lexEntry(noun,[symbol:viking,num:sg,syntax:[viking]]).
lexEntry(noun,[symbol:viking,num:pl,syntax:[vikings]]).
lexEntry(noun,[symbol:dragon,num:sg,syntax:[dragon]]).
lexEntry(noun,[symbol:dragon,num:pl,syntax:[dragons]]).
lexEntry(noun,[symbol:cave,num:sg,syntax:[cave]]).
lexEntry(noun,[symbol:cave,num:pl,syntax:[caves]]).



/*========================================================================
   Proper Names
========================================================================*/

lexEntry(pn,[symbol:butch,syntax:[butch]]).
lexEntry(pn,[symbol:esmarelda,syntax:[esmarelda]]).
lexEntry(pn,[symbol:honey_bunny,syntax:[honey,bunny]]).
lexEntry(pn,[symbol:jimmy,syntax:[jimmy]]).
lexEntry(pn,[symbol:jody,syntax:[jody]]).
lexEntry(pn,[symbol:jules,syntax:[jules]]).
lexEntry(pn,[symbol:lance,syntax:[lance]]).
lexEntry(pn,[symbol:marsellus,syntax:[marsellus]]).
lexEntry(pn,[symbol:marsellus,syntax:[marsellus,wallace]]).
lexEntry(pn,[symbol:marvin,syntax:[marvin]]).
lexEntry(pn,[symbol:mia,syntax:[mia]]).
lexEntry(pn,[symbol:mia,syntax:[mia,wallace]]).
lexEntry(pn,[symbol:pumpkin,syntax:[pumpkin]]).
lexEntry(pn,[symbol:thewolf,syntax:[the,wolf]]).
lexEntry(pn,[symbol:vincent,syntax:[vincent]]).
lexEntry(pn,[symbol:vincent,syntax:[vincent,vega]]).
lexEntry(pn,[symbol:yolanda,syntax:[yolanda]]).



/*========================================================================
   Quantified Noun Phrases
========================================================================*/

lexEntry(qnp,[symbol:person,syntax:[who],mood:int,type:wh]).
lexEntry(qnp,[symbol:thing,syntax:[what],mood:int,type:wh]).


/*========================================================================
   Intransitive Verbs
========================================================================*/

lexEntry(iv,[symbol:collapse,syntax:[collapse],inf:inf,num:sg]).
lexEntry(iv,[symbol:collapse,syntax:[collapses],inf:fin,num:sg]).
lexEntry(iv,[symbol:collapse,syntax:[collapse],inf:fin,num:pl]).

lexEntry(iv,[symbol:dance,syntax:[dance],inf:inf,num:sg]).
lexEntry(iv,[symbol:dance,syntax:[dances],inf:fin,num:sg]).
lexEntry(iv,[symbol:dance,syntax:[dance],inf:fin,num:pl]).

lexEntry(iv,[symbol:die,syntax:[die],inf:inf,num:_]).
lexEntry(iv,[symbol:die,syntax:[dies],inf:fin,num:sg]).
lexEntry(iv,[symbol:die,syntax:[die],inf:fin,num:pl]).

lexEntry(iv,[symbol:growl,syntax:[growl],inf:inf,num:sg]).
lexEntry(iv,[symbol:growl,syntax:[growls],inf:fin,num:sg]).
lexEntry(iv,[symbol:growl,syntax:[growl],inf:fin,num:pl]).

lexEntry(iv,[symbol:playairguitar,syntax:[play,air,guitar],inf:inf,num:sg]).
lexEntry(iv,[symbol:playairguitar,syntax:[plays,air,guitar],inf:fin,num:sg]).
lexEntry(iv,[symbol:playairguitar,syntax:[play,air,guitar],inf:fin,num:pl]).

lexEntry(iv,[symbol:smoke,syntax:[smoke],inf:inf,num:sg]).
lexEntry(iv,[symbol:smoke,syntax:[smokes],inf:fin,num:sg]).
lexEntry(iv,[symbol:smoke,syntax:[smoke],inf:fin,num:pl]).

lexEntry(iv,[symbol:snort,syntax:[snort],inf:inf,num:sg]).
lexEntry(iv,[symbol:snort,syntax:[snorts],inf:fin,num:sg]).
lexEntry(iv,[symbol:snort,syntax:[snort],inf:fin,num:pl]).

lexEntry(iv,[symbol:shriek,syntax:[shriek],inf:inf,num:sg]).
lexEntry(iv,[symbol:shriek,syntax:[shrieks],inf:fin,num:sg]).
lexEntry(iv,[symbol:shriek,syntax:[shriek],inf:fin,num:pl]).

lexEntry(iv,[symbol:walk,syntax:[walk],inf:inf,num:sg]).
lexEntry(iv,[symbol:walk,syntax:[walks],inf:fin,num:sg]).
lexEntry(iv,[symbol:walk,syntax:[walk],inf:fin,num:pl]).


%hinzugefügtes Vokabular

lexEntry(iv,[symbol:sleep,syntax:[sleep],inf:inf,num:sg]).
lexEntry(iv,[symbol:sleep,syntax:[sleeps],inf:fin,num:sg]).
lexEntry(iv,[symbol:sleep,syntax:[sleep],inf:fin,num:pl]).

lexEntry(iv,[symbol:roar,syntax:[roar],inf:inf,num:sg]).
lexEntry(iv,[symbol:roar,syntax:[roars],inf:fin,num:sg]).
lexEntry(iv,[symbol:roar,syntax:[roar],inf:fin,num:pl]).

lexEntry(iv,[symbol:dream,syntax:[dream],inf:inf,num:sg]).
lexEntry(iv,[symbol:dream,syntax:[dreams],inf:fin,num:sg]).
lexEntry(iv,[symbol:dream,syntax:[dream],inf:fin,num:pl]).


/*========================================================================
   Transitive Verbs
========================================================================*/

lexEntry(tv,[symbol:clean,syntax:[clean],inf:inf,num:sg]).
lexEntry(tv,[symbol:clean,syntax:[cleans],inf:fin,num:sg]).
lexEntry(tv,[symbol:clean,syntax:[clean],inf:fin,num:pl]).

lexEntry(tv,[symbol:drink,syntax:[drink],inf:inf,num:sg]).
lexEntry(tv,[symbol:drink,syntax:[drinks],inf:fin,num:sg]).
lexEntry(tv,[symbol:drink,syntax:[drink],inf:fin,num:pl]).

lexEntry(tv,[symbol:date,syntax:[date],inf:inf,num:sg]).
lexEntry(tv,[symbol:date,syntax:[dates],inf:fin,num:sg]).
lexEntry(tv,[symbol:date,syntax:[date],inf:fin,num:pl]).

lexEntry(tv,[symbol:discard,syntax:[discard],inf:inf,num:sg]).
lexEntry(tv,[symbol:discard,syntax:[discards],inf:fin,num:sg]).
lexEntry(tv,[symbol:discard,syntax:[discard],inf:fin,num:pl]).

lexEntry(tv,[symbol:eat,syntax:[eat],inf:inf,num:sg]).
lexEntry(tv,[symbol:eat,syntax:[eats],inf:fin,num:sg]).
lexEntry(tv,[symbol:eat,syntax:[eat],inf:fin,num:pl]).

lexEntry(tv,[symbol:enjoy,syntax:[enjoy],inf:inf,num:sg]).
lexEntry(tv,[symbol:enjoy,syntax:[enjoys],inf:fin,num:sg]).
lexEntry(tv,[symbol:enjoy,syntax:[enjoy],inf:fin,num:pl]).

lexEntry(tv,[symbol:hate,syntax:[hate],inf:inf,num:sg]).
lexEntry(tv,[symbol:hate,syntax:[hates],inf:fin,num:sg]).
lexEntry(tv,[symbol:hate,syntax:[hate],inf:fin,num:pl]).

lexEntry(tv,[symbol:have,syntax:[have],inf:inf,num:sg]).
lexEntry(tv,[symbol:have,syntax:[has],inf:fin,num:sg]).
lexEntry(tv,[symbol:have,syntax:[have],inf:fin,num:pl]).

lexEntry(tv,[symbol:kill,syntax:[kill],inf:inf,num:sg]).
lexEntry(tv,[symbol:kill,syntax:[kills],inf:fin,num:sg]).
lexEntry(tv,[symbol:kill,syntax:[kill],inf:fin,num:pl]).

lexEntry(tv,[symbol:know,syntax:[know],inf:inf,num:sg]).
lexEntry(tv,[symbol:know,syntax:[knows],inf:fin,num:sg]).
lexEntry(tv,[symbol:know,syntax:[know],inf:fin,num:pl]).

lexEntry(tv,[symbol:like,syntax:[like],inf:inf,num:sg]).
lexEntry(tv,[symbol:like,syntax:[likes],inf:fin,num:sg]).
lexEntry(tv,[symbol:like,syntax:[like],inf:fin,num:pl]).

lexEntry(tv,[symbol:love,syntax:[love],inf:inf,num:sg]).
lexEntry(tv,[symbol:love,syntax:[loves],inf:fin,num:sg]).
lexEntry(tv,[symbol:love,syntax:[love],inf:fin,num:pl]).

lexEntry(tv,[symbol:pickup,syntax:[pick,up],inf:inf,num:sg]).
lexEntry(tv,[symbol:pickup,syntax:[picks,up],inf:fin,num:sg]).
lexEntry(tv,[symbol:pickup,syntax:[pick,up],inf:fin,num:pl]).

lexEntry(tv,[symbol:shoot,syntax:[shot],inf:inf,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shot],inf:fin,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shoots],inf:fin,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shoot],inf:fin,num:pl]).


%hinzugefügtes Vokabular

lexEntry(tv,[symbol:meet,syntax:[meet],inf:inf,num:sg]).
lexEntry(tv,[symbol:meet,syntax:[meets],inf:fin,num:sg]).
lexEntry(tv,[symbol:meet,syntax:[meet],inf:fin,num:pl]).

lexEntry(tv,[symbol:tame,syntax:[tame],inf:inf,num:sg]).
lexEntry(tv,[symbol:tame,syntax:[tames],inf:fin,num:sg]).
lexEntry(tv,[symbol:tame,syntax:[tame],inf:fin,num:pl]).


/*========================================================================
   Copula
========================================================================*/

lexEntry(cop,[symbol:eq,syntax:[is],inf:fin,num:sg]).
lexEntry(cop,[symbol:eq,syntax:[are,not],inf:fin,num:pl]).

/*========================================================================
   Prepositions
========================================================================*/

lexEntry(prep,[symbol:about,syntax:[about]]).
lexEntry(prep,[symbol:in,syntax:[in]]).
lexEntry(prep,[symbol:of,syntax:[of]]).
lexEntry(prep,[symbol:with,syntax:[with]]).


/*========================================================================
   Adjectives
========================================================================*/

lexEntry(adj,[symbol:big,syntax:[big]]).
lexEntry(adj,[symbol:blue,syntax:[blue]]).
lexEntry(adj,[symbol:female,syntax:[female]]).
lexEntry(adj,[symbol:happy,syntax:[happy]]).
lexEntry(adj,[symbol:kahuna,syntax:[kahuna]]).
lexEntry(adj,[symbol:male,syntax:[male]]).
lexEntry(adj,[symbol:married,syntax:[married]]).
lexEntry(adj,[symbol:red,syntax:[red]]).
lexEntry(adj,[symbol:sad,syntax:[sad]]).
lexEntry(adj,[symbol:small,syntax:[small]]).
lexEntry(adj,[symbol:tall,syntax:[tall]]).



/*========================================================================
   Adverbs
========================================================================*/

lexEntry(adv,[symbol:quick,syntax:[quickly]]).
lexEntry(adv,[symbol:slow,syntax:[slowly]]).


/*========================================================================
   Relative Pronouns
========================================================================*/

lexEntry(relpro,[syntax:[who]]).
lexEntry(relpro,[syntax:[that]]).


/*========================================================================
   Coordinations
========================================================================*/

lexEntry(coord,[syntax:[and],type:conj]).
lexEntry(coord,[syntax:[or],type:disj]).


/*========================================================================
   Auxiliary Verbs
========================================================================*/

lexEntry(av,[syntax:[does],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[does,not],inf:fin,num:sg,pol:neg]).
lexEntry(av,[syntax:[do],inf:fin,num:pl,pol:pos]).
lexEntry(av,[syntax:[do,not],inf:fin,num:pl,pol:neg]).
lexEntry(av,[syntax:[did],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[did,not],inf:fin,num:sg,pol:neg]).
lexEntry(av,[syntax:[did],inf:fin,num:pl,pol:pos]).
lexEntry(av,[syntax:[did,not],inf:fin,num:pl,pol:neg]).


/*========================================================================
   Pronouns (third person)
========================================================================*/

lexEntry(pro,[symbol:male,  ref:no, syntax:[he]]).
lexEntry(pro,[symbol:male,  ref:no, syntax:[him]]).
lexEntry(pro,[symbol:male,  ref:yes,syntax:[himself]]).
lexEntry(pro,[symbol:female,ref:no, syntax:[she]]).
lexEntry(pro,[symbol:female,ref:yes,syntax:[herself]]).
lexEntry(pro,[symbol:female,ref:no, syntax:[her]]).
lexEntry(pro,[symbol:neuter,ref:no, syntax:[it]]).
lexEntry(pro,[symbol:neuter,ref:yes,syntax:[itself]]).
