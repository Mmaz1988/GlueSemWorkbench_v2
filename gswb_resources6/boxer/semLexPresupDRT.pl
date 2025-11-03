/*************************************************************************

    File: semLexPresupDRT.pl
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

*************************************************************************/

semLex(det,M):-
   M = [type:indef,
        num:sg,
        sem:lam(U,lam(V,merge(merge(drs([X],[]),app(U,X)),app(V,X))))].

semLex(det,M):-
   M = [type:uni,
        num:sg,
        sem:lam(U,lam(V,drs([],[imp(merge(drs([X],[]),app(U,X)),app(V,X))])))].

semLex(det,M):-
   M = [type:def,
        num:sg,
        sem:lam(U,lam(V,alfa(def,merge(drs([X],[]),app(U,X)),app(V,X))))].

semLex(det,M):-
   M = [type:neg,
        num:sg,
        sem:lam(U,lam(V,drs([],[not(merge(merge(drs([X],[]),app(U,X)),app(V,X)))])))].

semLex(det,M):-
   M = [type:poss(Symbol),
        num:sg,
        sem:lam(U,lam(V,alfa(pro,drs([Y],[pred(Symbol,Y)]),alfa(def,merge(drs([X],[rel(of,X,Y)]),app(U,X)),app(V,X)))))].

semLex(pn,M):-
   M = [symbol:Sym,
        sem:lam(P,alfa(nam,drs([X],[pred(Sym,X)]),app(P,X)))].

semLex(pro,M):-
   M = [symbol:Sym,
        sem:lam(P,alfa(pro,drs([X],[pred(Sym,X)]),app(P,X)))].

semLex(noun,M):-
   M = [symbol:Sym,
        sem:lam(X,drs([],[pred(Sym,X)]))].

semLex(iv,M):-
   M = [symbol:Sym,
        sem:lam(N,lam(P,app(N,lam(X,merge(drs([E],[pred(Sym,E),rel(agent,E,X)]),app(P,E))))))].



semLex(tv,M):-
   M = [symbol:Sym,ref:no,
        sem:lam(N1,lam(N2,lam(P,app(N2,lam(X,app(N1,lam(Y,merge(drs([E],[pred(Sym,E),rel(agent,E,X),rel(patient,E,Y),pred(nonreflexive,E)]),
                                                                app(P,E)))))))))];
   M = [symbol:Sym,ref:yes,
        sem:lam(N1,lam(N2,lam(P,app(N2,lam(X,app(N1,lam(Y,merge(drs([E],[pred(Sym,E),rel(agent,E,X),rel(patient,E,Y),pred(reflexive,E)]),
                                                                app(P,E)))))))))].


semLex(cop,M):-
   M = [pol:pos,
        sem:lam(K,lam(Y,app(K,lam(X,drs([],[eq(Y,X):[]])))))];
   M = [pol:neg,
        sem:lam(K,lam(Y,drs([],[not(app(K,lam(X,drs([],[eq(Y,X):[]]))))])))].



semLex(relpro,M):-
   M = [sem:lam(P,lam(Q,lam(X,merge(app(app(P,lam(R,app(R,X))),lam(E,drs([],[pred(event,E)]))),app(Q,X)))))].


semLex(prep,M):-
   M = [symbol:Sym,
        type:n,
        sem:lam(K,lam(P,lam(Y,merge(app(K,lam(X,drs([],[rel(Sym,Y,X)]))),app(P,Y)))))];
   M = [symbol:Sym,
        type:vp,
        sem:lam(K,lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(app(K,lam(Y,drs([],[rel(Sym,X,Y)]))),app(E,X))))))))].

semLex(adj,M):-
   M = [symbol:Sym,
        sem:lam(P,lam(X,merge(drs([],[pred(Sym,X)]),app(P,X))))].

semLex(adv,M):-
   M = [symbol:Sym,
        sem:lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(drs([],[pred(Sym,X)]),app(E,X)))))))].

semLex(av,M):-
   M = [pol:neg,
      sem:lam(P,lam(X,lam(E,drs([],[not(app(app(P,X),E))]))))];
   M = [pol:pos,
        sem:lam(P,lam(X,lam(E,app(app(P,X),E))))].

semLex(coord,M):-
   M = [type:conj,
        sem:lam(X,lam(Y,lam(P,merge(app(X,P),app(Y,P)))))];
   M = [type:disj,
        sem:lam(X,lam(Y,lam(P,drs([],[or(app(X,P),app(Y,P))]))))].






