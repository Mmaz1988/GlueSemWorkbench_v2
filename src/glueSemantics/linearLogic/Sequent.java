package glueSemantics.linearLogic;


import glueSemantics.lexicon.LexicalEntry;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Sequent {
    private List<Premise> lhs;
    private LLAtom rhs;
    private int idCounter;


    public List<Premise> getLhs() {
        return lhs;
    }



    public Sequent(List<LLTerm> parsedTerms,String msg) {
        lhs = new ArrayList<>();
        for (idCounter = 0; idCounter < parsedTerms.size(); idCounter++) {
            HashSet<Integer> idSet = new HashSet<>();
            idSet.add(idCounter);
            lhs.add(new Premise(idSet, parsedTerms.get(idCounter)));
        }
    }


     public Sequent(List<LexicalEntry> lexEn) {
         lhs = new ArrayList<>();
         for (idCounter = 0; idCounter < lexEn.size(); idCounter++) {
             HashSet<Integer> idSet = new HashSet<>();
             idSet.add(idCounter);
             lhs.add(new Premise(idSet, lexEn.get(idCounter)));
         }
     }

    // Returns the set containing all index sets (usually singletons) of the sequent's premises
    public HashSet<Integer> getMaxIDSet() {
        HashSet<Integer> maxIDSet = new HashSet<>();
        for (Premise premise : lhs) {
            maxIDSet.addAll(premise.getPremiseIDs());
        }
        return maxIDSet;
    }

    public HashSet<Integer> getNewID() {
        HashSet<Integer> newID = new HashSet<>();
        newID.add(idCounter++);
        return newID;
    }

    @Override
    public String toString() {
        return lhs + " => " +  "null";//rhs.toString();
    }


}