package gluePaP.linearLogic;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Sequent {
    private List<Premise> lhs;
    private LLAtom rhs;

    public List<Premise> getLhs() {
        return lhs;
    }

    public Sequent(List<LLTerm> parsedTerms) {
        lhs = new ArrayList<>();
        for (int i = 0; i < parsedTerms.size(); i++) {
            HashSet<Integer> idSet = new HashSet<>();
            idSet.add(i);
            lhs.add(new Premise(idSet,parsedTerms.get(i)));
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

    @Override
    public String toString() {
        return lhs + " => " +  "null";//rhs.toString();
    }

    /*    public String toString() {
        String toString = lhs.get(0).toString();
        for (int i = 1; i < lhs.size(); i++) {
            toString = toString + ", " + lhs.get(i).toString();
        }
        return toString + "=>" + rhs.toString();
    }*/


}