package gluePaP.linearLogic;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Sequent {
    private List<Premise> lhs;
    private LLAtom rhs;
    private int idCounter;


    public List<Premise> getLhs() {
        return lhs;
    }

    public Sequent(List<LLTerm> parsedTerms) {
        lhs = new ArrayList<>();
        for (idCounter = 0; idCounter < parsedTerms.size(); idCounter++) {
            HashSet<Integer> idSet = new HashSet<>();
            idSet.add(idCounter);
            lhs.add(new Premise(idSet,parsedTerms.get(idCounter)));
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

    public Integer getNewID() {
        return idCounter++;
    }

    @Override
    public String toString() {
        return lhs + " => " +  "null";//rhs.toString();
    }


}