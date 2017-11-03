package gluePaP.linearLogic;

import java.util.HashSet;
import java.util.Set;

public class Premise {
    private Set<Integer> premiseIDs;
    private LLTerm term;

    public Set<Integer> getPremiseIDs() {
        return premiseIDs;
    }

    public LLTerm getTerm() {
        return term;
    }

    public Premise(HashSet<Integer> premiseIDs, LLTerm llterm) {
        this.premiseIDs = premiseIDs;
        this.term = llterm;
        this.term.setPolarity(true);
    }

    @Override
    public String toString() {
        return term + "[" + premiseIDs + "]";
    }

    public boolean equals(Premise p) {
        return false;
    }
}
