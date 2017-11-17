package gluePaP.linearLogic;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Premise {
    private Set<Integer> premiseIDs;
    private LLTerm term;
    private Premise func;
    private Premise arg;

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

    public void setTerm(LLTerm term) {
        this.term = term;
    }


    /*
    This method keeps track of all the previous derivation steps of a premise.
    Not required, only for testing purposes
    */
    public void setHistory(Premise func, Premise arg) {
        this.func = func;
        this.arg = arg;
    }
}
