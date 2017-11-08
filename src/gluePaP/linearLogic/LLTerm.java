package gluePaP.linearLogic;


import Prover.Equality;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class LLTerm {

    public enum Type {
        E, T, COMPLEX
    }

    private String termId;
    private boolean polarity;
    private Type type;

    // experimental fields for compilation process
    public Set<LLTerm> assumptions = new HashSet<>();
    public Set<LLTerm> discharges = new HashSet<>();

    //Default constructor
    public LLTerm(){ }

    public String getTermId() {
        return termId;
    }

    void setTermId(String termId) { this.termId = termId;}

    public boolean isPolarity() { return polarity; }

    public void setPolarity(boolean pol) { this.polarity = pol; }

    public abstract boolean checkEquivalence(LLTerm term);

    public abstract List<Equality> checkCompatibility(LLTerm term);

    //Get and set type

    public Type getType(){
        return this.type;
    }

    public void setType(Type type) {
        this.type = type;
    }

}
