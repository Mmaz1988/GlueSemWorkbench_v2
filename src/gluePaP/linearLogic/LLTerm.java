package gluePaP.linearLogic;


import Prover.AtomEquality;

import java.util.List;

public abstract class LLTerm {

    public enum Type {
        E, T, COMPLEX
    }

    private String termId;
    private boolean polarity;
    private boolean assumption;
    private Type type;

    //Default constructor
    public LLTerm(){ }

    public String getTermId() {
        return termId;
    }

    void setTermId(String termId) { this.termId = termId;}

    public boolean isPolarity() { return polarity; }

    public void setPolarity(boolean pol) { this.polarity = pol; }

    public abstract boolean checkEquivalence(LLTerm term);

    public boolean isAssumption() { return assumption; }

    public void setAssumption(boolean assumption) { this.assumption = assumption; }

    //Get and set type

    public Type getType(){
        return this.type;
    }

    public void setType(Type type) {
        this.type = type;
    }

}
