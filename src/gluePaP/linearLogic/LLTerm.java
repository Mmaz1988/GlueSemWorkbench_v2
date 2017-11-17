package gluePaP.linearLogic;


import Prover.Equality;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

public abstract class LLTerm {

    public enum Type {
        E, T,
    }

    private String termId;
    private boolean polarity;
    private Type type;
    public Set<LLTerm> assumptions = new HashSet<>();
    public Set<LLTerm> discharges = new HashSet<>();

    //Default constructor
    public LLTerm(){ }

    public LLTerm(LLTerm term) {
        this.assumptions = term.assumptions;
        this.discharges = term.discharges;
        this.termId = term.termId;
        this.type = term.type;
        this.polarity = term.polarity;

    }

    public String getTermId() {
        return termId;
    }

    void setTermId(String termId) { this.termId = termId;}

    public boolean isPolarity() { return polarity; }

    public void setPolarity(boolean pol) { this.polarity = pol; }

    public abstract boolean checkEquivalence(LLTerm term);

    public abstract LinkedHashSet<Equality> checkCompatibility(LLTerm term);

    public Type getType(){
        return this.type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String toPlainString() {
        return super.toString();
    }
}
