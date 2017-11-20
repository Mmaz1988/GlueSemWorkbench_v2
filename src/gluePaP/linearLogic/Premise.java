package gluePaP.linearLogic;

import gluePaP.semantics.SemRepresentation;

import java.util.HashSet;
import java.util.Set;

public class Premise {
    private HashSet<Integer> premiseIDs;
    private LLTerm glueTerm;
    private SemRepresentation semTerm;
    private Premise func;
    private Premise arg;

    public HashSet<Integer> getPremiseIDs() {
        return premiseIDs;
    }

    public LLTerm getGlueTerm() {
        return glueTerm;
    }

    public void setGlueTerm(LLTerm glueTerm) {
        this.glueTerm = glueTerm;
    }

    public SemRepresentation getSemTerm() { return semTerm; }

    public void setSemTerm(SemRepresentation semTerm) { this.semTerm = semTerm; }


    public Premise(HashSet<Integer> premiseIDs, LLTerm llterm) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = llterm;
        this.glueTerm.setPolarity(true);
    }

    public Premise(HashSet<Integer> premiseIDs, SemRepresentation semTerm, LLTerm glueTerm) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = glueTerm;
        this.semTerm = semTerm;
    }

    @Override
    public String toString() {
        return glueTerm + " : " + semTerm + "[" + premiseIDs + "]";
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
