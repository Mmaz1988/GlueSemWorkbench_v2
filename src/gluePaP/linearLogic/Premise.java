package gluePaP.linearLogic;

import gluePaP.lexicon.LexicalEntry;
import gluePaP.semantics.SemRepresentation;

import java.util.HashSet;
import java.util.Set;

public class Premise {
    //Definitions for colored console output
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";


    private HashSet<Integer> premiseIDs;
    private LLTerm glueTerm;
    private boolean modifier;
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
        if (glueTerm instanceof LLFormula &&
                (((LLFormula) glueTerm).getLhs().checkEquivalence(((LLFormula) glueTerm).getRhs())))
            setModifier(true);
        else
            setModifier(false);
    }

    //For work with Lexicon
    public Premise(HashSet<Integer> premiseIDs, LexicalEntry lexEn) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = lexEn.getLlTerm();
        this.semTerm = lexEn.getSem();
        if (glueTerm instanceof LLFormula &&
                (((LLFormula) glueTerm).getLhs().checkEquivalence(((LLFormula) glueTerm).getRhs())))
            setModifier(true);
        else
            setModifier(false);
    }


    @Override
    public String toString() {
        //return ANSI_BLUE + glueTerm + ANSI_RESET + " : " + ANSI_YELLOW + semTerm + ANSI_RESET +  premiseIDs;
        return glueTerm + " : " + semTerm +  premiseIDs;
    }




    /*
    This method keeps track of all the previous derivation steps of a premise.
    Not required, only for testing purposes
    */
    public void setHistory(Premise func, Premise arg) {
        this.func = func;
        this.arg = arg;
    }

    public boolean isModifier() {
        return modifier;
    }

    public void setModifier(boolean modifier) {
        this.modifier = modifier;
    }
}
