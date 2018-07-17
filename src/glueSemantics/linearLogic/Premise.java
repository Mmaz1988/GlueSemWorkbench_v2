/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.linearLogic;

import glueSemantics.lexicon.LexicalEntry;
import glueSemantics.semantics.lambda.SemRepresentation;

import java.util.HashSet;

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
        if (glueTerm instanceof LLFormula &&
                (((LLFormula) glueTerm).getLhs().checkEquivalence(((LLFormula) glueTerm).getRhs())))
            setModifier(true);
        else
            setModifier(false);
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


    /**
     * Keeps track of all parents of a premise and is used to record a derivation history.
     * @param func
     * @param arg
     */
    public void setHistory(Premise func, Premise arg) {
        this.func = func;
        this.arg = arg;
    }

    public Premise getFunc(){
        return this.func;
    }

    public Premise getArg() {
        return this.arg;
    }

    public boolean isModifier() {
        return modifier;
    }

    public void setModifier(boolean modifier) {
        this.modifier = modifier;
    }
}
