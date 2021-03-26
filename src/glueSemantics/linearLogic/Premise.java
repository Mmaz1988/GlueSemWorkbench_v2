/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.linearLogic;

import glueSemantics.semantics.LexicalEntry;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemanticExpression;

import java.util.HashSet;
import java.util.LinkedList;

public class Premise {
    //Definitions for colored console output
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";


    private HashSet<Integer> premiseIDs;
    private LLTerm glueTerm;
    private SemanticRepresentation semTerm;

    private SemanticRepresentation originalSemTerm;
    private Object func;
    private Object arg;
    public boolean used;

    public HashSet<Integer> getPremiseIDs() {
        return premiseIDs;
    }

    public LLTerm getGlueTerm() {
        return glueTerm;
    }

    public void setGlueTerm(LLTerm glueTerm) {
        this.glueTerm = glueTerm;
    }

    public SemanticRepresentation getSemTerm() { return semTerm; }

    public void setSemTerm(SemanticExpression semTerm) { this.semTerm = semTerm; }

    private LinkedList<SemAtom> assumptionVars = new LinkedList<>();
    private LinkedList<SemAtom> binderVars = new LinkedList<>();

    public Premise(HashSet<Integer> premiseIDs, LLTerm llterm) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = llterm;
        this.glueTerm.setPolarity(true);
        this.used = false;
    }

    public Premise(HashSet<Integer> premiseIDs, SemanticRepresentation semTerm, LLTerm glueTerm) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = glueTerm;
        this.semTerm = semTerm;
        this.originalSemTerm = semTerm.clone();
        this.used = false;
    }

    public Premise(HashSet<Integer> premiseIDs, SemanticRepresentation semTerm, SemanticRepresentation originalSemTerm, LLTerm glueTerm) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = glueTerm;
        this.semTerm = semTerm;
        this.originalSemTerm = originalSemTerm;
        this.used = false;
    }

    //For work with Lexicon
    public Premise(HashSet<Integer> premiseIDs, LexicalEntry lexEn) {
        this.premiseIDs = premiseIDs;
        this.glueTerm = lexEn.getLlTerm();
        this.semTerm = lexEn.getSem();
        this.originalSemTerm = semTerm.clone();
        this.used = false;
    }


    @Override
    public String toString() {
        //return ANSI_BLUE + glueTerm + ANSI_RESET + " : " + ANSI_YELLOW + semTerm + ANSI_RESET +  premiseIDs;
        return glueTerm + " : " + semTerm; //
        // +  premiseIDs;
    }

    @Override
    public boolean equals(Object obj) {
        boolean result = this.premiseIDs.equals(((Premise) obj).premiseIDs);
        return result;
    }

    /**
     * Keeps track of all parents of a premise and is used to record a derivation history.
     * @param func
     * @param arg
     */
    public void setHistory(Object func, Object arg) {
        this.func = func;
        this.arg = arg;
    }

    public Object getFunc(){
        return this.func;
    }

    public Object getArg() {
        return this.arg;
    }

    public boolean isModifier() {
        return glueTerm.isModifier();
    }

    public SemanticRepresentation getOriginalSemTerm() {
        return originalSemTerm;
    }

    public void setOriginalSemTerm(SemanticRepresentation originalSemTerm) {
        this.originalSemTerm = originalSemTerm;
    }

    public LinkedList<SemAtom> getAssumptionVars() {
        return assumptionVars;
    }

    public void setAssumptionVars(LinkedList<SemAtom> assumptionVars) {
        this.assumptionVars = assumptionVars;
    }

    public LinkedList<SemAtom> getBinderVars() {
        return binderVars;
    }

    public void setBinderVars(LinkedList<SemAtom> binderVars) {
        this.binderVars = binderVars;
    }

}
