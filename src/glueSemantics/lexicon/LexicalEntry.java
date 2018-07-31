/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.lexicon;

import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.SemanticExpression;

public class LexicalEntry {

    String identifier;

    public LLTerm getLlTerm() {
        return llTerm;
    }

    public void setLlTerm(LLTerm llTerm) {
        this.llTerm = llTerm;
    }

    public SemanticRepresentation getSem() {
        return sem;
    }

    public void setSem(SemanticRepresentation sem) {
        this.sem = sem;
    }

    private LLTerm llTerm;
    private SemanticRepresentation sem;

    public enum LexType {

        //Verbs
        V_INTR,
        V_TRANS,
        V_DTRAN,
        V_COMP,
        V_XCOMP,

        //Nouns
        N_NN,
        N_NNP,
        N_DP,

        //Determiner
        DET,
        //modifiers
        MOD

    }

}
