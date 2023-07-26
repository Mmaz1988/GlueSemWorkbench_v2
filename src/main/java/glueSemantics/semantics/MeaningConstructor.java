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

package glueSemantics.semantics;

import glueSemantics.linearLogic.LLTerm;

public class MeaningConstructor {

    String identifier;
    private LLTerm llTerm;
    private SemanticRepresentation sem;

    private boolean isNonscope;

    private int stage = 0;


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

    public enum LexType {

        //Verbs
        V_NULL,
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

    public boolean isNonscope() {
        return isNonscope;
    }

    public void setNonscope(boolean nonscope) {
        isNonscope = nonscope;
    }

    public int getStage() {
        return stage;
    }

    public void setStage(int stage) {
        this.stage = stage;
    }

}
