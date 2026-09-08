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
    private Integer sourceIndex;
    private LLTerm llTerm;
    private SemanticRepresentation sem;

    private boolean isNonscope;
    private boolean isInsitu;

    private String stage = "0";

    public MeaningConstructor() {
    }

    public MeaningConstructor(Integer sourceIndex) {
        this.sourceIndex = sourceIndex;
    }


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

    public Integer getSourceIndex() {
        return sourceIndex;
    }

    public void setSourceIndex(Integer sourceIndex) {
        this.sourceIndex = sourceIndex;
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

    public boolean isInsitu() {
        return isInsitu;
    }

    public void setInsitu(boolean insitu) {
        isInsitu = insitu;
    }

    public String getStage() {
        return stage;
    }

    public void setStage(String stage) {
        this.stage = stage;
    }

    @Override
    public String toString() {
        return this.getSem().toString() + " : " + this.getLlTerm().toString();
    }

}
