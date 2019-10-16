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


import glueSemantics.semantics.lambda.SemType;
import prover.Equality;

import java.util.*;

public abstract class LLTerm {

    public enum Type {
        E, T,
    }

    private boolean polarity;
    private SemType semType;
    //public Set<LLTerm> assumptions = new HashSet<>();

    public Set<Premise> assumptions2 = new HashSet<>();

    //public Set<LLTerm> discharges = new HashSet<>();

    public LinkedList<Premise> orderedDischarges = new LinkedList<>();

    //Default constructor
    public LLTerm(){ }


    public boolean isPolarity() { return polarity; }

    void setPolarity(boolean pol) { this.polarity = pol; }

    public abstract boolean checkEquivalence(LLTerm term);

    public abstract LinkedHashSet<Equality> checkCompatibility(LLTerm term);

    public SemType getType(){
        return this.semType;
    }

    public abstract String category();


    public void setType(SemType type) {
        this.semType = type;
    }

    public String toPlainString() {
        return super.toString();
    }

    public abstract boolean isModifier();

    // This is not a regular clone() method, it just calls the copy constructor
    // of the respective class.
    public abstract LLTerm clone();


 /*TODO update this method
    String printAssumptions() {
        StringBuilder sb = new StringBuilder();
        ArrayList<LLTerm> as = new ArrayList<>(assumptions);
        sb.append("{");
        for (int i = 0; i < as.size(); i++) {
            sb.append(as.get(i).toPlainString());
            if (i+1 < as.size())
                sb.append(",");
        }
        sb.append("}");
        return sb.toString();
    }
*/

 /*
    public Set<LLTerm> getDischarges() {
        return discharges;
    }

    public void setDischarges(Set<LLTerm> discharges) {
        this.discharges = discharges;
    }
*/

    public LinkedList<Premise> getOrderedDischarges() {
        return orderedDischarges;
    }

    public void setOrderedDischarges(LinkedList<Premise> orderedDischarges) {
        this.orderedDischarges = orderedDischarges;
    }


    public Set<Premise> getAssumptions2() {
        return assumptions2;
    }

    public void setAssumptions2(Set<Premise> assumptions2) {
        this.assumptions2 = assumptions2;
    }
}
