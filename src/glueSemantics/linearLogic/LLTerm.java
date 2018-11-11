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


import prover.Equality;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

public abstract class LLTerm {

    public enum Type {
        E, T,
    }

    private boolean polarity;
    private Type type;
    public Set<LLTerm> assumptions = new HashSet<>();
    public Set<LLTerm> discharges = new HashSet<>();

    //Default constructor
    public LLTerm(){ }


    public boolean isPolarity() { return polarity; }

    void setPolarity(boolean pol) { this.polarity = pol; }

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

    public abstract boolean isModifier();

    // This is not a regular clone() method, it just calls the copy constructor
    // of the respective class.
    public abstract LLTerm clone();

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


}
