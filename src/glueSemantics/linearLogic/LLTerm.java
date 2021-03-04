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

import javax.annotation.processing.ProcessingEnvironment;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class LLTerm {


    public enum Type {
        E, T,
    }

    private boolean polarity;
    private SemType semType;
    //public Set<LLTerm> assumptions = new HashSet<>();

    public List<Premise> assumptions2 = new ArrayList<>();

    //public Set<LLTerm> discharges = new HashSet<>();

    public LinkedHashMap<Integer,Premise> orderedDischarges = new LinkedHashMap<>();

    private List<LLAtom> variable;

    //Compilation history
    private List<LLTerm> cHistory;

    private HashMap<LLAtom,LLAtom> variableAssignment = new HashMap<>();

    //Default constructor
    public LLTerm() {
    }


    public LLTerm(List<LLAtom> var) {
        this.variable = var;
    }

    public boolean isPolarity() {
        return polarity;
    }

    void setPolarity(boolean pol) {
        this.polarity = pol;
    }

    public abstract boolean checkEquivalence(LLTerm term);

    public abstract LinkedHashSet<Equality> checkCompatibility(LLTerm term);

    public SemType getType() {
        return this.semType;
    }

    public abstract Category category();

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

    public abstract Set<Category> returnAllCategories();



    public abstract List<LLAtom> returnAllAtoms();

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

    /*
   Represents the binder relation between the quantifier
   and the variables in the scope of the quantifier
   */
    public List<LLAtom> findBoundOccurrences(LLTerm term, LLAtom var) {


        // Variables that are equivalent are bound by the quantifier
        if (term instanceof LLAtom) {
            if (((LLAtom) term).getLLtype() == LLAtom.LLType.VAR) {

                if (var.checkEquivalence(term)) {
                    List<LLAtom> vars = new ArrayList<>();
                    vars.add((LLAtom) term);
                    return vars;
                }
            }

            //Recursive call to find embedded instances of variables
        } else if (term instanceof LLFormula) {
            List<LLAtom> right = findBoundOccurrences(((LLFormula) term).getLhs(), var);
            List<LLAtom> left = findBoundOccurrences(((LLFormula) term).getRhs(), var);

            return Stream.concat(right.stream(), left.stream()).collect(Collectors.toList());
        }
        List<LLAtom> emptyList = Collections.emptyList();
        return emptyList;
    }


    public LinkedHashMap<Integer, Premise> getOrderedDischarges() {
        return orderedDischarges;
    }

    public void setOrderedDischarges(LinkedHashMap<Integer,Premise> orderedDischarges) {
        this.orderedDischarges = orderedDischarges;
    }

/*
    public void updateBoundVariables()
    {
        if (this instanceof LLFormula)
        {
        if(getVariable()!= null) {
            //List<LLAtom> bvl = Stream.concat(findBoundOccurrences(lhs).stream(),findBoundOccurrences(rhs).stream()).collect(Collectors.toList());
            List<LLAtom> bvl = new ArrayList<>();
            bvl.addAll(findBoundOccurrences(((LLFormula) this).getLhs()));
            bvl.addAll(findBoundOccurrences(((LLFormula) this).getRhs()));
            ((LLFormula) this).getBoundVariables().put(getVariable(), bvl);
        }
        }
    }
*/

    //For multiple quantifiers
    public abstract void updateBoundVariables(LLAtom var);

    public List<Premise> getAssumptions2() {
        return assumptions2;
    }

    public void setAssumptions2(List<Premise> assumptions2) {
        this.assumptions2 = assumptions2;
    }

    public List<LLAtom> getVariable() {
        return variable;
    }

    public void setVariable(List<LLAtom> variable) {
        this.variable = variable;
    }


    public List<LLTerm> getcHistory() {
        return cHistory;
    }

    public void setcHistory(List<LLTerm> cHistory) {
        this.cHistory = cHistory;
    }

    public HashMap<LLAtom, LLAtom> getVariableAssignment() {
        return variableAssignment;
    }

    public void setVariableAssignment(HashMap<LLAtom, LLAtom> variableAssignment) {
        this.variableAssignment = variableAssignment;
    }

}



