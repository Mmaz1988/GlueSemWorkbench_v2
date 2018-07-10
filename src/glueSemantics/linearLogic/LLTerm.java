/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
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

    private String termId;
    private boolean polarity;
    private Type type;
    public Set<LLTerm> assumptions = new HashSet<>();
    public Set<LLTerm> discharges = new HashSet<>();

    //Default constructor
    public LLTerm(){ }

/*
    public LLTerm(LLTerm term) {
        this.assumptions = term.assumptions;
        this.discharges = term.discharges;
        this.termId = term.termId;
        this.type = term.type;
        this.polarity = term.polarity;

    }
*/

    public String getTermId() {
        return termId;
    }

    public boolean isPolarity() { return polarity; }

    public void setPolarity(boolean pol) { this.polarity = pol; }

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
