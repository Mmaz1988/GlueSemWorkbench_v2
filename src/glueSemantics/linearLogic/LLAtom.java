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

public class LLAtom extends LLTerm {



    public enum LLType {
        VAR,
        CONST
    }


    private String name;
    public SemType lltype;




    public LLAtom(String name, Type type, LLType lltype, boolean pol) {
        this.name = name;
        this.setType(type);
        this.setPolarity(pol);
        this.setLLtype(lltype);
    }

    //binder variables are not polarized -- they can occur in positive and negative formulas
    public LLAtom(String name, Type type, LLType lltype) {
        this.name = name;
        this.setType(type);
        this.setLLtype(lltype);
    }


    public LLAtom(LLAtom term) {
        this.assumptions = new HashSet<>(term.assumptions);
        this.discharges = new HashSet<>(term.discharges);
        this.name = term.getName();
        this.setType(term.getType());
        this.setPolarity(term.isPolarity());
        this.setLLtype(term.getLLtype());


    }

    @Override
    public String toString() {
        if (this.assumptions.isEmpty())
            return this.toPlainString();
        else {
            if (this.assumptions.size() == 1 && this.assumptions.contains(this) && this.discharges.isEmpty())
                return "{" + name + "}";
            else
                return name + this.printAssumptions();
        }
    }

    public String toPlainString() {
        return name;
    }


    // checks absolute equivalence (type and name)
    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLAtom) {
            if (this.name.equals(((LLAtom) term).name)
                    && this.getType().equals(((LLAtom) term).getType()))
            {
                return true;
            }
        }
        return false;

    }

    @Override
    public LinkedHashSet<Equality> checkCompatibility(LLTerm term) {
        if (term instanceof LLAtom) {
            if (this.getLLtype().equals(LLType.VAR)) {
                {
                    if (((LLAtom) term).getLLtype().equals(LLType.VAR)) {
                        // Not possible to unify two variables?
                        return null;
                    } else if ( ((LLAtom) term).getLLtype().equals(LLType.CONST) &&
                            this.getType().equals((term.getType()))) {
                        {
                            LinkedHashSet<Equality> newEq = new LinkedHashSet<>();
                            newEq.add(new Equality(this, (LLAtom) term));
                            return newEq;
                        }
                    }
                }
            } else if (this.getLLtype().equals(LLType.CONST)) {
                if (((LLAtom) term).getLLtype().equals(LLType.CONST)) {
                    if (this.getName().equals(((LLAtom) term).getName()) &&
                            this.getType().equals(term.getType())) {
                        LinkedHashSet<Equality> emptyList = new LinkedHashSet<>();
                        return emptyList;
                    } else
                        {
                            return null;
                        }
                } else if (((LLAtom) term).getLLtype().equals(LLType.VAR) &&
                        this.getType().equals(term.getType()))
                    {
                        LinkedHashSet<Equality> newEq = new LinkedHashSet<>();
                        newEq.add(new Equality((LLAtom) term,  this));
                        return newEq;
                    }


            }
        }
            return null;
    }



    // Getter and Setter name
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    // Getter and setter LLType
    public SemType getLLtype() {
        return lltype;
    }

    public void setLLtype(SemType lltype) {
        this.lltype = lltype;
    }


    @Override
    public LLTerm clone() {
        return new LLAtom(this   );
    }

    @Override
    public boolean isModifier() {
        return false;
    }
}
