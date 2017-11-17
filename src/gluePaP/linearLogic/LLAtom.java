package gluePaP.linearLogic;

import Prover.Equality;

import java.util.*;

public class LLAtom extends LLTerm {



    public enum LLType {
        VAR,
        CONST
    }


    private String name;
    public LLType lltype;




    public LLAtom(String id, String name, Type type, LLType lltype, boolean pol) {
        this.name = name;
        this.setTermId(id);
        this.setType(type);
        this.setPolarity(pol);
        this.setLLtype(lltype);
    }


    public LLAtom(LLAtom term) {
        this.assumptions = new HashSet<>(term.assumptions);
        this.discharges = new HashSet<>(term.discharges);
        this.name = term.getName();
        this.setTermId(term.getTermId());
        this.setType(term.getType());
        this.setPolarity(term.isPolarity());
        this.setLLtype(term.getLLtype());


    }

    @Override
    public String toString() {
        if (!this.assumptions.isEmpty())
            return "{" + name + "_" + this.getTermId() + "}";
       else
           return name + "_" + this.getTermId();
    }

    public String toPlainString() {
        return name + "_" + this.getTermId();
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
                        LinkedHashSet<Equality> emptyList = new LinkedHashSet();
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
    public LLType getLLtype() {
        return lltype;
    }

    public void setLLtype(LLType lltype) {
        this.lltype = lltype;
    }

}
