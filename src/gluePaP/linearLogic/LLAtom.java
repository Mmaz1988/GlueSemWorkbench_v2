package gluePaP.linearLogic;

import Prover.Equality;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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





    // toString method
    @Override
    public String toString() {
        return name + "_" + this.getTermId();
    }


    // checks absolute equivalence (type and name)
    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLAtom) {
            if (this.name.equals(((LLAtom) term).name)
                    && this.getType().equals(((LLAtom) term).getType()))
                 //   && this.getLLtype().equals(((LLAtom) term).getLLtype()))
            {
                return true;
            }
        }
        return false;

    }

    @Override
    public List<Equality> checkCompatibility(LLTerm term) {
        if (term instanceof LLAtom) {
            if (this.getLLtype().equals(LLType.VAR)) {
                {
                    if (((LLAtom) term).getLLtype().equals(LLType.VAR)) {
                        List<Equality> emptyList = Collections.emptyList();
                        return emptyList;
                    } else if ( ((LLAtom) term).getLLtype().equals(LLType.CONST) &&
                            this.getType().equals((term.getType()))) {
                        {
                            List<Equality> newEq = new ArrayList<>();
                            newEq.add(new Equality(this, (LLAtom) term));
                            return newEq;
                        }
                    }
                }
            } else if (this.getLLtype().equals(LLType.CONST)) {
                if (((LLAtom) term).getLLtype().equals(LLType.CONST)) {
                    if (this.getName().equals(((LLAtom) term).getName()) &&
                            this.getType().equals(term.getType())) {
                        List<Equality> emptyList = Collections.emptyList();
                        return emptyList;
                    } else
                        {
                            return null;
                        }
                } else if (((LLAtom) term).getLLtype().equals(LLType.VAR) &&
                        this.getType().equals(term.getType()))
                    {
                        List<Equality> newEq = new ArrayList<>();
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
