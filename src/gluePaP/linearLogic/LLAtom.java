package gluePaP.linearLogic;

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
        this.setLltype(lltype);
        this.setAssumption(false);
    }


    public LLType getLLtype(){
        return this.lltype;
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

    public boolean checkEquivalencies(LLTerm term){

        if (term instanceof LLAtom)
        {

        }

        return false;
    }


    // Getter and Setter name
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    // Getter and setter LLType
    public LLType getLltype() {
        return lltype;
    }

    public void setLltype(LLType lltype) {
        this.lltype = lltype;
    }

}
