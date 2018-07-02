package gluePaP.glue.lfg;

import java.util.List;

public class PrologComplexTerm extends PrologPredicate {
    private String functor;
    private List<PrologPredicate> arguments;


    public String getFunctor() {
        return functor;
    }

    public void setFunctor(String functor) {
        this.functor = functor;
    }

    public List<PrologPredicate> getArguments() {
        return arguments;
    }

    public void setArguments(List<PrologPredicate> arguments) {
        this.arguments = arguments;
    }
}
