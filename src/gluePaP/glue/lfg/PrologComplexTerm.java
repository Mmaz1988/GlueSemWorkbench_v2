package gluePaP.glue.lfg;

import java.util.List;

public class PrologComplexTerm extends PrologPredicate {
    private String functor;
    private List<String> arguments;


    public String getFunctor() {
        return functor;
    }

    public void setFunctor(String functor) {
        this.functor = functor;
    }

    public List<String> getArguments() {
        return arguments;
    }

    public void setArguments(List<String> arguments) {
        this.arguments = arguments;
    }
}
