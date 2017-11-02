package gluePaP.linearLogic;


import java.util.ArrayList;
import java.util.List;

public class Sequent {
    private List<Premise> lhs;
    private Atom rhs;

    public Sequent(List<LLTerm> parsedTerms) {
        lhs = new ArrayList<>();
        for (int i = 0; i < parsedTerms.size(); i++) {
            lhs.add(new Premise(Integer.toString(i+1),parsedTerms.get(i)));
        }
    }

    @Override
    public String toString() {
        return lhs + " => " +  "null";//rhs.toString();
    }

    /*    public String toString() {
        String toString = lhs.get(0).toString();
        for (int i = 1; i < lhs.size(); i++) {
            toString = toString + ", " + lhs.get(i).toString();
        }
        return toString + "=>" + rhs.toString();
    }*/
}