package gluePaP.linearLogic;


import java.util.ArrayList;
import java.util.List;

public class Sequent {
    private List<Premise> lhs;
    private Atom rhs;

    public List<Premise> getLhs() {
        return lhs;
    }

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

    public Atom deduce() {
        // Initialize database of premises
        List<Premise> database = new ArrayList<>();

        for (int i = 0; i < lhs.size(); i++) {
            Premise new_premise = lhs.get(i);
            // add premise to database
            database.add(new_premise);
            for (int j = 0; j < database.size(); j++) {
                Premise db_premise = database.get(j);
                if (db_premise.getLlterm() instanceof LLFormula) {
                    if(((LLFormula) db_premise.getLlterm()).getLhs().checkEquivalence(new_premise.getLlterm())) {
                        // insert the new term into the LHS of the database term and add the simplified term

                    }
                }
                else if (new_premise.getLlterm() instanceof LLFormula) {
                    if(((LLFormula) new_premise.getLlterm()).getLhs().checkEquivalence(db_premise.getLlterm())) {
                        // insert the database term into the LHS of the new term and add the simplified term
                    }
                }
            }
        }

        return null;
    }

    private boolean checkIndices(Premise a, Premise b){ return true;}
}