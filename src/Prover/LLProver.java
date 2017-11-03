package Prover;

import gluePaP.linearLogic.*;

import java.util.*;

public class LLProver {


    public Premise deduce(Sequent seq) {
        // Initialize database of premises and initialize superset
        // containing all indexes of the sequents premises
        Stack<Premise> agenda = new Stack<>();
        List<Premise> database = new ArrayList<>();
        for (Premise p: seq.getLhs()) {
            agenda.push(p);
        }
        HashSet<Integer> goalIDs = seq.getMaxIDSet();

        while (!agenda.empty()) {
            Premise curr_premise = agenda.pop();
            // add premise to database
            database.add(curr_premise);
            for (int j = 0; j < database.size(); j++) {
                Premise db_premise = database.get(j);
                // Prevents comparing a premise with itself
                if(db_premise.getPremiseIDs().equals(curr_premise.getPremiseIDs()))
                    continue;
                /*
                Check if the database term is a (complex) formula, if so try to do an
                implication elimination step with the current term on the agenda (curr_premise).
                If successful add the newly created Premise to the database.
                */
                if (db_premise.getTerm() instanceof LLFormula) {
                    Premise new_premise = this.combinePremises(db_premise,curr_premise);
                    if (new_premise != null) {
                        if (new_premise.getPremiseIDs().equals(goalIDs)) {
                            return new_premise;
                        }
                        else if (!database.contains(new_premise)) {
                            database.add(new_premise); j=0;
                        }
                        continue;
                    }
                }
                /*
                Check if the current term on the agenda is a (complex) formula. If so do the same procedure
                as above, but reverse (apply db_premise to curr_premise).
                 */
                if (curr_premise.getTerm() instanceof LLFormula) {
                    Premise new_premise = this.combinePremises(curr_premise,db_premise);
                    if (new_premise != null) {
                        if (new_premise.getPremiseIDs().equals(goalIDs)) {
                            return new_premise;
                        }
                        else if (!database.) {
                            database.add(new_premise); j=0;
                        }
                    }

                }
            }
        }
        /*
        All premises were added to the database check for a successful analysis
        by looking for a database premise that contains the maximal set of all premise
        indexes and return it as goal.
        TODO probably not necessary, as the maximum IDSet can be checked after each elimination step
        */

/*        for (int i = database.size()-1; i >= 0; i--) {
            if (database.get(i).getPremiseIDs().equals(goalIDs))
                return database.get(i);
        }*/
        // No proper deduction was found
        // TODO return exception or some kind of message
        return null;
    }



    public Premise combinePremises(Premise func, Premise arg) {
        /*
        implementation of the linear implication elimination rule for indexed premises
        check if arg is equivalent to LHS of func and then return RHS of func
        then check if the sets of indexes are disjoint
        if both checks succeed a new Premise is created containing the unified set of indexes
        and the RHS LL term of func (see below)
        */
        if (((LLFormula) func.getTerm()).getLhs().checkEquivalence(arg.getTerm())) {
            HashSet<Integer> combined_IDs = new HashSet<>();
            if (Collections.disjoint(func.getPremiseIDs(),arg.getPremiseIDs()))
                combined_IDs.addAll(func.getPremiseIDs());
                combined_IDs.addAll(arg.getPremiseIDs());
                return new Premise(combined_IDs,((LLFormula) func.getTerm()).getRhs());
        }
        else {
            return null;
        }
    }

    public LLTerm combineTerms(LLFormula func, LLTerm arg) {
        /*
        Similar to combinePremises(), but for simple LL terms
        implementation of the linear implication elimination rule for LL terms
        check if arg is equivalent to LHS of func and then return RHS of func
        e.g. func = a -o b; arg = a --> returns b
        */
        if (func.getLhs().checkEquivalence(arg)) {
            return func.getRhs();
        }
        else {
            return null;
        }
    }

}
