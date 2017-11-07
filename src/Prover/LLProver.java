package Prover;

import gluePaP.linearLogic.*;

import java.util.*;

public class LLProver {

    /*
    Does a deduction of a given sequent by evaluating the list of premises on its LHS
    and trying to find a valid proof for its RHS.
    TODO So far only applies implication elimination
     */
    public Premise deduce(Sequent seq) throws ProverException {
        /*
        Initialize an agenda stack initially containing all premises from the sequent.
        Premises are popped from the stack into the database and additionally created
        premises get pushed onto the stack.
        Then initialize a database of all premises which is used to look for possible
        implication elimination steps.
        */
        Stack<Premise> agenda = new Stack<>();
        List<Premise> database = new ArrayList<>();
        for (Premise p: seq.getLhs()) {
            agenda.push(p);
        }

        /*
        Initialize the set containing the IDs of all premises of the sequent.
        This set is used to determine possible goal terms.
        */
        HashSet<Integer> goalIDs = seq.getMaxIDSet();

        /*
        The algorithm loops over the agenda until it is empty or until a premise is created
        that contains all indexes of the sequent's premises and is therefore the goal.
        */
        while (!agenda.empty()) {
            Premise curr_premise = agenda.pop();
            // add premise to database
            database.add(curr_premise);
            for (int i = 0; i < database.size(); i++) {
                Premise db_premise = database.get(i);

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
                        else {
                            agenda.push(new_premise);
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
                        else {
                            agenda.push(new_premise);
                        }
                    }

                }
            }
        }

        throw new ProverException("No valid proof found for premises");
    }


    /*
    implementation of the linear implication elimination rule for indexed premises
    check if arg is equivalent to LHS of func and then return RHS of func
    then check if the sets of indexes are disjoint
    if both checks succeed a new Premise is created containing the unified set of indexes
    and the RHS LL term of func (see below)
    */



    public Premise combinePremises(Premise func, Premise arg) {

        if (((LLFormula) func.getTerm()).getLhs().checkEquivalence(arg.getTerm())) {
            HashSet<Integer> combined_IDs = new HashSet<>();
            if (Collections.disjoint(func.getPremiseIDs(),arg.getPremiseIDs()))
                combined_IDs.addAll(func.getPremiseIDs());
                combined_IDs.addAll(arg.getPremiseIDs());
                return new Premise(combined_IDs,((LLFormula) func.getTerm()).getRhs());
        }
        else if (func.getTerm() instanceof LLUniversalQuant) {
            LLUniversalQuant quant = (LLUniversalQuant) func.getTerm();


            return null;
        }
        else {
            return null;
        }

    }


    /*
    Similar to combinePremises(), but for simple LL terms
    implementation of the linear implication elimination rule for LL terms
    check if arg is equivalent to LHS of func and then return RHS of func
    e.g. func = a -o b; arg = a --> returns b
    */
    public LLTerm combineTerms(LLFormula func, LLTerm arg) {

        if (func.getLhs().checkEquivalence(arg)) {
            return func.getRhs();
        }
        else {
            return null;
        }
    }

}
