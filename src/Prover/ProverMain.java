package Prover;

import gluePaP.linearLogic.LLAtom;
import gluePaP.linearLogic.LLTerm;
import gluePaP.linearLogic.Premise;
import gluePaP.linearLogic.Sequent;
import gluePaP.parser.LinearLogicParser;
import gluePaP.semantics.SemAtom;
import gluePaP.semantics.SemFunction;
import gluePaP.semantics.SemPred;
import gluePaP.semantics.SemType;

import java.util.ArrayList;
import java.util.List;

import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class ProverMain {

    public static void main(String[] args) throws VariableBindingException {
        String test1 = "AX_t.(g_e -o X_t) -o X_t";
        String test2 = "(h_e -o f_t)";


        System.out.println("Parsing input...");

        List<String> testquant = new ArrayList<String>();
        List<String> testmod = new ArrayList<String>();
        List<String> testnest = new ArrayList<String>();

        // Test for quantifier premise
        testquant.add(test1);
        testquant.add(test2);

        // Instantiating semantic sides for "everybody sleeps"
        SemAtom varP = new SemAtom(VAR,"P",new SemType(E,T));
        SemAtom varQ = new SemAtom(VAR,"Q",new SemType(E,T));
        SemAtom varX = new SemAtom(VAR,"x1",E);
        SemAtom varX2 = new SemAtom(VAR,"x2",E);
        SemAtom varY = new SemAtom(VAR,"y",E);

        SemFunction sleep = new SemFunction(varY,new SemPred("sleep",varY));
        // TODO the second argument should be a pred too
        SemPred every0 = new SemPred("every",new SemPred("person",varX),varP);
        SemFunction every1 = new SemFunction(varX,every0);
        SemFunction every = new SemFunction(varP,every1);

        // Parsing
        LinearLogicParser parser2 = new LinearLogicParser(testquant);
        Sequent testseq = new Sequent(parser2.premises);

        // Adding meaning side to premises
        testseq.getLhs().get(0).setSemTerm(every);
        testseq.getLhs().get(1).setSemTerm(sleep);

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
        LLProver prover = new LLProver(testseq);
        List<Premise> result = null;
        try {
            result = prover.deduce();
            System.out.println("Found valid deduction(s): ");
            for (Premise sol : result) {
                System.out.println(sol.toString());
            }
        } catch (ProverException e) {
            e.printStackTrace();
        }



    }
}
