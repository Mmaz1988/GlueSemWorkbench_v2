package Prover;

import gluePaP.linearLogic.Premise;
import gluePaP.linearLogic.Sequent;
import gluePaP.parser.LinearLogicParser;
import gluePaP.semantics.*;

import java.util.ArrayList;
import java.util.List;

import static gluePaP.semantics.BinaryTerm.SemOperator.AND;
import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class ProverMain {

    public static void main(String[] args) throws VariableBindingException {
        String test1 = "AX_t.(g_e -o X_t) -o X_t";
        String test2 = "AY_t.(h_e -o Y_t) -o Y_t";
        String test3 = "(g_e -o (h_e -o f_t))";
        String test4 = "(g_e -o f_t)";


        System.out.println("Parsing input...");

        List<String> testquant = new ArrayList<String>();
        List<String> testmod = new ArrayList<String>();
        List<String> testnest = new ArrayList<String>();

        // Test for quantifier premise
        testquant.add(test1);
        testquant.add(test4);

        // Instantiating semantic sides for "everybody loves someone"
        SemAtom varP = new SemAtom(VAR,"P",new SemType(E,T));
        SemAtom varQ = new SemAtom(VAR,"Q",new SemType(E,T));
        SemAtom varX = new SemAtom(VAR,"x1",E);
        SemAtom varX2 = new SemAtom(VAR,"x2",E);
        SemAtom varY = new SemAtom(VAR,"y",E);
        SemAtom varY2 = new SemAtom(VAR,"y2",E);

        SemFunction love = new SemFunction(varX2, new SemFunction(varY2,new SemPred("love",varX2,varY2)));
        SemFunction sleep = new SemFunction(varX2, new SemPred("sleep",varX2));
        // TODO the second argument should be a pred too
        BinaryTerm every0 = new BinaryTerm(new SemPred("person",varX),AND,new FuncApp(varP,varX));
        //SemFunction every1 = new SemFunction(varX,every0);
        SemFunction every = new SemFunction(varP,every0);

        SemPred some0 = new SemPred("some",new SemPred("person",varY),varQ);
        //SemFunction some1 = new SemFunction(varY,some0);
        SemFunction some = new SemFunction(varQ,some0);

        // Parsing
        LinearLogicParser parser2 = new LinearLogicParser(testquant);
        Sequent testseq = new Sequent(parser2.premises);

        // Adding meaning side to premises
        testseq.getLhs().get(0).setSemTerm(every);
        testseq.getLhs().get(1).setSemTerm(sleep);
        //testseq.getLhs().get(2).setSemTerm(love);

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
