package gluePaP.parser;

/*
    Todo: Already add possibility to parse complex formulas including parenthesis e.g. (f -o g) -o h
 */

import Prover.LLProver;
import Prover.ProverException;
import gluePaP.linearLogic.*;

import java.util.ArrayList;
import java.util.List;

public class ParserMain {
    public String Input;
    public List<LLTerm> Premises;
    public LLAtom Goal;

    public ParserMain(String input)
    {
        this.Input = input;

        // TODO Handle unwanted exceptions (Number of array arguments)
        String[] In = Input.split("=>");
        String[] stringPremises = In[0].split(",");


        if (stringPremises.length != 0)
        {
            // TODO Create semantic parser
            LinearLogicParser llParser = new LinearLogicParser();
            for (String stringPremise :  stringPremises) {
                String[] glueRepresentation = stringPremise.split(":");

                llParser.unparsedPremises.add(glueRepresentation[1]);
            }
        }
        else
            {
            throw new IllegalArgumentException();
        }

    }

    public static void main(String[] args) {
        String test1 = "(a -o b)";
        String test2 = "(a -o (b -o (c -o d))";
        String test3 = "c";

        System.out.println("Parsing input...");

        List<String> testpremises = new ArrayList<String>();
        testpremises.add(test1);
        testpremises.add(test2);
        testpremises.add(test3);

       // LLAtom goal = new LLAtom("1","d", LLAtom.Type.T, false);

        LinearLogicParser parser = new LinearLogicParser(testpremises);
        Sequent testseq = new Sequent(parser.premises);

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
        LLProver prover = new LLProver();
        Premise result = null;
        try {
            result = prover.deduce(testseq);
        } catch (ProverException e) {
            e.printStackTrace();
        }
        System.out.println(result.toString());
        System.out.println("Done!");

        String quantStr = "AX.((g -o X) -o X)";
        String instStr = "(g -o f)";


        try {
            System.out.println("Testing Variable instantiation");
            LLUniversalQuant quant = (LLUniversalQuant) parser.parse(quantStr);
            LLFormula inst = (LLFormula) parser.parse(instStr);
            LLTerm inst_result = LLFormula.instantiateVar(quant.getVariable(),quant.getTerm(),inst);
            System.out.println(inst_result.toString());
        } catch (ParserInputException e) {
            e.printStackTrace();
        }
    }
}