package gluePaP.parser;

/*
    Todo: Already add possibility to parse complex formulas including parenthesis e.g. (f -o g) -o h
 */

import Prover.LLProver;
import Prover.ProverException;
import Prover.VariableBindingException;
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

    public static void main(String[] args) throws VariableBindingException {
        String test1 = "AX_t.(g_e -o X_t) -o X_t";
        String test3 = "AY_t.(h_e -o Y_t) -o Y_t";
        String test4 = "(g_e -o (h_e -o f_t))";
        //String test4 = "a";

        System.out.println("Parsing input...");

        List<String> testpremises = new ArrayList<String>();
        testpremises.add(test1);
        testpremises.add(test3);
        testpremises.add(test4);

        LinearLogicParser parser = new LinearLogicParser(testpremises);
        Sequent testseq = new Sequent(parser.premises);
        Sequent tempseq = testseq;

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
        LLProver prover = new LLProver();
        Premise result = null;
        try {
            result = prover.deduce(tempseq);
        } catch (ProverException e) {
            e.printStackTrace();
        }
        System.out.println("Found valid deduction: " + result.toString());
        System.out.println("Done!");

        String quantStr = "(AX.(X -o (X -o X) -o X)";
        String instStr = "(f -o (f -o g))";




        try{
            System.out.println("Testing Variable instantiation");
            LLFormula quant = (LLFormula) parser.parse(quantStr);
            LLFormula inst = (LLFormula) parser.parse(instStr);

            if (quant.getLhs().checkCompatibility(inst) != null) {

                System.out.println(quant.getLhs().checkCompatibility(inst));
                System.out.print(LLProver.checkDuplicateBinding(quant.getLhs().checkCompatibility(inst)));
            }
        } catch (ParserInputException e) {
            e.printStackTrace();
        }

    }

}