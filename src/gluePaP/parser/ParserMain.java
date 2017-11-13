package gluePaP.parser;

/*
    Todo: Already add possibility to parse complex formulas including parenthesis e.g. (f -o g) -o h
 */

import Prover.LLProver;
import Prover.ProverException;
import Prover.VariableBindingException;
import gluePaP.linearLogic.*;

import java.util.ArrayList;
import java.util.HashSet;
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
       // String test1 = "AX_t.(g_e -o X_t) -o X_t";
        // String test2 = "AY_t.(h_e -o Y_t) -o Y_t";
        // String test3 = "(g_e -o (h_e -o f_t))";


        String test4 = "(e -o f)";
        // String test5 = "(((a -o b) -o c) -o d)";
        String test6 = "((e -o f) -o (e -o f))";
        String test7 = "e";

        System.out.println("Parsing input...");

        List<String> testquant = new ArrayList<String>();
        List<String> testmod = new ArrayList<String>();

        // Test for quantifier premise
        //testquant.add(test1);
        //testquant.add(test3);
        //testquant.add(test3);
        // Test for modifier premises
        testmod.add(test4);
        // testmod.add(test5);
        testmod.add(test6);
        testmod.add(test7);


        LinearLogicParser parser = new LinearLogicParser(testmod);
        //LinearLogicParser parser = new LinearLogicParser(testquant);
        Sequent testseq = new Sequent(parser.premises);

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
        LLProver prover = new LLProver();
        List<Premise> result = null;
        try {
            result = prover.deduce(testseq);
            System.out.println("Found valid deduction(s): ");
            for (Premise sol : result) {
                System.out.println(sol.toString());
            }
        } catch (ProverException e) {
            e.printStackTrace();
        }

        System.out.println("Done!");

        String quantStr = "(AX.(X -o (X -o X) -o X)";
        String instStr = "(f -o (f -o g))";
        

    }

}