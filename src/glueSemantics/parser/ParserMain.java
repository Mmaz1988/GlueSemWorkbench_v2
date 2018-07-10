/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.parser;



import prover.VariableBindingException;
import glueSemantics.linearLogic.*;

import java.util.ArrayList;
import java.util.List;

public class ParserMain {
    public String Input;
    public List<LLTerm> Premises;
    public LLAtom Goal;

    public ParserMain(String input)
    {
        this.Input = input;


        String[] in = Input.split("=>");
        String[] stringPremises = in[0].split(",");


        if (stringPremises.length != 0)
        {
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
        String test2 = "AY_t.(h_e -o Y_t) -o Y_t";
        String test3 = "(g_e -o (h_e -o f_t))";
        String test9 = "((g_e -o (h_e -o f_t)) -o (g_e -o (h_e -o f_t)))";
        String test10 = "(h_e -o h_e)";

        String test4 = "(e -o f)";
        String test6 = "((e -o f) -o (e -o f))";
        String test7 = "e";

        String test5 = "(((a -o b) -o c) -o d)";
        String test8 = "((((a -o b) -o c) -o d) -o e)";

        System.out.println("Parsing input...");

        List<String> testquant = new ArrayList<>();
        List<String> testmod = new ArrayList<>();
        List<String> testnest = new ArrayList<>();

        // Test for quantifier premise
        testquant.add(test1);
        testquant.add(test2);
        testquant.add(test3);
        testquant.add(test9);
        testquant.add(test10);
        // Test for modifier premises
        testmod.add(test4);
        //testmod.add(test5);
        testmod.add(test6);
        testmod.add(test7);
        testnest.add(test5);
        testnest.add(test8);

        LinearLogicParser parser = new LinearLogicParser(testquant);
        System.out.println("Parsed terms: " + parser.premises.toString());

    }

}