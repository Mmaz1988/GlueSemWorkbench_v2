package gluePaP.parser;

/*
    Todo: Already add possibility to parse complex formulas including parenthesis e.g. (f -o g) -o h
 */

import gluePaP.linearLogic.Atom;
import gluePaP.linearLogic.LLFormula;
import gluePaP.linearLogic.LLTerm;
import gluePaP.linearLogic.Sequent;

import java.util.ArrayList;
import java.util.List;

public class ParserMain {
    public String Input;
    public List<LLTerm> Premises;
    public Atom Goal;

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
        String test1 = "((a -o b) -o (f -o d))";
        String test2 = "(a -o b)";
        String test3 = "f";


        System.out.println("Parsing input...");

        List<String> testpremises = new ArrayList<String>();
        testpremises.add(test1);
        testpremises.add(test2);
        testpremises.add(test3);

        LinearLogicParser parser = new LinearLogicParser(testpremises);
        Sequent testseq = new Sequent(parser.premises);

        System.out.println(testseq.toString());
        System.out.println("Done!");

        System.out.println("Checking equivalence function");
        LLFormula a = (LLFormula) testseq.getLhs().get(0).getLlterm();
        LLFormula b = (LLFormula) testseq.getLhs().get(1).getLlterm();
        if(a.getLhs().checkEquivalence(b))
            System.out.println("It worked!");

    }



    /*
    public void parse(String str) {
        // Initialize list of left hand side terms for the sequent
        List<LLTerm> lhs_terms;
        // Initialize new sequent
        //Sequent input_seq = new Sequent();
        // Read input string characterwise
        for (int i = 0; i < str.length(); i++) {
            int c = (int) str.charAt(i);

            // character is a whitespace
            if (c == 32)
                continue;
                // character is a lower case letter
            else if (c >= 97 || c <= 122) {
                lhs_terms.add(new LLConstant(Character.toString((char) c)));
            }
            // character is an upper case letter
            else if (c >= 65 || c <= 90) {
                lhs_terms.add(new LLVariable(Character.toString((char) c)));
            }
            // character is a left parenthesis, set scope
            else if (c == 40) {

            }
            // character is a comma, delimits premises
            else if (c == 44) {

            }
            // character is an equal sign, might be first part of consequent separator (=>)
            else if (c == 61) {

            }
            // character is a minus, might be first part of linear implication
            else if (c == 45) {

            } else {
                // return exception?
            }


        }
    }
    */
}