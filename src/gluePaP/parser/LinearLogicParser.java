package gluePaP.parser;

import gluePaP.linearLogic.*;

import java.util.List;

public class LinearLogicParser {

    public List<String> unparsedPremises;
    public List<LLTerm> premises;

    public LinearLogicParser() {

        for (String unparsedPremise : unparsedPremises) {
            premises.add(parse(unparsedPremise,0, 0,0));
        }

    }

    public LLTerm parse(String llExpressionString, Integer i, Integer newIndex, Integer parensCounter) {



        while ( i < llExpressionString.length()) {
            int c = (int) llExpressionString.charAt(i);

            // character is a whitespace
            if (c == 32) {
                i++;
            }
                // character is a lower case letter
            else if (c >= 97 || c <= 122) {
                LLTerm term = new LLConstant(newIndex.toString());
                newIndex++;
            }
            // character is an upper case letter
            else if (c >= 65 || c <= 90) {
                LLTerm term = new LLVariable(newIndex.toString());
                newIndex++;

            }
            // character is a left parenthesis, set scope
            else if (c == 40) {
                LLTerm term = new LLFormula(newIndex.toString());
                i++;
                newIndex++;
                parensCounter++;


            } else if (c == 41)
            {
                parensCounter--;
                i++;
            }
            else
            {
                // return exception?
            }


        }


    }
}

            /*
    public void parse(String str) {
        // Initialize list of left hand side terms for the sequent
        List<LinearLogicTerm> lhs_terms;
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