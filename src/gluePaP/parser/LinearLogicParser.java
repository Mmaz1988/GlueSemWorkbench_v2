package gluePaP.parser;

import gluePaP.linearLogic.LLConstant;
import gluePaP.linearLogic.LLVariable;
import gluePaP.linearLogic.LinearLogicTerm;
import gluePaP.linearLogic.Sequent;

import java.util.ArrayList;
import java.util.List;

public class LinearLogicParser {

    public List<String> UnparsedPremises;
    public List<LinearLogicTerm> Premises;

    public LinearLogicParser() {

    }
        public List<LinearLogicTerm> parse(String llExpressionString)
        {
            List<LinearLogicTerm> premises = new ArrayList<LinearLogicTerm>();
            return premises;
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