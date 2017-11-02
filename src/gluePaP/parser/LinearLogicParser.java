package gluePaP.parser;

import gluePaP.linearLogic.*;

import java.util.ArrayList;
import java.util.List;

public class LinearLogicParser {

    public List<String> unparsedPremises;
    public List<LLTerm> premises;


    // Handles the position in the recursive function parse(string)
    
    private int pos;


    public LinearLogicParser(){}
    public LinearLogicParser(List<String> unparsedPremises) {

        this.premises = new ArrayList<LLTerm>();
        for (String unparsedPremise : unparsedPremises) {
            premises.add(parse(unparsedPremise));
        }


    }



    private void resetPosition() {
        this.pos = 0;
    }

    private void nextPos() {
        this.pos = pos + 1;
    }

    public LLTerm parse(String unparsedInput) {
        this.resetPosition();
        return parseTerm(unparsedInput);
    }

    private LLTerm parseTerm(String unparsedInput){

        //skip whitespaces
        while(unparsedInput.charAt(pos) == ' '){
            pos++;
        }
        int c = (int) unparsedInput.charAt(pos);
        pos++;
        // character is a lower case letter
        if(c >= 97 && c <= 122){
            return new LLConstant(""+(char) c);
        }

        // character is an upper case letter
        else if (c >= 65 && c <= 90){
            return new LLVariable(""+(char) c);
        }

        // character is a minus, might be first part of linear implication
        else if (c == 45) {
            // currently reading in a linear implication
            if(unparsedInput.charAt(pos) == 111) {
                pos++;
                return new LLImplication();
            }
            else //Exception??? {
                // TODO add appropriate
                return null;
        }

        // character is a left parenthesis, set scope
        else if (c == 40) {
            LLTerm left = parseTerm(unparsedInput);
            LLOperator op = (LLOperator) parseTerm(unparsedInput);
            LLTerm right = parseTerm(unparsedInput);
            pos++;
            return new LLFormula(left,op,right);
        }

        // TODO something else, add exception here
        else {
            return null;
        }
    }

}