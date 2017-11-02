package gluePaP.parser;

import gluePaP.linearLogic.*;

import java.util.ArrayList;
import java.util.List;

public class LinearLogicParserMo {


    public LinearLogicParserMo(String input) {
        this.input = input;
        //pos = 0;
    }

    private String input;
    private int pos;

    private void resetPosition() {
        this.pos = 0;
    }

    private void nextPos() {
        this.pos = pos + 1;
    }

    public LLTerm parse() {
        this.resetPosition();
        return parseTerm();
    }



    private LLTerm parseTerm(){

        //skip whitespaces
        while(input.charAt(pos) == ' '){
            pos++;
        }
        int c = (int) input.charAt(pos);
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
            if(input.charAt(pos) == 111) {
                pos++;
                return new LLImplication();
            }
            else //Exception??? {
            // TODO add appropriate
            return null;
        }

        // character is a left parenthesis, set scope
        else if (c == 40) {
            LLTerm left = parseTerm();
            LLOperator op = (LLOperator) parseTerm();
            LLTerm right = parseTerm();
            pos++;
            return new LLFormula(left,op,right);
        }

        // TODO something else, add exception here
        else {
            return null;
        }
    }

}
