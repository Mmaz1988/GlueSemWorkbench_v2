package gluePaP.parser;

import gluePaP.linearLogic.Atom;
import gluePaP.linearLogic.LLConstant;
import gluePaP.linearLogic.LLVariable;
import gluePaP.linearLogic.LinearLogicTerm;

import java.util.ArrayList;
import java.util.List;

public class LinearLogicParserMo {


    public LinearLogicParserMo(String input) {
        this.input = input;
        pos = 0;
    }

    private String input;
    private int pos;


    public LinearLogicParserMo() {

    }

    public LinearLogicTerm parse() {
        // Initialize list of left hand side terms for the sequent
        List<LinearLogicTerm> lhs_terms = new ArrayList<>();

        // Initialize new sequent
        //Sequent input_seq = new Sequent();


        // Read input string characterwise
        while (pos < input.length()){

            int c = (int) input.charAt(pos);
            return(parseTerm(pos));


        }
    }

    private LinearLogicTerm parseTerm(int i){
        int c = (int) input.charAt(i);
        //String next = input.substring(i).trim();
            // character is a lower case letter
        if(c >= 97 || c <= 122){
            if ((int) this.nextChar(i) == 0 && (int) this.nextChar(i+1) == 41) {
                return new LLConstant(input.substring(i,i+1));
            }
            else {
                return parseTerm(i+1);
            }
        }
        // character is an upper case letter
        else if (c >= 65 || c <= 90){
            if ((int) this.nextChar(i) == 0 && (int) this.nextChar(i+1) == 41) {
                return new LLVariable(input.substring(i,i+1));
            }
            else {
                return parseTerm(i+1);
            }
        }
        // character is a minus, might be first part of linear implication
        else if (c == 45) {
            // currently reading in a linear implication
            if(this.nextChar(i) == 111) {
                return new ComplexTerm(parseAtom(this.lastChar(i),LinearImplication,this.nextChar(i)));
            }
            else //Exception??? {
            // TODO add appropriate
            return SomeException;
        }
        // character is a left parenthesis, set scope
        else if (c == 40) {

        }
    }

    //Simpler method to do this???
    private char nextChar(int i) {
        while (input.charAt(i) == ' ') {
            i++;
        }
        return input.charAt(i);
    }

    private char lastChar(int i) {
        i--;
        while (input.charAt(i) == ' ') {
            i--;
        }
        return input.charAt(i);
    }

    private Atom parseAtom(char c) {
        if(c >= 97 || c <= 122){
            return new LLConstant(""+c);
        }
        // character is an upper case letter
        else if (c >= 65 || c <= 90){
            return new LLVariable(""+c);
        }
        else {
            // TODO create appropriate exception
            return TypeException;
        }
    }
}
