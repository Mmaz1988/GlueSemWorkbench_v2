package gluePaP.parser;

import gluePaP.linearLogic.*;

import java.util.ArrayList;
import java.util.List;


public class LinearLogicParser {

    public List<String> unparsedPremises;
    public List<LLTerm> premises;

    // Handles the position in the recursive function parse(string)
    private int pos;
    // Used for assigning ID numbers to terms of a single premise
    private int curr_id;


    public LinearLogicParser(){}
    public LinearLogicParser(List<String> unparsedPremises) {

        this.premises = new ArrayList<LLTerm>();
        for (String unparsedPremise : unparsedPremises) {

            try {
                premises.add(parse(unparsedPremise));
            } catch (ParserInputException pe) {
                System.out.println(pe.getMessage());
            }
        }


    }


    /*
    Set string index and ID counter to zero. Used before a new premise
    is parsed.
    */
    private void resetParser() {
        this.pos = 0;
        this.curr_id = 0;
    }

    public String assignId() {
        this.curr_id++;
        return Integer.toString(curr_id);
    }


    public LLTerm parse(String unparsedInput) throws ParserInputException {
        this.resetParser();
        return parseTerm(unparsedInput, true);
    }

    // TODO properly add types (e or t)
    private LLTerm parseTerm(String unparsedInput, boolean polarity) throws ParserInputException {

        //skip whitespaces
        while(unparsedInput.charAt(pos) == ' '){
            pos++;
        }
        // get current character and increment the position counter
        int c = (int) unparsedInput.charAt(pos);
        pos++;

        // character is a lower case letter
        if(c >= 97 && c <= 122){
            return new LLAtom(assignId(),""+(char) c, LLTerm.Type.E, LLAtom.LLType.CONST, polarity);
        }

        /*
        character is an upper case letter
        reservered characters: 'A' for universal quantifier
        */
        else if (c >= 66 && c <= 90){
            return new LLAtom(assignId(),""+(char) c, LLTerm.Type.E, LLAtom.LLType.VAR,polarity);
        }

        // character is a minus, might be first part of linear implication
        else if (c == 45) {
            // currently reading in a linear implication
            if(unparsedInput.charAt(pos) == 111) {
                pos++;
                return new LLImplication();
            }
            else {
                throw new ParserInputException(pos+1);
            }
        }

        // character is a left parenthesis, set scope
        else if (c == 40) {

            LLTerm left = parseTerm(unparsedInput, !polarity);
            LLOperator op = (LLOperator) parseTerm(unparsedInput, polarity);
            LLTerm right = parseTerm(unparsedInput, polarity);
            pos++;
            return new LLFormula(assignId(),left,op,right, polarity);
        }

        else if (c == 41) {
            throw new ParserInputException("Unmatched closing paranthesis");
        }

        /*
        character is a universal quantifier (either 'A' or the unicode character,
        try parsing a quantified expression
        */
        else if (c == 14846080|| c == 65) {
            LLTerm var;
            var = parseTerm(unparsedInput,polarity);
            if (!(var instanceof LLAtom && ((LLAtom) var).getLLtype() == LLAtom.LLType.VAR))
                throw new ParserInputException(pos);
            pos++;
            LLTerm left = parseTerm(unparsedInput, !polarity);
            LLOperator op = (LLOperator) parseTerm(unparsedInput, polarity);
            LLTerm right = parseTerm(unparsedInput, polarity);
            pos++;
            return new LLFormula(assignId(),left,op,right, polarity,(LLAtom) var);

        }

        else {
            throw new ParserInputException("ParserError: Unknown character in formula");
        }
    }

}