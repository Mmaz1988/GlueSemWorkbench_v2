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

            try {
                premises.add(parse(unparsedPremise));
            } catch (ParserInputException pe) {
                System.out.println(pe.getMessage());
            }
        }
    }


    /**
    Set string index and ID counter to zero. Used before a new premise
    is parsed.
    */
    private void resetParser() {
        this.pos = 0;
    }


    public LLTerm parse(String unparsedInput) throws ParserInputException {
        this.resetParser();
        try {
            return parseTerm(unparsedInput, true);
        }
        catch (StringIndexOutOfBoundsException se) {
            throw new ParserInputException("ParserError: Unexpected end of input term");
        }
    }

    /**
     * Parse string as a glue expression
     * @param unparsedInput Input string to be parsed
     * @param polarity Polarity of the term. Is positive by default but changes during recursive calls
     * @return An LLTerm
     * @throws ParserInputException
     * @throws StringIndexOutOfBoundsException
     */
    private LLTerm parseTerm(String unparsedInput, boolean polarity) throws ParserInputException, StringIndexOutOfBoundsException {

        //skip whitespaces
        while(unparsedInput.charAt(pos) == ' '){
            pos++;
        }
        // get current character and increment the position counter
        int c = (int) unparsedInput.charAt(pos);
        //char test = (char) c;
        pos++;

        // character is a lower case letter
        if(c >= 97 && c <= 122){
            // check for a type identifier
            try {
                if(unparsedInput.charAt(pos) == '_') {
                    pos++;
                    if (unparsedInput.charAt(pos ) == 'e') {
                        pos++;
                        return new LLAtom( "" + (char) c,
                                LLTerm.Type.E, LLAtom.LLType.CONST, polarity);
                    }
                    else if (unparsedInput.charAt(pos) == 't') {
                        pos++;
                        return new LLAtom("" + (char) c,
                                LLTerm.Type.T, LLAtom.LLType.CONST, polarity);
                    }
                    else
                        throw new ParserInputException(pos,"Type identifier expected (e or t)");
                }
            } catch (StringIndexOutOfBoundsException e) {
                return new LLAtom(""+(char) c, LLTerm.Type.E, LLAtom.LLType.CONST,polarity);
            }
            return new LLAtom(""+(char) c, LLTerm.Type.E, LLAtom.LLType.CONST, polarity);
        }

        /*
        character is an upper case letter
        reserved characters: 'A' for universal quantifier
        */
        else if (c >= 66 && c <= 90){
            try {
                if(unparsedInput.charAt(pos) == '_') {
                    pos++;
                    if (unparsedInput.charAt(pos ) == 'e') {
                        pos++;
                        return new LLAtom( "" + (char) c,
                                LLTerm.Type.E, LLAtom.LLType.VAR, polarity);
                    }
                    else if (unparsedInput.charAt(pos) == 't') {
                        pos++;
                        return new LLAtom( "" + (char) c,
                                LLTerm.Type.T, LLAtom.LLType.VAR, polarity);
                    }
                    else
                        throw new ParserInputException(pos,"Type identifier expected (e or t)");
                }
            } catch (StringIndexOutOfBoundsException e) {
                return new LLAtom(""+(char) c, LLTerm.Type.T, LLAtom.LLType.VAR,polarity);
            }
            return new LLAtom(""+(char) c, LLTerm.Type.T, LLAtom.LLType.VAR,polarity);

        }

        // character is a left parenthesis, set scope
        else if (c == 40) {

            LLTerm left = parseTerm(unparsedInput, !polarity);
            //skip whitespaces
            while(unparsedInput.charAt(pos) == ' '){
                pos++;
            }
            if(unparsedInput.charAt(pos) == '-' && unparsedInput.charAt(pos+1) == 'o') {
                LLTerm right = parseTerm(unparsedInput, polarity);
                pos++;
                return new LLFormula(left,right,polarity);
            }
            throw new ParserInputException(pos,"implication expected");

        }

        else if (c == 41) {
            throw new ParserInputException("Unmatched closing parenthesis");
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

            LLTerm right = parseTerm(unparsedInput, polarity);
            pos++;
            return new LLFormula(left,right, polarity,(LLAtom) var);

        }

        else {
            throw new ParserInputException("ParserError: Unknown character in formula");
        }
    }

}