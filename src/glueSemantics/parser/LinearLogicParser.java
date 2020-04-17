/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.parser;

import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.lambda.SemType;

import java.util.ArrayList;
import java.util.List;

//Testtest
public class LinearLogicParser {

    public List<LLTerm> premises;


    // Handles the position in the recursive function parse(string)
    private int pos;


    public LinearLogicParser(){}


    public LinearLogicParser(List<String> unparsedPremises) {

        this.premises = new ArrayList<LLTerm>();
        for (String unparsedPremise : unparsedPremises) {

            try {

                LLTerm p = parse(unparsedPremise);
                //If p is null the line is commente
                if (p != null) {
                    premises.add(p);
                }
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
            throw new ParserInputException(String.format("ParserError: Linear logic '%s' term seems to be malformed",unparsedInput));
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
        char c = unparsedInput.charAt(pos);

        //comment out only at beginning of line.
        if (c == 45)
        {
            return null;
        }

        //char test = (char) c;

        // character is a lower case letter
        if(c >= 97 && c <= 122 || c >= 48 && c <= 57){

            StringBuilder sb = new StringBuilder();

            //or sequence of letters
            while (((c >= 97 && c <= 122) || (c >= 48 && c <= 57))) {
                    sb.append(c);
                    pos++;
                if (pos < unparsedInput.length()) {
                    c = unparsedInput.charAt(pos);
                }else
                {
                    break;
                }
            }


            String glueIdentifier = sb.toString();
            // check for a type identifier
            try {
                if(c == '_') {
                   // pos++;

                    if ( SemType.typeStrings.contains(String.valueOf(unparsedInput.charAt(pos)))) {
                        if (unparsedInput.charAt(pos) == 'e') {
                            pos++;
                            return new LLAtom(glueIdentifier,
                                    new SemType(SemType.AtomicType.E), LLAtom.LLType.CONST, polarity);
                        } else if (unparsedInput.charAt(pos) == 't') {
                            pos++;
                            return new LLAtom(glueIdentifier,
                                    new SemType(SemType.AtomicType.T), LLAtom.LLType.CONST, polarity);
                        } else if (unparsedInput.charAt(pos) == 's') {
                            pos++;
                            return new LLAtom(glueIdentifier,
                                    new SemType(SemType.AtomicType.S), LLAtom.LLType.CONST, polarity);

                        } else if (unparsedInput.charAt(pos) == 'i') {
                            pos++;
                            return new LLAtom(glueIdentifier,
                                    new SemType(SemType.AtomicType.I), LLAtom.LLType.CONST, polarity);
                        } else if (unparsedInput.charAt(pos) == 'v') {
                            pos++;
                            return new LLAtom(glueIdentifier,
                                    new SemType(SemType.AtomicType.V), LLAtom.LLType.CONST, polarity);
                        }
                    }
                    else
                        throw new ParserInputException(pos,"Type identifier expected (e,s,v,t or t)");
                }
                else
                {
                    return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.CONST,polarity);
                }
            } catch (StringIndexOutOfBoundsException e) {
                return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.CONST,polarity);
            }
            return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.CONST, polarity);
        }

        /*
        character is an upper case letter
        reserved characters: 'A' for universal quantifier
        */
        else if (c >= 66 && c <= 90){

            StringBuilder sb = new StringBuilder();

            //or sequence of letters
            while ((c >= 66 && c <= 90) || (c >= 48 && c <= 57))
            {
                sb.append(c);
                c = unparsedInput.charAt(pos);
                pos++;
            }

            String glueIdentifier = sb.toString();

            try {
                if(c == '_') {
                   // pos++;
                    if (unparsedInput.charAt(pos) == 'e') {
                        pos++;
                        return new LLAtom( glueIdentifier,
                                new SemType(SemType.AtomicType.E), LLAtom.LLType.VAR, polarity);
                    }
                    else if (unparsedInput.charAt(pos) == 't') {
                        pos++;
                        return new LLAtom( glueIdentifier,
                                new SemType(SemType.AtomicType.T), LLAtom.LLType.VAR, polarity);
                    }
                    else if (unparsedInput.charAt(pos) == 's') {
                        pos++;
                        return new LLAtom( glueIdentifier,
                                new SemType(SemType.AtomicType.S), LLAtom.LLType.VAR, polarity);
                    }
                    else if (unparsedInput.charAt(pos) == 'i') {
                    pos++;
                    return new LLAtom(glueIdentifier,
                            new SemType(SemType.AtomicType.I), LLAtom.LLType.CONST, polarity);
                } else if (unparsedInput.charAt(pos) == 'v') {
                    pos++;
                    return new LLAtom(glueIdentifier,
                            new SemType(SemType.AtomicType.V), LLAtom.LLType.CONST, polarity);
                }
                    else
                        throw new ParserInputException(pos,"Type identifier expected (e or t)");
                }
            } catch (StringIndexOutOfBoundsException e) {
                return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.VAR,polarity);
            }
            pos = pos - 1;
            return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.VAR,polarity);

        }

        // character is a left parenthesis, set scope
        else if (c == 40) {
            pos++;
            LLTerm left = parseTerm(unparsedInput, !polarity);
            //skip whitespaces
            while(unparsedInput.charAt(pos) == ' '){
                pos++;
            }
            if(unparsedInput.charAt(pos) == '-' && unparsedInput.charAt(pos+1) == 'o') {
                while(unparsedInput.charAt(pos) == ' ') {
                    pos++;
                }
                pos += 2;
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

            /*
            pos++;
            LLTerm left = parseTerm(unparsedInput, !polarity);
            left.setVariable((LLAtom) var);
            pos+=3;
            LLTerm right = parseTerm(unparsedInput, polarity);
            right.setVariable((LLAtom) var);
            pos++;
             */
            pos++;
            LLTerm scope = parseTerm(unparsedInput,polarity);
            scope.setVariable((LLAtom) var);

            if (scope instanceof LLFormula)
            {
                ((LLFormula) scope).updateBoundVariables();
            }

            return scope;

        }

        else {
            throw new ParserInputException("ParserError: Unknown character at position " + pos +": '" + unparsedInput.charAt(pos) + "'.");
        }
    }

}