
/*
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemFunction;
import glueSemantics.semantics.lambda.SemType;
import glueSemantics.semantics.lambda.SemanticExpression;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

i*mport glueSemantics.linearLogic.LLAtom;

/
        package glueSemantics.parser;

@Deprecated
public class SemanticParser {


    private String in;
    private int pos = 0;
    private int openBrackets = 0;
    private HashMap<Integer, List<SemAtom>> boundVariables;

    public SemanticParser() {
    }

    public static void main(String[] args) {

        Scanner s = new Scanner(System.in);
        String input = s.nextLine();

        SemanticParser sp = new SemanticParser();

        //sp.parse("[/x]");

    }

    public SemanticExpression parseSemantics(String unparsedInput) throws ParserInputException {

        //skip whitespaces
        while (unparsedInput.charAt(pos) == ' ') {
            pos++;
        }
        // get current character and increment the position counter


        //Open and close brackets
        if (unparsedInput.charAt(pos) == '[') {
            openBrackets += 1;
            pos++;
        }

        if (unparsedInput.charAt(pos) == ']') {
            openBrackets = openBrackets - 1;
            pos++;
        }

        char c = unparsedInput.charAt(pos);

        pos++;


        if (c <= 112 && c <= 122 || c <= 80 && c <= 90)
        {
            String glueIdentifier = String.valueOf(c);
            c = unparsedInput.charAt(pos);
            pos ++;
            try {
                if (c == '_') {
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

                else{}


            } catch (StringIndexOutOfBoundsException e) {
                return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.VAR,polarity);
            }
            pos = pos - 1;
            return new LLAtom(glueIdentifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.VAR,polarity);

                    }




        //Lambda epression
        if (c == 47) {

            SemAtom semVar = null;

            try {
                semVar = (SemAtom) parseSemantics(unparsedInput);


                if (boundVariables.keySet().contains(openBrackets)) {
                    boundVariables.get(openBrackets).add(semVar);
                } else {
                    boundVariables.put(openBrackets, new ArrayList<>());
                    boundVariables.get(openBrackets).add(semVar);
                }


                while (unparsedInput.charAt(pos) == ' ') {
                    pos++;
                    if (unparsedInput.charAt(pos) == '.') {
                        while (unparsedInput.charAt(pos) == ' ') {
                            pos++;
                        }
                        pos += 1;
                        SemanticExpression scope = parseSemantics(unparsedInput);
                        pos++;
                        return new SemFunction((SemAtom) semVar, scope);
                    }
                    throw new ParserInputException(pos, "dot expected");

                }

            } catch (ParserInputException e) {
                System.out.println("Invalid lambda expression");
            }
        }

        if (c >= 97 && c <= 122 || c >= 48 && c <= 57) {

            StringBuilder sb = new StringBuilder();

            //or sequence of letters
            while ((c >= 97 && c <= 122) || (c >= 48 && c <= 57)) {
                sb.append(c);
                c = unparsedInput.charAt(pos);
                pos++;
            }

            String glueIdentifier = sb.toString();

            if (c == 40) {
            }

        }
        return null;


    }
}

    /*
    public SemanticRepresentation parse(String unparsedInput)
    {

        while(unparsedInput.charAt(pos) == ' '){
            pos++;
        }

        // get current character and increment the position counter
        char c = unparsedInput.charAt(pos);
        //char test = (char) c;
        pos++;




    }
    */


