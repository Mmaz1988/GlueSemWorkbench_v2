package glueSemantics.parser;

import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.SemanticExpression;

import java.util.Scanner;

public class SemanticParser {


    private String in;
    private int pos = 0;

    public SemanticParser()
    {

    }
    public static void main(String[] args) {

        Scanner s = new Scanner(System.in);
        String input = s.nextLine();

        SemanticParser sp = new SemanticParser();

        //sp.parse("[/x]");

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

}
