package gluePaP.parser;

public class ParserInputException extends Exception {

    /*
    TODO add the ID of the premise to the constructor so the error can be
    located precisely.
    */
    public ParserInputException(int position) {
        super("ParserError: Unexpected character at position "+position);
    }

    public ParserInputException(String message) {
        super(message);
    }
}
