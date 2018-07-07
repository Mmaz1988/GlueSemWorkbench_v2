package glueSemantics.parser;

public class ParserInputException extends Exception {

    public ParserInputException(int position) {
        super("ParserError: Unexpected character at position "+position);
    }

    public ParserInputException(String message) {
        super(message);
    }

    public ParserInputException(int position, String message) {
        super("ParserError at position " + position + ": " + message);
    }
}

