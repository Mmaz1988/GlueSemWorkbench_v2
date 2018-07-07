package glueSemantics.glue;

import java.util.regex.Matcher;

public class LexicalParserException extends Exception {
    public LexicalParserException(String message) {
        super(message);
    }

    public LexicalParserException(Matcher m) {
        super ("Couldn't find pattern "+ m.pattern().toString());
    }

}
