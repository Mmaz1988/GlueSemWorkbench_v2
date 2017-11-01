package gluePaP.linearLogic;

public class LLImplication implements LLOperator {
    private String Id;
    private final String symbol = "\u22B8";


    public String getId() {
        return Id;
    }

    public String toString() {
        return symbol;
    }


}
