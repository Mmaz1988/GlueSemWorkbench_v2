import Prover.LLProver;
import gluePaP.linearLogic.LLFormula;
import gluePaP.linearLogic.LLTerm;
import gluePaP.parser.LinearLogicParser;
import gluePaP.parser.ParserInputException;

public class Main {

    public static void main(String[] args) {
        LinearLogicParser pa = new LinearLogicParser();
        //LLProver pr = new LLProver();
        String form1 = "((a -o b) -o c)";
        String form2 = "(((a -o b) -o c) -o d)";
        String form3 = "(a -o b)";
        LLTerm term1, term2, term3;

        try {
            term1 = pa.parse(form1);
            term2 = pa.parse(form2);
            term3 = pa.parse(form3);
            //LLTerm converted = pr.convert(term2);
            System.out.println("DONE!");
        } catch (ParserInputException e) {
            e.printStackTrace();
        }


    }

}
