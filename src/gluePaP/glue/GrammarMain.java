package gluePaP.glue;

// Base class for creating glue semantic representations with a meaning side and a glue side

import Prover.VariableBindingException;
import edu.stanford.nlp.trees.GrammaticalStructure;

import java.util.ArrayList;
import java.util.List;

public class GrammarMain {

    private final List<String> ps;
    private final DependencyParser dp = new DependencyParser();

    public GrammarMain(List<String> parseSet) throws VariableBindingException
    {
        this.ps = parseSet;
        List<GrammaticalStructure> parses =  dp.generateParses(this.ps);

        for (GrammaticalStructure parse : parses)
        {
           SentenceMeaning meaning = new SentenceMeaning(parse);
        }


    }

    public static void main(String[] args) throws VariableBindingException {

        List<String> testSentences = new ArrayList<String>();

//        testSentences.add("The ball flew through the broken window");
        testSentences.add("John snores.");
        testSentences.add("John loves Maria.");
        testSentences.add("A loud dog barks.");
/*        testSentences.add("John is happy.");
        testSentences.add("John ran quickly.");
        testSentences.add("John died on the bed");
        testSentences.add("John said that Karen was sick.");
        testSentences.add("Es regnet.");
*/
//        testSentences.add("John was able to open the door.");
//        testSentences.add("John saw the monkey with the telescope.");


        GrammarMain grammar = new GrammarMain(testSentences);

    }


}
