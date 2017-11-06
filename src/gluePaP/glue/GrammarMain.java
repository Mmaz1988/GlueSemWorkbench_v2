package gluePaP.glue;

// Base class for creating glue semantic representations with a meaning side and a glue side

import edu.stanford.nlp.trees.GrammaticalStructure;

import java.util.ArrayList;
import java.util.List;

public class GrammarMain {

    private final List<String> ps;
    private final DependencyParser dp = new DependencyParser();

    public GrammarMain(List<String> parseSet)
    {
        this.ps = parseSet;
        List<GrammaticalStructure> parses =  dp.generateParses(this.ps);

        for (GrammaticalStructure parse : parses)
        {
           SentenceMeaning meaning = new SentenceMeaning(parse);
        }


    }

    public static void main(String[] args) {

        List<String> testSentences = new ArrayList<String>();

        testSentences.add("John snores.");
        testSentences.add("John loves Maria.");
        testSentences.add("John was able to open the door.");
        testSentences.add("John said that Mary was sick");
        testSentences.add("John was building a house.");
        testSentences.add("John saw the monkey with the telescope.");
        testSentences.add("John died on the table");

        GrammarMain grammar = new GrammarMain(testSentences);

    }


}
