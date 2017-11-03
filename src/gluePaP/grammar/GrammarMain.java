package gluePaP.grammar;

// Base class for creating glue semantic representations with a meaning side and a glue side

import edu.stanford.nlp.trees.GrammaticalStructure;

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


}
