package gluePaP.glue;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.process.TokenizerFactory;
import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.stanford.nlp.process.CoreLabelTokenFactory;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.trees.*;

public class DependencyParser {
    private final static String PCG_MODEL = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz";

    private final TokenizerFactory<CoreLabel> tokenizerFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "invertible=true");

    private final TreebankLanguagePack tlp = new PennTreebankLanguagePack();

    //for dependencies
    private final GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();

    private final LexicalizedParser parser = LexicalizedParser.loadModel(PCG_MODEL);

    public List<GrammaticalStructure> parseSet;


    public DependencyParser(){}


    //Main method for testing parser functionalities

    public static void main(String[] args) {

        DependencyParser dp = new DependencyParser();

        List<String> testSentences = new ArrayList<String>();

        testSentences.add("John snores.");

        // TODO: For some reason ungrammatical
        // testSentences.add("Cats chase dogs.");
        testSentences.add("John loves Maria.");
        testSentences.add("Every man loves a woman.");



        DependencyParser parser = new DependencyParser();

        for (String sentence : testSentences)
        {
            Tree tree = parser.parse(sentence);
            GrammaticalStructure gs = dp.gsf.newGrammaticalStructure(tree);


            tree.pennPrint();
            System.out.print(gs.typedDependenciesEnhanced());
            System.out.println();
            System.out.print(gs.typedDependencies());
        }
    }


    public List<GrammaticalStructure> generateParses(List<String> sentences) {

        List<GrammaticalStructure> parsedSentences = new ArrayList<>();

        for (String sentence : sentences)
        {
            Tree tree = parser.parse(sentence);
            GrammaticalStructure gs = gsf.newGrammaticalStructure(tree);

            parsedSentences.add(gs);
        }
        return parsedSentences;
    }


    public Tree parse(String str) {
        List<CoreLabel> tokens = tokenize(str);
        Tree tree = parser.apply(tokens);
        return tree;
    }

    private List<CoreLabel> tokenize(String str) {
        Tokenizer<CoreLabel> tokenizer =
                tokenizerFactory.getTokenizer(
                        new StringReader(str));
        return tokenizer.tokenize();
    }

    }


