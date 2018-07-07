package gluePaP.glue;

import Prover.VariableBindingException;
import edu.stanford.nlp.trees.GrammaticalStructure;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

// Base class for creating glue semantic representations with a meaning side and a glue side
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

    public GrammarMain(String parseSingle) throws VariableBindingException
    {
        this.ps = new ArrayList<String>();
        ps.add(parseSingle);
        List<GrammaticalStructure> parses =  dp.generateParses(this.ps);

        for (GrammaticalStructure parse : parses)
        {
            SentenceMeaning meaning = new SentenceMeaning(parse);
        }


    }

    public static void main(String[] args) throws VariableBindingException {

        List<String> testSentences = new ArrayList<String>(Arrays.asList(args));
        if (testSentences.isEmpty()) {
            Scanner s = new Scanner(System.in);
            String input;
            while (true) {
                System.out.println("Enter sentence to be analyzed or enter 'quit'.");
                input = s.nextLine();
                if (input.equals("quit"))
                    break;
                new GrammarMain(input);

            }

        }
        else {
            new GrammarMain(testSentences);
        }


//        testSentences.add("The ball flew through the broken window");
  //      testSentences.add("John snores.");
      //  testSentences.add("John loves Maria.");
//        testSentences.add("Every man owns a dog");
   //     testSentences.add("A loud dog barks.");
 //       testSentences.add("Every man with a dog owns a leash.");
     //   testSentences.add("A loud dog chases every small cat.");
/*        testSentences.add("John is happy.");
        testSentences.add("John ran quickly.");
        testSentences.add("John died on the bed");
        testSentences.add("John said that Karen was sick.");
        testSentences.add("Es regnet.");
*/
//        testSentences.add("John was able to open the door.");
//        testSentences.add("John saw the monkey with the telescope.");



    }


}
