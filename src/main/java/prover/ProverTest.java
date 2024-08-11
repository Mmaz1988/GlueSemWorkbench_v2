package prover;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import main.InputOutputProcessor;
import main.Settings;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.*;

public class ProverTest {

    private Settings testSettings = new Settings();
    private static Path testFolderPath = Path.of("gswb_resources/test_files");

    private static Integer noOfIterations = 100;

    public LinkedHashMap<Integer, List<MeaningConstructor>> loadMeaningConstructors(String fileName) throws ParserInputException {
        List<String> formulas = new ArrayList<>();

        Path inputFileStream = testFolderPath.resolve(fileName);
        StringBuilder inputStringBuilder = new StringBuilder();

        /* Read the input */
        Scanner scanner = null;
        try {
            scanner = new Scanner(inputFileStream);
            while (scanner.hasNextLine()) {
                inputStringBuilder.append(scanner.nextLine() + System.lineSeparator());
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (scanner != null)
                scanner.close();
        }
        InputOutputProcessor.process(inputStringBuilder.toString());
        String input = InputOutputProcessor.translate(inputStringBuilder.toString());

        String lines[] = input.split("\\r?\\n|\\r");

        GlueParser parser = new GlueParser(testSettings);

        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = parser.parseMeaningConstructorList(Arrays.asList(lines));

        return lexicalEntries;
    }


    @Test
    public void testHeppleWorstCase() throws ParserInputException, VariableBindingException, ProverException {
        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = loadMeaningConstructors("hepple_worst_case.txt");
        testSettings.setProverType(Settings.HEPPLE);
        testSettings.setDebugging(true);
        assert (lexicalEntries.size() == 1);
        assert (lexicalEntries.get(1).size() == 9);

        LLProver prover = new LLProver2(testSettings);

        List<Long> processingTimes = new ArrayList<>();

        for (int i = 0; i < noOfIterations; i++) {
            for (Integer key : lexicalEntries.keySet()) {
                prover.searchProof(key, lexicalEntries, true);
                if (!prover.getSolutions().isEmpty()) {
                    processingTimes.add(prover.db.computationTime);
                }
            }
        }

        // assert(processingTimes.size() == noOfIterations);

        //calculate average of processing times
        Long sum = 0L;
        for (Long time : processingTimes) {
            sum += time;
        }
        Long average = sum / processingTimes.size();

        System.out.println("Average processing time: " + average / 1000000 + "ms for " + processingTimes.size() + " proofs");

    }

    @Test
    public void testHeppleWorstCase2() throws ParserInputException, VariableBindingException, ProverException {
        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = loadMeaningConstructors("lev_worst_case.txt");
        testSettings.setProverType(Settings.HEPPLE);
        testSettings.setDebugging(true);
        assert (lexicalEntries.size() == 8);
        assert (lexicalEntries.get(1).size() == 9);

        LLProver prover = new LLProver2(testSettings);

        List<Long> processingTimes = new ArrayList<>();

        for (int i = 0; i < noOfIterations; i++) {
            for (Integer key : lexicalEntries.keySet()) {
                prover.searchProof(key, lexicalEntries, true);
                if (!prover.getSolutions().isEmpty()) {
                    processingTimes.add(prover.db.computationTime);
                }
            }
        }

        // assert(processingTimes.size() == noOfIterations);

        //calculate average of processing times
        Long sum = 0L;
        for (Long time : processingTimes) {
            sum += time;
        }
        Long average = sum / processingTimes.size();

        System.out.println("Average processing time: " + average / 1000000 + "ms for " + processingTimes.size() + " proofs");

    }

    @Test
    public void testLevWorstCase() throws ParserInputException, VariableBindingException, ProverException {
        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = loadMeaningConstructors("lev_worst_case.txt");
        testSettings.setProverType(Settings.LEV);
        testSettings.setDebugging(true);
        assert (lexicalEntries.size() == 8);
        assert (lexicalEntries.get(1).size() == 9);

        LLProver prover = new LLProver1(testSettings);

        List<Long> processingTimes = new ArrayList<>();

        for (int i = 0; i < noOfIterations; i++) {
            for (Integer key : lexicalEntries.keySet()) {
                prover.searchProof(key, lexicalEntries, true);
                if (!prover.getSolutions().isEmpty()) {
                    processingTimes.add(prover.db.computationTime);
                }
            }
        }

        // assert(processingTimes.size() == noOfIterations);

        //calculate average of processing times
        Long sum = 0L;
        for (Long time : processingTimes) {
            sum += time;
        }
        Long average = sum / processingTimes.size();

        System.out.println("Average processing time: " + average / 1000000 + "ms for " + processingTimes.size() + " proofs");

    }

    @Test
    public void testNestedQuantifierHepple() throws ParserInputException, VariableBindingException, ProverException {
        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = loadMeaningConstructors("nested_quantifiers1.txt");
        testSettings.setProverType(Settings.HEPPLE);
        testSettings.setDebugging(true);
        assert (lexicalEntries.size() == 1);
        assert (lexicalEntries.get(1).size() == 6);

        LLProver prover = new LLProver2(testSettings);

        List<Long> processingTimes = new ArrayList<>();

        for (int i = 0; i < noOfIterations; i++) {
            for (Integer key : lexicalEntries.keySet()) {
                prover.searchProof(key, lexicalEntries, true);
                if (!prover.getSolutions().isEmpty()) {
                    processingTimes.add(prover.db.computationTime);
                }
            }
        }

        prover.db.toString();
        // assert(processingTimes.size() == noOfIterations);

        //calculate average of processing times
        Long sum = 0L;
        for (Long time : processingTimes) {
            sum += time;
        }
        Long average = sum / processingTimes.size();

        System.out.println("Average processing time: " + average / 1000000 + "ms for " + processingTimes.size() + " proofs");

    }

    @Test
    public void testNestedQuantifierLev() throws ParserInputException, VariableBindingException, ProverException {
        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = loadMeaningConstructors("nested_quantifiers2.txt");
        testSettings.setProverType(Settings.LEV);
        testSettings.setDebugging(true);
        assert (lexicalEntries.size() == 2);
        assert (lexicalEntries.get(1).size() == 6);

        LLProver prover = new LLProver1(testSettings);

        List<Long> processingTimes = new ArrayList<>();

        for (int i = 0; i < noOfIterations; i++) {
            for (Integer key : lexicalEntries.keySet()) {
                prover.searchProof(key, lexicalEntries, true);
                if (!prover.getSolutions().isEmpty()) {
                    processingTimes.add(prover.db.computationTime);
                }
            }
        }

        prover.db.toString();

        // assert(processingTimes.size() == noOfIterations);

        //calculate average of processing times
        Long sum = 0L;
        for (Long time : processingTimes) {
            sum += time;
        }
        Long average = sum / processingTimes.size();

        System.out.println("Average processing time: " + average / 1000000 + "ms for " + processingTimes.size() + " proofs");

    }


}