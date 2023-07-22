package webservice.rest;

import glueSemantics.linearLogic.Premise;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import main.NaturalDeductionProof;
import main.Settings;
import main.failExplainer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import prover.LLProver;
import prover.LLProver1;
import prover.LLProver2;
import utilities.LexVariableHandler;
import webservice.rest.dtos.GswbOutput;
import webservice.rest.dtos.GswbRequest;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.*;

@CrossOrigin
@RestController
public class GswbController {

    @Autowired
    private GswbService gswbService;

    public GswbController(){}

    @CrossOrigin
    //(origins = "http://localhost:63342")
    @PostMapping(value = "/deduce", produces = "application/json", consumes = "application/json")
    public GswbOutput applyRuleRequestXLE2(@RequestBody GswbRequest request) throws ParserInputException {


        boolean displayDRT = false;
        //    public GswbPreferences(int prover, int outputstyle, boolean solutionOnly, boolean debugging, boolean explainFail)
        Settings settings = new Settings();

        if (request.gswbPreferences.outputstyle == 4)
        {
            displayDRT = true;
            settings.setSemanticOutputStyle(1);
        } else {
            settings.setSemanticOutputStyle(request.gswbPreferences.outputstyle);
        }


        settings.setProverType(request.gswbPreferences.prover);
        settings.setDebugging(request.gswbPreferences.debugging);
        settings.setExplainFail(request.gswbPreferences.explainFail);
        settings.setParseSemantics(request.gswbPreferences.parseSem);
        settings.setNaturalDeductionOutput(request.gswbPreferences.naturalDeductionStyle);

        GlueParser gp = new GlueParser(settings);

        LinkedHashMap<Integer, List<MeaningConstructor>> mcs = gp.parseMeaningConstructorString(request.premises);
        LinkedHashMap<Integer, List<Premise>> allSolutions = new LinkedHashMap<>();

        LLProver prover = null;
        StringBuilder sb = new StringBuilder();

        if (settings.getProverType() == 0) {
        prover = new LLProver2(settings,sb);
        } else {
            prover = new LLProver1(settings,sb);
        }


        for (Integer key : mcs.keySet()) {
            try {
                List<Premise> solutions = prover.searchProof(key,mcs);
                allSolutions.put(key, solutions);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }


                /*w.append("solution" + "(" + key.toString() + i + ",");
                                    w.append(solution.getSemTerm().toString());
                                    w.append(").");

                 */


        List<String> solutions = new ArrayList<>();
        StringBuilder explainBuilder = new StringBuilder();
        for (Integer key : allSolutions.keySet()) {




            for (int i = 0; i < allSolutions.get(key).size(); i++) {
                StringBuilder solutionBuilder = new StringBuilder();
                if (settings.getSemanticOutputStyle() == 1) {
                        solutionBuilder.append("solution" + "(" + key.toString() + i + ",");
                        solutionBuilder.append(allSolutions.get(key).get(i).getSemTerm().toString());
                        solutionBuilder.append(").");




                } else if (settings.getSemanticOutputStyle() == 0) {
                    solutionBuilder.append(key.toString() + i + ": " + allSolutions.get(key).get(i).getSemTerm().toString());
                }

                solutions.add(solutionBuilder.toString());

                //outputSolutions.add(solutionBuilder.toString());
                if (settings.isExplainFail())
                {
                    try {
                        explainBuilder.append(NaturalDeductionProof.getNaturalDeductionProof(allSolutions.get(key).get(i), settings.getNaturalDeductionOutput()));
                        explainBuilder.append(System.lineSeparator());
                        explainBuilder.append(System.lineSeparator());
                    } catch(Exception e)
                    {
                        System.out.println("Failed to print natural deduction proof.");
                    }
                }
            }

            if (allSolutions.get(key).isEmpty() && settings.getProverType() == 0)
            {
                try {
                    explainBuilder.append(failExplainer.explain(((LLProver2) prover).getNonAtomicChart(), ((LLProver2) prover).getAtomicChart(), true));
                } catch(Exception e)
                {
                    System.out.println("Failed to calculate explanation.");
                }
            }

        }

        if (displayDRT)
        {
            List<String> drtSolutions = new ArrayList<>();
            //create a file that includes all Strings in solutions line  by line
            //run swipl with the file as input

            //create temporary directory gswb_resources/tmp
            File tmpDir = new File("gswb_resources/tmp");
            if (!tmpDir.exists())
            {
                tmpDir.mkdir();
            }

            File gswbFile = new File("gswb_resources/tmp/gswbFile.txt");

            try {
                if (gswbFile.createNewFile()) {
                    System.out.println("File created successfully!");
                } else {
                    System.out.println("File already exists!");
                }
            } catch (IOException e) {
                System.out.println("An error occurred while creating the file: " + e.getMessage());
                e.printStackTrace();
            }

            File drtOutputFile = new File("gswb_resources/tmp/drtOutputFile.txt");
            try {
                BufferedWriter writer = new BufferedWriter(new FileWriter(gswbFile));
                for (String solution : solutions) {
                    writer.write(solution);
                    writer.newLine();
                }
                writer.close();
            } catch (IOException e) {
                e.printStackTrace();
            }



            try {
                String[] command = {
                        "swipl",
                        "-q",
                        "-f",
                        "gswb_resources/lambdaDRT.pl",
                        "-t",
                        "main.",
                        "--",
                        gswbFile.getAbsolutePath(),
                        drtOutputFile.getAbsolutePath()
                };

                ProcessBuilder processBuilder = new ProcessBuilder(command);

                // Java join command with white space


                System.out.println("Prolog command: ");
                Arrays.asList(command).stream().forEach(s -> System.out.print(s + " "));

                processBuilder.redirectErrorStream(true);

                Process process = processBuilder.start();

                StringBuilder prettyDRT = new StringBuilder();

                // Get the input stream to read the process output
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        prettyDRT.append(line);
                        prettyDRT.append(System.lineSeparator());
                    }
                }

                System.out.println("Pretty DRT: " + prettyDRT.toString());

                // Read and print the output of the external command

                // Wait for the process to complete
                // Create a separate thread to wait for the process to finish
                FutureTask<Integer> task = new FutureTask<>(process::waitFor);
                ExecutorService executor = Executors.newFixedThreadPool(1);
                executor.execute(task);

                // Wait for 5 seconds for the process to finish
                try {
                    int exitCode = task.get(5, TimeUnit.SECONDS);
                    if (exitCode != 0) {
                        System.out.println("\nFailed to read output from lambdaDRT.pl!\n");
                    }
                } catch (Exception e) {
                    // If the process takes more than 5 seconds, destroy it
                    process.destroyForcibly();
                    System.out.println("\nProcess timed out and was forcibly terminated.\n");
                }

                executor.shutdown();

                //Kill process if it takes longer than 10 seconds




                //create a new arraylist with prettyDRT as content
                drtSolutions.add(prettyDRT.toString());
                solutions = drtSolutions;


                //delete temporary files
                gswbFile.delete();
                drtOutputFile.delete();
                tmpDir.delete();



            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        /*
        for (String solution : solutions)
        {
            System.out.println(solution);
        }
         */

        Object derivation = null;

        if (settings.getProverType() == 0) {
            derivation = explainBuilder.toString();
        } else if (prover instanceof LLProver1)
        {
            derivation = ((LLProver1) prover).analysis.returnJSONGraph();
        }

        LexVariableHandler.resetVars();

        String log = sb.toString().toString();

        if (settings.isDebugging())
        {
            log = prover.db.toString() + "\n" + log;
        }

        //transform list of premises into list of strings
        return new GswbOutput(solutions, log, derivation);
    }


}
