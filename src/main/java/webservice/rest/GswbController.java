package webservice.rest;

import glueSemantics.linearLogic.Premise;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import main.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import prover.*;
import utilities.LexVariableHandler;
import utilities.PrintDRT;
import webservice.rest.dtos.GswbBatchOutput;
import webservice.rest.dtos.GswbBatchRequest;
import webservice.rest.dtos.GswbOutput;
import webservice.rest.dtos.GswbRequest;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Logger;

@CrossOrigin
@RestController
public class GswbController {

    @Autowired
    private GswbService gswbService;
    private final static Logger LOGGER = Logger.getLogger(GswbController.class.getName());

    public GswbController(){}


    @CrossOrigin
    //(origins = "http://localhost:63342")
    @PostMapping(value = "/gswb_batch_proof", produces = "application/json", consumes = "application/json")
    public GswbBatchOutput glueBatchDeduce(@RequestBody GswbBatchRequest request) throws ParserInputException {

        boolean displayDRT = false;
        //    public GswbPreferences(int prover, int outputstyle, boolean solutionOnly, boolean debugging, boolean explainFail)
        Settings settings = new Settings();

        LOGGER.info("Received request: " + request.toString() + "\n" + "Applying settings...");

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

        LLProver prover = null;
        StringBuilder sb = new StringBuilder();

        LOGGER.info("Running prover...");

        if (settings.getProverType() == 0) {
            prover = new LLProver2(settings,sb);
        } else if (settings.getProverType() == 1) {
            prover = new LLProver1(settings,sb);
        } else if (settings.getProverType() == 2) {
            prover = new LLProver3(settings,sb);
        } else if (settings.getProverType() == 3) {
            prover = new LLProver4(settings,sb);
        }

        boolean multistage = false;
        if (settings.getProverType() == 3)
        {
            multistage = true;
        }
        HashMap<String,GswbOutput> analyses = new HashMap<>();

        StringBuilder reportBuilder = new StringBuilder();

        reportBuilder.append(System.lineSeparator());
        reportBuilder.append("ID:     No of meaning constructors:     Solutions:\n");


        List<String> keys = new ArrayList<>(request.premises.keySet());

        //sort keys by string final number
        keys.sort(new Comparator<String>() {
            @Override
            public int compare(String s1, String s2) {
                // Extract the numbers from the end of the strings
                int num1 = Integer.parseInt(s1.replaceAll("\\D", ""));
                int num2 = Integer.parseInt(s2.replaceAll("\\D", ""));

                // Compare the numbers
                return Integer.compare(num1, num2);
            }
        });


        for (int i = 0; i < keys.size(); i++)
        {
            String id = keys.get(i);
            LexicalEntries mcs =
                    gp.parseMeaningConstructorString(request.premises.get(id),multistage);

            Integer noOfMCs = 0;
            LinkedHashMap<Integer, List<Premise>> allSolutions = new LinkedHashMap<>();

            Integer countSolutions = 0;

            for (Integer key : mcs.lexicalEntries.keySet()) {
                try {
                     noOfMCs = noOfMCs + mcs.lexicalEntries.get(key).size();
                    List<Premise> solutions = prover.searchProof(key,mcs);
                    allSolutions.put(key, solutions);
                    countSolutions = countSolutions + solutions.size();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            LOGGER.info("Formatting output...");

            List<String> solutions = new ArrayList<>();
            StringBuilder explainBuilder = new StringBuilder();
            for (Integer key : allSolutions.keySet()) {

                for (int j = 0; j < allSolutions.get(key).size(); j++) {
                    StringBuilder solutionBuilder = new StringBuilder();
                    if (settings.getSemanticOutputStyle() == 1) {
                            solutionBuilder.append("solution" + "(" + key.toString() + j + ",");
                            solutionBuilder.append(allSolutions.get(key).get(j).getSemTerm().toString());
                            solutionBuilder.append(").");

                    } else if (settings.getSemanticOutputStyle() == 0) {
                            solutionBuilder.append(key.toString() + j + ": " + allSolutions.get(key).get(j).getSemTerm().toString());

                    }

                    solutions.add(solutionBuilder.toString());

                    /*
                    //outputSolutions.add(solutionBuilder.toString());
                    if (settings.isExplainFail())
                    {
                        try {
                            explainBuilder.append(NaturalDeductionProof.getNaturalDeductionProof(allSolutions.get(key).get(j), settings.getNaturalDeductionOutput()));
                            explainBuilder.append(System.lineSeparator());
                            explainBuilder.append(System.lineSeparator());
                        } catch(Exception e)
                        {
                            LOGGER.warning("Failed to print natural deduction proof.");
                        }
                    }

                     */
                }

                LOGGER.info("Preparing explanation of failure...");

                if (allSolutions.get(key).isEmpty() && settings.getProverType() == 0)
                {
                    try {
                        explainBuilder.append(failExplainer.explain(((LLProver2) prover).getNonAtomicChart(), ((LLProver2) prover).getAtomicChart(), true));
                    } catch(Exception e)
                    {
                        LOGGER.warning("Failed to calculate explanation.");
                    }
                }

            }

        /*
        for (String solution : solutions)
        {
            System.out.println(solution);
        }
         */
            LexVariableHandler.resetVars();

            String log = sb.toString().toString();

            if (settings.isDebugging())
            {
                log = prover.db.toString() + "\n" + log;
            }

            //transform list of premises into list of strings
            GswbOutput current = new GswbOutput(solutions, log, null);
            analyses.remove(id,current);

            reportBuilder.append(String.format("%s\t\t%s\t\t%s", id, noOfMCs, countSolutions));
            reportBuilder.append(System.lineSeparator());
        }
        return new GswbBatchOutput(analyses,reportBuilder.toString());
    }

    @CrossOrigin
    //(origins = "http://localhost:63342")
    @PostMapping(value = "/deduce", produces = "application/json", consumes = "application/json")
    public GswbOutput glueDeduce(@RequestBody GswbRequest request) throws ParserInputException {

        boolean displayDRT = false;
        //    public GswbPreferences(int prover, int outputstyle, boolean solutionOnly, boolean debugging, boolean explainFail)
        Settings settings = new Settings();

        LOGGER.info("Received request: " + request.toString() + "\n" + "Applying settings...");

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

        Boolean multistage = false;
        if (settings.getProverType() == 3)
        {
            multistage = true;
        }

        GlueParser gp = new GlueParser(settings);

        InputOutputProcessor.process(request.premises);
        String input = InputOutputProcessor.translate(request.premises);

        LexicalEntries mcs = gp.parseMeaningConstructorString(input, multistage);
        LinkedHashMap<Integer, List<Premise>> allSolutions = new LinkedHashMap<>();

        LLProver prover = null;
        StringBuilder sb = new StringBuilder();

        LOGGER.info("Running prover...");

        if (settings.getProverType() == 0) {
        prover = new LLProver2(settings,sb);
        } else if (settings.getProverType() == 1) {
            prover = new LLProver1(settings,sb);
        } else if (settings.getProverType() == 2) {
            prover = new LLProver3(settings,sb);
        } else if (settings.getProverType() == 3) {
            prover = new LLProver4(settings,sb);
        }

        for (Integer key : mcs.lexicalEntries.keySet()) {
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

        LOGGER.info("Formatting output...");

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
                        LOGGER.warning("Failed to print natural deduction proof.");
                    }
                }
            }

            LOGGER.info("Preparing explanation of failure...");

            if (allSolutions.get(key).isEmpty() && settings.getProverType() == 0)
            {
                try {
                    explainBuilder.append(failExplainer.explain(((LLProver2) prover).getNonAtomicChart(), ((LLProver2) prover).getAtomicChart(), true));
                } catch(Exception e)
                {
                    LOGGER.warning("Failed to calculate explanation.");
                }
            }
        }

        if (displayDRT)
        {
            if (!solutions.isEmpty()) {
                solutions = PrintDRT.printDRT(solutions);
            }
        }

        Object derivation = null;

        if (settings.isExplainFail()) {
            if (settings.getProverType() == 0) {
                derivation = explainBuilder.toString();
            } else if (prover instanceof LLProver1) {
                derivation = ((LLProver1) prover).analysis.returnJSONGraph();
            } else if (prover instanceof LLProver4) {
                derivation = ((LLProver4) prover).analysis.returnJSONGraph();
            }
        }

        LexVariableHandler.resetVars();

        String log = sb.toString();

        if (settings.isDebugging())
        {
            log = prover.db.toString() + "\n" + log;
            LOGGER.info("Debugging output: \n" + prover.db.toString());
        }

        //transform list of premises into list of strings
        return new GswbOutput(solutions, log, derivation);
    }


}
