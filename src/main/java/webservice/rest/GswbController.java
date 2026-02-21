package webservice.rest;

import Discriminants.ScopeDiscriminant;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import Discriminants.McDiscriminant;
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

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

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

        String resolveSetting = "false";
        if (request.gswbPreferences.resolveDrs)
        {
            resolveSetting = "true";
        }


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
            LinkedHashMap<Integer, List<SolutionObject>> allSolutions = new LinkedHashMap<>();

            Integer countSolutions = 0;

            for (Integer key : mcs.lexicalEntries.keySet()) {
                try {
                     noOfMCs = noOfMCs + mcs.lexicalEntries.get(key).size();
                    List<SolutionObject> solutions = prover.searchProof(key,mcs);
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
                            solutionBuilder.append(allSolutions.get(key).get(j).solution.getSemTerm().toString());
                            solutionBuilder.append(").");

                    } else if (settings.getSemanticOutputStyle() == 0) {
                            solutionBuilder.append(key.toString() + j + ": " + allSolutions.get(key).get(j).solution.getSemTerm().toString());

                    }

                    if (displayDRT)
                    {
                        List<String> drtSolutions = new ArrayList<>();
                        drtSolutions.add(solutionBuilder.toString());

                        List<String> drts =  PrintDRT.printDRT(drtSolutions, resolveSetting)
                                .stream()
                                .flatMap(s -> Arrays.stream(s.split("####")))
                                .map(String::trim)
                                .filter(part -> !part.isEmpty())
                                .collect(Collectors.toList());

                        solutions.add(String.join("\n\n",drts));
                    } else {
                        solutions.add(solutionBuilder.toString());
                    }
                    //

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
            analyses.put(id,current);

            reportBuilder.append(String.format("%s\t\t%s\t\t\t%s", id, noOfMCs, countSolutions));
            reportBuilder.append(System.lineSeparator());
        }


        LOGGER.info("Finished processing with GSWB ... Returning results.");
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

        String resolveSetting = "false";
        if (request.gswbPreferences.resolveDrs)
        {
            resolveSetting = "true";
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
        LinkedHashMap<Integer, List<SolutionObject>> allSolutions = new LinkedHashMap<>();

        LLProver prover = null;
        StringBuilder sb = new StringBuilder();
        String log = "";

        LOGGER.info("Running prover...");

        //0 == Hepple prover (Prover 2), 1 == Lev Prover (prover 1), 4 == multistage prover (prover 4)

        if (settings.getProverType() == 0) {
        prover = new LLProver2(settings,sb);
        } else if (settings.getProverType() == 1) {
            prover = new LLProver1(settings,sb);
        } else if (settings.getProverType() == 2) {
            prover = new LLProver3(settings,sb);
        }

        HashSet<Integer> mcSetWithSolution = new HashSet<>();
        for (Integer key : mcs.lexicalEntries.keySet()) {
            try {
                List<SolutionObject> solutions = prover.searchProof(key,mcs);
                if (!solutions.isEmpty())
                {
                    mcSetWithSolution.add(key);
                }
                allSolutions.put(key, solutions);

                log = log + "#### Proof with index " + key + " ####\n";
                log = log + sb.toString() + "\n";

                if (settings.isDebugging())
                {
                    log = log + prover.db.toString() + "\n\n";
                    LOGGER.info("Debugging output: \n" + prover.db.toString());
                }

                //reset stringbuilder to empty string
                sb.setLength(0);


            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        //copy lexical entries and remove all entries whose key is not in mcSetWithSolution
        LinkedHashMap<Integer, List<MeaningConstructor>> filteredLexicalEntries = new LinkedHashMap<>();
        for (Integer key : mcs.lexicalEntries.keySet()) {
            if (mcSetWithSolution.contains(key))
            {                filteredLexicalEntries.put(key, mcs.lexicalEntries.get(key));
            }
        }

        LexicalEntries filteredMcs = new LexicalEntries(filteredLexicalEntries);

        List<McDiscriminant> discriminants = filteredMcs.calculateDiscriminants();



                /*w.append("solution" + "(" + key.toString() + i + ",");
                                    w.append(solution.getSemTerm().toString());
                                    w.append(").");

                 */

        LOGGER.info("Formatting output...");


        HashMap<Integer,SolutionObject> solutionStringsToObject = new HashMap<>();

        List<String> solutions = new ArrayList<>();
        StringBuilder explainBuilder = new StringBuilder();

        HashMap<String,ScopeDiscriminant> scopeDiscriminants = new HashMap<>();

        int solutionIndex = 0;
        int scopeDiscriminantIndex = 0;
        for (Integer key : allSolutions.keySet()) {
            for (int i = 0; i < allSolutions.get(key).size(); i++) {
                StringBuilder solutionBuilder = new StringBuilder();
                if (settings.getSemanticOutputStyle() == 1) {
                        solutionBuilder.append("solution" + "(" + key.toString() + i + ",");
                        solutionBuilder.append(allSolutions.get(key).get(i).solution.getSemTerm().toString());
                        solutionBuilder.append(").");

                } else if (settings.getSemanticOutputStyle() == 0) {
                    solutionBuilder.append(key.toString() + "." + i + ": " + allSolutions.get(key).get(i).solution.getSemTerm().toString());
                }

                SolutionObject currentSO = allSolutions.get(key).get(i);

                String currentSolution = solutionBuilder.toString().trim();

                for (McDiscriminant d : discriminants) {
                    if (d.mcSetIds.contains(key)) {
                        d.associatedSolutions.add("s" +  solutionIndex);
                    }
                }

                for (String sd : currentSO.scopeDiscriminants){
                   if (!scopeDiscriminants.containsKey(sd)){
                       ScopeDiscriminant newSD = new ScopeDiscriminant("sc" + scopeDiscriminantIndex,sd, new HashSet<>());
                       newSD.solutionIds.add("s" +  solutionIndex);
                       scopeDiscriminants.put(sd,newSD);
                       scopeDiscriminantIndex++;
                       continue;
                   }
                   scopeDiscriminants.get(sd).solutionIds.add("s" +  solutionIndex);
                }


                currentSO.solutionString = currentSolution;
                currentSO.solutionId = "s" +  solutionIndex;
                solutions.add(currentSolution);

                solutionStringsToObject.put(solutionIndex,allSolutions.get(key).get(i));
                solutionIndex++;

                //outputSolutions.add(solutionBuilder.toString());
                if (settings.isExplainFail())
                {
                    try {
                        explainBuilder.append(NaturalDeductionProof.getNaturalDeductionProof(allSolutions.get(key).get(i).solution, settings.getNaturalDeductionOutput()));
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
                solutions =  PrintDRT.printDRT(solutions, resolveSetting)
                        .stream()
                        .flatMap(s -> Arrays.stream(s.split("####")))
                        .map(String::trim)
                        .filter(part -> !part.isEmpty())
                        .collect(Collectors.toList());
            }
        }

        if (solutions.size() == solutionStringsToObject.keySet().size())
        {
            for (Integer key : solutionStringsToObject.keySet()) {
                solutionStringsToObject.get(key).solutionString = solutions.get(key);
            }
        }


        Object derivation = null;

        if (settings.isExplainFail()) {
            if (settings.getProverType() == 0) {
                derivation = explainBuilder.toString();
            } else if (prover instanceof LLProver1) {
                derivation = ((LLProver1) prover).analysis.returnJSONGraph();
            } else if (prover instanceof LLProver3) {
                derivation = ((LLProver3) prover).analysis.returnJSONGraph();
            }
        }

        LexVariableHandler.resetVars();

        //transform list of premises into list of strings
        LOGGER.info("Finished processing with GSWB ... Returning results.");
        return new GswbOutput(solutions, log, derivation);
    }


}
