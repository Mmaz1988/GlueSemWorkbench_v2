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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

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


        //    public GswbPreferences(int prover, int outputstyle, boolean solutionOnly, boolean debugging, boolean explainFail)
        Settings settings = new Settings();
        settings.setSemanticOutputStyle(request.gswbPreferences.outputstyle);
        settings.setProverType(request.gswbPreferences.prover);
        settings.setDebugging(request.gswbPreferences.debugging);
        settings.setExplainFail(request.gswbPreferences.explainFail);
        settings.setParseSemantics(request.gswbPreferences.parseSem);
        settings.setNaturalDeductionOutput(request.gswbPreferences.naturalDeductionStyle);

        GlueParser gp = new GlueParser(settings.isParseSemantics());

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


        List<java.lang.String> solutions = new ArrayList<>();
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
