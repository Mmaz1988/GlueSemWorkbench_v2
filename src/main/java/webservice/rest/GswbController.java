package webservice.rest;

import Discriminants.ScopeDiscriminant;
import de.ukon.lfgxdrt.SemanticExpression;
import de.ukon.lfgxdrt.drs_elements.DRS;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import Discriminants.McDiscriminant;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import de.ukon.lfgxdrt.DrsSvgRenderer;
import main.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import prover.*;
import utilities.LexVariableHandler;
import utilities.PrintDRT;
import webservice.rest.dtos.*;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

@CrossOrigin
@RestController
public class GswbController {

    @Autowired
    private GswbService gswbService;
    @Autowired
    private GswbRedisSessionService gswbRedisSessionService;
    private final static Logger LOGGER = Logger.getLogger(GswbController.class.getName());

    public GswbController(){}

    @CrossOrigin
    @PostMapping(value = "/gswb_batch_proof", produces = "application/json", consumes = "application/json")
    public GswbBatchOutput glueBatchDeduce(@RequestBody GswbBatchRequest request) throws Exception {

        RunContext ctx = buildRunContext(request.gswbPreferences);
        LOGGER.info("Received request: " + request.toString() + "\n" + "Applying settings...");

        GlueParser gp = new GlueParser(ctx.settings);

        HashMap<String, GswbOutput> analyses = new HashMap<>();
        String sessionKey = (request.sessionKey == null || request.sessionKey.isBlank()) ? "last_session" : request.sessionKey;
        StringBuilder reportBuilder = new StringBuilder()
                .append(System.lineSeparator())
                .append("ID:     No of meaning constructors:     Solutions:\n");

        gswbRedisSessionService.clear(sessionKey);

        List<String> ids = sortedBatchKeys(request.premises.keySet());

        for (String id : ids) {
            SingleRunResult run =
                    runAndFormatSingle(
                            request.premises.get(id),
                            ctx,
                            gp,
                            true,  // batch mode
                            false  // includeDerivation
                    );

            analyses.put(id, run.output);

            reportBuilder.append(String.format("%s\t\t%s\t\t\t%s", id, run.noOfMCs, run.countSolutions));
            reportBuilder.append(System.lineSeparator());

            gswbRedisSessionService.saveBatchOutput(sessionKey, new GswbBatchOutput(new HashMap<>(analyses), reportBuilder.toString()));
        }

        LOGGER.info("Finished processing with GSWB ... Returning results.");
        GswbBatchOutput batchOutput = new GswbBatchOutput(analyses, reportBuilder.toString());
        gswbRedisSessionService.saveBatchOutput(sessionKey, batchOutput);
        return batchOutput;
    }

    @CrossOrigin
    @PostMapping(value = "/deduce", produces = "application/json", consumes = "application/json")
    public GswbOutput glueDeduce(@RequestBody GswbRequest request) throws Exception {

        RunContext ctx = buildRunContext(request.gswbPreferences);
        LOGGER.info("Received request: " + request.toString() + "\n" + "Applying settings...");

        GlueParser gp = new GlueParser(ctx.settings);

        InputOutputProcessor.process(request.premises);
        String input = InputOutputProcessor.translate(request.premises);

        SingleRunResult run =
                runAndFormatSingle(
                        input,
                        ctx,
                        gp,
                        false, // single mode
                        true   // includeDerivation
                );

        return run.output;
    }

    @CrossOrigin
    @GetMapping(value = "/gswb_batch_session/{sessionKey}/summary", produces = "application/json")
    public HashMap<String, Object> getBatchSummary(@org.springframework.web.bind.annotation.PathVariable String sessionKey) {
        return gswbRedisSessionService.summarizeBatchOutput(sessionKey);
    }

    @CrossOrigin
    @GetMapping(value = "/gswb_batch_session/{sessionKey}", produces = "application/json")
    public GswbBatchOutput getBatchSession(@org.springframework.web.bind.annotation.PathVariable String sessionKey) {
        return gswbRedisSessionService.loadBatchOutput(sessionKey);
    }

    @CrossOrigin
    @org.springframework.web.bind.annotation.DeleteMapping(value = "/gswb_batch_session/{sessionKey}", produces = "application/json")
    public HashMap<String, String> deleteBatchSession(@org.springframework.web.bind.annotation.PathVariable String sessionKey) {
        gswbRedisSessionService.clear(sessionKey);
        HashMap<String, String> response = new HashMap<>();
        response.put("status", "ok");
        return response;
    }

    // --------------------------
    // Parallel pipeline helpers (both endpoints call the same ones)
    // --------------------------

    private static final class RunContext {
        private final Settings settings;
        private final boolean displayDRT;
        private final boolean displayLfgxDrt;
        private final boolean multistage;

        private RunContext(Settings settings, boolean displayDRT, boolean displayLfgxDrt, boolean multistage) {
            this.settings = settings;
            this.displayDRT = displayDRT;
            this.displayLfgxDrt = displayLfgxDrt;
            this.multistage = multistage;
        }
    }

    private static final class LLProverAndLog {
        private final LLProver prover;
        private final StringBuilder sb;

        private LLProverAndLog(LLProver prover, StringBuilder sb) {
            this.prover = prover;
            this.sb = sb;
        }
    }

    private static final class SolutionsAndDiscriminants {
        private final List<String> solutionStrings;
        private final Map<Integer, SolutionObject> solutionIndexToObject;
        private final List<ScopeDiscriminant> finalScopeDiscriminants;

        private SolutionsAndDiscriminants(
                List<String> solutionStrings,
                Map<Integer, SolutionObject> solutionIndexToObject,
                List<ScopeDiscriminant> finalScopeDiscriminants
        ) {
            this.solutionStrings = solutionStrings;
            this.solutionIndexToObject = solutionIndexToObject;
            this.finalScopeDiscriminants = finalScopeDiscriminants;
        }
    }

    private static final class SingleRunResult {
        private final GswbOutput output;
        private final int noOfMCs;
        private final int countSolutions;

        private SingleRunResult(GswbOutput output, int noOfMCs, int countSolutions) {
            this.output = output;
            this.noOfMCs = noOfMCs;
            this.countSolutions = countSolutions;
        }
    }

    private RunContext buildRunContext(GswbPreferences prefs) {
        boolean displayDRT = false;
        boolean displayLfgxDrt = false;
        Settings settings = new Settings();

        if (prefs.outputstyle == 4) {
            displayDRT = true;
            settings.setSemanticOutputStyle(1);
        } else if (prefs.outputstyle == Settings.LFGXDRT) {
            displayLfgxDrt = true;
            settings.setSemanticOutputStyle(Settings.LFGXDRT);
        } else {
            settings.setSemanticOutputStyle(prefs.outputstyle);
        }

        settings.setProverType(prefs.prover);
        settings.setAllowRelaxedGraph(true);
        settings.setDebugging(prefs.debugging);
        settings.setExplainFail(prefs.explainFail);
        settings.setParseSemantics(prefs.parseSem);
        settings.setNaturalDeductionOutput(prefs.naturalDeductionStyle);
        // DRS resolution requires beta reduction. Keep the effective settings
        // consistent even when a client sends an invalid combination.
        boolean betaReduce = prefs.betaReduce || prefs.resolveDrs;
        settings.setBetaReduce(betaReduce);
        settings.setResolveDrs(prefs.resolveDrs);

        boolean multistage = (settings.getProverType() == 3);

        return new RunContext(settings, displayDRT, displayLfgxDrt, multistage);
    }

    private SingleRunResult runAndFormatSingle(
            String premiseInput,
            RunContext ctx,
            GlueParser gp,
            boolean batchMode,
            boolean includeDerivation
    ) throws Exception {

        LLProverAndLog proverAndLog = createProver(ctx.settings);
        LLProver prover = proverAndLog.prover;
        StringBuilder sb = proverAndLog.sb;

        LOGGER.info("Running prover...");

        LexicalEntries mcs = gp.parseMeaningConstructorString(premiseInput, ctx.multistage);

        ProofRun run = runProofsOverLexicalEntries(mcs, prover, sb, ctx.settings, batchMode);

        LexicalEntries filteredMcs = filterLexicalEntriesByKeySet(mcs, run.mcSetWithSolution);
        List<McDiscriminant> finalMcDiscriminants = filteredMcs.calculateDiscriminants();

        LOGGER.info("Formatting output...");

        SolutionsAndDiscriminants formatted =
                formatSolutionsAndDiscriminants(run.allSolutions, finalMcDiscriminants, prover.scope2instantiations, ctx.settings);

        applyOptionalSemanticRendering(ctx, formatted);

        List<GswbSolution> outputSolutions = toOutputSolutions(formatted.solutionIndexToObject);

        Object derivation = null;
        if (includeDerivation) {
            derivation = buildDerivationIfEnabled(ctx.settings, prover);
        }

        List<GswbDiscriminant> outputDiscriminants =
                toOutputDiscriminants(formatted.finalScopeDiscriminants, finalMcDiscriminants);

        LexVariableHandler.resetVars();

        String log = run.log;
        if (ctx.settings.isDebugging()) {
            log = prover.db.toString() + "\n" + log;
        }

        return new SingleRunResult(
                new GswbOutput(outputSolutions, log, derivation, outputDiscriminants),
                run.noOfMCs,
                run.countSolutions
        );
    }

    private Object buildDerivationIfEnabled(Settings settings, LLProver prover) {
        if (!settings.isExplainFail()) {
            return null;
        }

        // Preserve prior behavior for the graph-based provers.
        if (prover instanceof LLProver1 p1) {
            return (p1.analysis != null) ? p1.analysis.returnJSONGraph() : null;
        }
        if (prover instanceof LLProver3 p3) {
            return (p3.analysis != null) ? p3.analysis.returnJSONGraph() : null;
        }

        // For the Hepple prover path (LLProver2), the old controller code built a textual explanation
        // via charts + fail explainer. That logic isn't currently part of the shared pipeline.
        // Returning null here keeps behavior safe/non-breaking until that is refactored in similarly.
        return null;
    }

    private static final class ProofRun {
        private final LinkedHashMap<Integer, List<SolutionObject>> allSolutions;
        private final HashSet<Integer> mcSetWithSolution;
        private final int noOfMCs;
        private final int countSolutions;
        private final String log;

        private ProofRun(
                LinkedHashMap<Integer, List<SolutionObject>> allSolutions,
                HashSet<Integer> mcSetWithSolution,
                int noOfMCs,
                int countSolutions,
                String log
        ) {
            this.allSolutions = allSolutions;
            this.mcSetWithSolution = mcSetWithSolution;
            this.noOfMCs = noOfMCs;
            this.countSolutions = countSolutions;
            this.log = log;
        }
    }

    private ProofRun runProofsOverLexicalEntries(
            LexicalEntries mcs,
            LLProver prover,
            StringBuilder sb,
            Settings settings,
            boolean batchMode
    ) {
        int noOfMCs = 0;
        int countSolutions = 0;

        LinkedHashMap<Integer, List<SolutionObject>> allSolutions = new LinkedHashMap<>();
        HashSet<Integer> mcSetWithSolution = new HashSet<>();

        String log = "";

        for (Integer key : mcs.lexicalEntries.keySet()) {
            try {
                noOfMCs += mcs.lexicalEntries.get(key).size();

                List<SolutionObject> solutions = prover.searchProof(key, mcs);
                allSolutions.put(key, solutions);

                if (!solutions.isEmpty()) {
                    mcSetWithSolution.add(key);
                }

                countSolutions += solutions.size();

                if (!batchMode) {
                    log = log + "#### Proof with index " + key + " ####\n";
                    log = log + sb.toString() + "\n";
                }

                sb.setLength(0);

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return new ProofRun(allSolutions, mcSetWithSolution, noOfMCs, countSolutions, log);
    }

    private void applyOptionalSemanticRendering(RunContext ctx, SolutionsAndDiscriminants formatted) throws Exception {
        if ((!ctx.displayDRT && !ctx.displayLfgxDrt) || formatted.solutionStrings.isEmpty()) {
            return;
        }
        if (ctx.displayLfgxDrt) {
            for (Integer idx : formatted.solutionIndexToObject.keySet().stream().sorted().toList()) {
                SolutionObject so = formatted.solutionIndexToObject.get(idx);
                if (!(so.solution.getSemTerm() instanceof glueSemantics.semantics.LfgxDrtSemanticRepresentation)) {
                    throw new RuntimeException("Expected LFGxDRT semantic payload for solution " + idx);
                }
                glueSemantics.semantics.LfgxDrtSemanticRepresentation wrapped =
                        (glueSemantics.semantics.LfgxDrtSemanticRepresentation) so.solution.getSemTerm();
                SemanticExpression sol = wrapped.getDelegate();
                String assembledSolution = sol.toString();
                LOGGER.fine("Assembled LFGxDRT solution " + idx + ": " + assembledSolution);
                if (ctx.settings.isBetaReduce()) {
                    sol = sol.betaReduce();
                }
                if (ctx.settings.isResolveDrs()) {
                    sol = sol.resolveMerges();
                }

                sol.setSourceIndex(so.sourceIndex);

                // SVG supports unresolved lambda/function-application structure;
                // LiGER graph conversion requires beta-reduced semantics.
                so.graph = null;
                if (ctx.settings.isBetaReduce()) {
                    try {
                        so.graph = sol.toJson();
                    } catch (Exception e) {
                        LOGGER.warning("Could not render LiGER graph for solution " + idx + ": " + e.getMessage());
                    }
                }



                try {
                    so.solutionString = new DrsSvgRenderer().toSvg(sol);
                } catch (Exception e) {
                    throw new RuntimeException("Failed to render SVG for solution " + idx, e);
                }
            }
            return;
        }

        List<String> rendered = PrintDRT.printDRT(formatted.solutionStrings, ctx.settings.isResolveDrs())
                .stream()
                .flatMap(s -> Arrays.stream(s.split("####")))
                .map(String::trim)
                .filter(part -> !part.isEmpty())
                .collect(Collectors.toList());

        if (rendered.size() == formatted.solutionIndexToObject.keySet().size()) {
            for (Integer idx : formatted.solutionIndexToObject.keySet().stream().sorted().toList()) {
                formatted.solutionIndexToObject.get(idx).solutionString = rendered.get(idx);
            }
        }
    }

    private List<GswbSolution> toOutputSolutions(Map<Integer, SolutionObject> solutionsByIndex) {
        List<GswbSolution> outputSolutions = new ArrayList<>();
        for (Integer idx : solutionsByIndex.keySet().stream().sorted().toList()) {
            SolutionObject so = solutionsByIndex.get(idx);
            outputSolutions.add(new GswbSolution(so.solutionString, so.solutionId, so.sourceIndex, so.graph));
        }
        return outputSolutions;
    }

    private LLProverAndLog createProver(Settings settings) {
        StringBuilder sb = new StringBuilder();
        LLProver prover = null;

        if (settings.getProverType() == 0) {
            prover = new LLProver2(settings, sb);
        } else if (settings.getProverType() == 1) {
            prover = new LLProver1(settings, sb);
        } else if (settings.getProverType() == 2) {
            prover = new LLProver3(settings, sb);
        }

        return new LLProverAndLog(prover, sb);
    }

    private List<String> sortedBatchKeys(Set<String> ids) {
        List<String> keys = new ArrayList<>(ids);
        keys.sort(Comparator.comparingInt(s -> Integer.parseInt(s.replaceAll("\\D", ""))));
        return keys;
    }

    private LexicalEntries filterLexicalEntriesByKeySet(LexicalEntries mcs, Set<Integer> keysToKeep) {
        LinkedHashMap<Integer, List<MeaningConstructor>> filtered = new LinkedHashMap<>();
        for (Integer key : mcs.lexicalEntries.keySet()) {
            if (keysToKeep.contains(key)) {
                filtered.put(key, mcs.lexicalEntries.get(key));
            }
        }
        return new LexicalEntries(filtered);
    }

    private SolutionsAndDiscriminants formatSolutionsAndDiscriminants(
            LinkedHashMap<Integer, List<SolutionObject>> allSolutions,
            List<McDiscriminant> finalMcDiscriminants,
            LinkedHashMap<String, LinkedHashSet<String>> scope2instantiations,
            Settings settings
    ) {
        Map<Integer, SolutionObject> solutionIndexToObject = new HashMap<>();
        List<String> solutions = new ArrayList<>();

        HashMap<String, ScopeDiscriminant> scopeDiscriminants = new HashMap<>();

        int solutionIndex = 0;
        int scopeDiscriminantIndex = 0;

        for (Integer key : allSolutions.keySet()) {
            for (int i = 0; i < allSolutions.get(key).size(); i++) {

                SolutionObject currentSO = allSolutions.get(key).get(i);

                String currentSolution = buildSolutionString(settings, key, i, currentSO).trim();

                for (McDiscriminant d : finalMcDiscriminants) {
                    if (d.mcSetIds.contains(key)) {
                        d.associatedSolutions.add("s" + solutionIndex);
                    }
                }

                for (String sd : currentSO.scopeDiscriminants) {
                    ScopeDiscriminant existing = scopeDiscriminants.get(sd);
                    if (existing == null) {
                        ScopeDiscriminant newSD =
                                new ScopeDiscriminant("sc" + scopeDiscriminantIndex, sd, new HashSet<>(), scope2instantiations.getOrDefault(sd, new LinkedHashSet<>()));
                        newSD.solutionIds.add("s" + solutionIndex);
                        scopeDiscriminants.put(sd, newSD);
                        scopeDiscriminantIndex++;
                    } else {
                        existing.solutionIds.add("s" + solutionIndex);
                        existing.instantiations.addAll(scope2instantiations.get(sd));
                    }
                }

                currentSO.solutionString = currentSolution;
                currentSO.solutionId = "s" + solutionIndex;

                solutions.add(currentSolution);
                solutionIndexToObject.put(solutionIndex, currentSO);
                solutionIndex++;
            }
        }

        List<ScopeDiscriminant> finalScopeDiscriminants =
                finalizeScopeDiscriminants(scopeDiscriminants, solutions.size());

        return new SolutionsAndDiscriminants(solutions, solutionIndexToObject, finalScopeDiscriminants);
    }

    private String buildSolutionString(Settings settings, Integer key, int i, SolutionObject so) {
        StringBuilder solutionBuilder = new StringBuilder();
        if (settings.getSemanticOutputStyle() == Settings.LFGXDRT) {
            solutionBuilder.append(so.solution.getSemTerm().toString());
        } else if (settings.getSemanticOutputStyle() == 1) {
            solutionBuilder.append("solution").append("(").append(key).append(i).append(",");
            solutionBuilder.append(so.solution.getSemTerm().toString());
            solutionBuilder.append(").");
        } else if (settings.getSemanticOutputStyle() == 0) {
            solutionBuilder.append(key).append(i).append(": ").append(so.solution.getSemTerm().toString());
        }
        return solutionBuilder.toString();
    }

    private List<ScopeDiscriminant> finalizeScopeDiscriminants(
            Map<String, ScopeDiscriminant> scopeDiscriminants,
            int solutionSize
    ) {
        scopeDiscriminants.entrySet().removeIf(e -> e.getValue().solutionIds.size() == solutionSize);

        List<ScopeDiscriminant> initial = scopeDiscriminants.values().stream()
                .sorted(Comparator.comparingDouble((ScopeDiscriminant v) ->
                        entropy(v.solutionIds.size(), solutionSize)))
                .toList();

        Map<Set<String>, ScopeDiscriminant> unique = new LinkedHashMap<>();
        for (ScopeDiscriminant d : initial) {
            Set<String> key = Set.copyOf(d.solutionIds);
            unique.putIfAbsent(key, d);
        }

        List<ScopeDiscriminant> finals = new ArrayList<>(unique.values());
        Collections.reverse(finals);
        return finals;
    }

    private List<GswbDiscriminant> toOutputDiscriminants(
            List<ScopeDiscriminant> scopeDiscriminants,
            List<McDiscriminant> mcDiscriminants
    ) {
        List<GswbDiscriminant> out = new ArrayList<>();
        for (ScopeDiscriminant d : scopeDiscriminants) {
            out.add(new GswbDiscriminant(d.discriminantID, "scope", d.scopeConstraint, d.solutionIds, d.instantiations));
        }
        for (McDiscriminant mc : mcDiscriminants) {
            out.add(new GswbDiscriminant(mc.discriminantID, "MCs", mc.meaningConstructor, mc.associatedSolutions));
        }
        return out;
    }

    private static double entropy(int k, int n) {
        if (n <= 0) return 0.0;
        if (k <= 0 || k >= n) return 0.0;
        double p = (double) k / (double) n;
        return -(p * log2(p) + (1.0 - p) * log2(1.0 - p));
    }

    private static double log2(double x) {
        return Math.log(x) / Math.log(2.0);
    }
}
