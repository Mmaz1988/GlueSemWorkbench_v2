package webservice.rest;

import Discriminants.ScopeDiscriminant;
import de.ukon.lfgxdrt.SemanticExpression;
import de.ukon.lfgxdrt.DrsParser;
import de.ukon.lfgxdrt.DrsGraphParser;
import de.ukon.lfgxdrt.DrsSequenceMerger;
import de.ukon.lfgxdrt.DrsReasoningCheckBuilder;
import de.ukon.lfgxdrt.ReasoningCheckType;
import de.ukon.lfgxdrt.drs_elements.DRS;
import de.ukon.lfgxdrt.drs_elements.AnaphoraMapping;
import de.ukon.lfgxdrt.drs_elements.AnaphoraRelation;
import de.ukon.lfgxdrt.drs_elements.DiscourseReferent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import Discriminants.McDiscriminant;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import de.ukon.lfgxdrt.DrsSvgRenderer;
import main.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;
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
    private final RestTemplate restTemplate = new RestTemplate();
    private final String ligerApiUrl = System.getenv().getOrDefault("LIGER_API_URL", "http://localhost:8080");

    public GswbController(){}

    @CrossOrigin
    @PostMapping(value = "/gswb_batch_proof", produces = "application/json", consumes = "application/json")
    public GswbBatchOutput glueBatchDeduce(@RequestBody GswbBatchRequest request) throws Exception {

        RunContext ctx = buildRunContext(request.gswbPreferences);
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
                            false, // includeDerivation
                            null
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
        LOGGER.info("Received /deduce request: proofs="
                + (request.proofs == null ? 0 : request.proofs.size())
                + ", premisesLength=" + (request.premises == null ? 0 : request.premises.length())
                + ", hasStructure=" + (request.structure != null));

        GlueParser gp = new GlueParser(ctx.settings);

        SingleRunResult run;
        if (request.proofs != null && !request.proofs.isEmpty()) {
            ParsedProofInputs parsed = parseProofInputs(request.proofs, gp, ctx.multistage);
            LOGGER.info("Parsed structured proof inputs: records=" + request.proofs.size()
                    + ", mcSets=" + parsed.entries.lexicalEntries.size()
                    + ", origins=" + parsed.origins.size());
            run = runAndFormatSingle(
                    parsed.entries,
                    parsed.origins,
                    ctx,
                    false, // single mode
                    true,  // includeDerivation
                    request.structure
            );
        } else {
            InputOutputProcessor.process(request.premises);
            String input = InputOutputProcessor.translate(request.premises);
            run = runAndFormatSingle(
                    input,
                    ctx,
                    gp,
                    false, // single mode
                    true,  // includeDerivation
                    request.structure
            );
        }

        LOGGER.info("Completed /deduce request: solutions=" + run.output.solutions.size()
                + ", discriminants=" + run.output.discriminants.size());
        return run.output;
    }

    @CrossOrigin
    @PostMapping(value = "/generate_pcdrs", produces = "application/json", consumes = "application/json")
    public GswbPcdrsOutput generatePcdrs(@RequestBody GswbPcdrsRequest request) throws Exception {
        if (request == null || (!hasCanonicalGraph(request.mergedStructure)
                && (request.semantic == null || request.semantic.isBlank()))) {
            throw new IllegalArgumentException("A semantic DRS or canonical graph is required to generate PCDRS solutions.");
        }

        LOGGER.info("PCDRS request received: parentSolutionId=" + request.parentSolutionId
                + ", semantic=" + request.semantic
                + ", mergedStructureKeys="
                + (request.mergedStructure == null ? "[]" : request.mergedStructure.keySet()));

        SemanticExpression expression = hasCanonicalGraph(request.mergedStructure)
                ? DrsGraphParser.parse(request.mergedStructure)
                : new DrsParser().parse(request.semantic).expression;
        if (!(expression instanceof DRS)) {
            LOGGER.info("PCDRS graph expression is unresolved: " + expression.getClass().getSimpleName()
                    + "; resolving merges before generating PCDRS");
            expression = expression.resolveMerges();
        }
        if (!(expression instanceof DRS drs)) {
            throw new IllegalArgumentException("PCDRS generation requires a DRS semantic expression.");
        }

        LinkedHashMap<String, String> nodeNames = extractNodeNames(request.mergedStructure);
        LinkedHashMap<String, LinkedHashSet<String>> candidates = extractPossibleAntFacts(request.mergedStructure, nodeNames);
        LOGGER.info("PCDRS semantic node names: " + nodeNames);
        List<LinkedHashMap<String, String>> mappings = expandAnaphoraMappings(candidates);
        LOGGER.info("PCDRS possible-ant candidates: " + candidates);
        if (mappings.isEmpty()) {
            mappings = List.of(new LinkedHashMap<>());
        }
        LOGGER.info("PCDRS mapping branches: " + mappings.size());

        String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                ? "solution"
                : request.parentSolutionId;
        List<GswbSolution> solutions = new ArrayList<>();
        for (int i = 0; i < mappings.size(); i++) {
            AnaphoraMapping mapping = toAnaphoraMapping(mappings.get(i));
            LOGGER.info("PCDRS branch " + (i + 1) + ": mapping=" + mapping.toString());
            DRS pcdrs = drs.withAnaphoraMapping(mapping);
            LOGGER.info("PCDRS branch " + (i + 1) + ": combined DRS=" + pcdrs.toString());
            GswbSolution solution = new GswbSolution(
                    new DrsSvgRenderer().toSvg(pcdrs),
                    parentId + "-pcdrs-" + (i + 1),
                    drs.getSourceIndex(),
                    null,
                    pcdrs.toString());
            solution.anaphoraMapping = mapping.toString();
            solution.anaphoraRelations = AnaphoraMappingConverter.toDto(mapping);
            solutions.add(solution);
        }

        LOGGER.info("PCDRS rendering complete: solutions=" + solutions.size());

        return new GswbPcdrsOutput(request.parentSolutionId, solutions);
    }

    @CrossOrigin
    @PostMapping(value = "/collapse_anaphora", produces = "application/json", consumes = "application/json")
    public GswbSolution collapseAnaphora(@RequestBody GswbCollapseAnaphoraRequest request) throws Exception {
        if (request == null || request.semantic == null || request.semantic.isBlank()) {
            throw new IllegalArgumentException("A PCDRS semantic is required to collapse anaphora.");
        }

        LOGGER.info("Collapse anaphora request received: parentSolutionId=" + request.parentSolutionId
                + ", semantic=" + request.semantic);
        SemanticExpression expression = new DrsParser().parse(request.semantic).expression;
        if (!(expression instanceof DRS drs)) {
            throw new IllegalArgumentException("Anaphora collapse requires a DRS semantic expression.");
        }

        DRS collapsed = drs.collapseAnaphora();
        String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                ? "solution"
                : request.parentSolutionId;
        GswbSolution result = new GswbSolution(
                new DrsSvgRenderer().toSvg(collapsed),
                parentId + "-collapsed",
                collapsed.getSourceIndex(),
                null,
                collapsed.toString());
        // The mapping being resolved lives on the pre-collapse DRS -- collapseAnaphora()
        // folds it into the DRS's own conditions, so `collapsed.anaphoraMapping` is typically
        // empty by the time we get here.
        if (drs.anaphoraMapping != null) {
            result.anaphoraMapping = drs.anaphoraMapping.toString();
            result.anaphoraRelations = AnaphoraMappingConverter.toDto(drs.anaphoraMapping);
        }
        LOGGER.info("Anaphora collapse complete: solutionId=" + result.id
                + ", semantic=" + result.semantic);
        return result;
    }

    @CrossOrigin
    @PostMapping(value = "/merge_sequence_semantics", produces = "application/json", consumes = "application/json")
    public GswbSolution mergeSequenceSemantics(@RequestBody GswbSequenceMergeRequest request) throws Exception {
        if (request != null && request.parts != null && !request.parts.isEmpty()) {
            List<SemanticExpression> expressions = new ArrayList<>();
            for (GswbSemanticMergePart part : request.parts) {
                if (part == null || (part.graph == null && (part.semantic == null || part.semantic.isBlank()))) {
                    throw new IllegalArgumentException("Every sequence part requires a semantic graph or semantic text");
                }
                SemanticExpression expression = part.graph != null
                        ? DrsGraphParser.parse(part.graph)
                        : new DrsParser().parse(part.semantic).expression;
                expressions.add(expression);
                LOGGER.info("Sequence part reconstructed: source=" + expression.getSourceIndex()
                        + ", representation=" + (part.graph != null ? "graph" : "semantic")
                        + ", expression=" + expression);
            }

            SemanticExpression merged = DrsSequenceMerger.merge(expressions);
            SemanticExpression resolvedExpression = merged.resolveMerges();
            if (!(resolvedExpression instanceof DRS resolved)) {
                throw new IllegalStateException("Sequence merge did not resolve to a DRS");
            }
            SemanticExpression displayExpression = mergeForDisplay(expressions, request.resolveDrs);
            LOGGER.info("Sequence display expression: resolveEach=" + request.resolveDrs
                    + ", expression=" + displayExpression);

            String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                    ? "sequence" : request.parentSolutionId;
            GswbSemanticMergePart lastPart = request.parts.get(request.parts.size() - 1);
            GswbSolution output = new GswbSolution(
                    new DrsSvgRenderer().toSvg(displayExpression),
                    compositeSemanticId(request.parts, parentId),
                    merged.getSourceIndex(),
                    resolved.toJson(),
                    displayExpression.toString());
            output.solutionKey = request.solutionKey != null
                    ? request.solutionKey : lastPart.syntacticOrigin;
            output.mcSetId = request.mcSetId != null ? request.mcSetId : lastPart.mcSetId;
            output.proofId = lastPart.proofId;
            output.semanticAnalysis = semanticAnalysis(output, output.solutionKey);
            output.synSemMapping.put(compositeSyntaxId(request.parts), List.of(output.id));
            return output;
        }
        if (request == null || request.graphs == null || request.graphs.isEmpty()
                || request.graphs.stream().anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("At least one canonical semantic graph is required");
        }

        LOGGER.info("Sequence merge received: graphs=" + request.graphs.size()
                + ", semantics=" + (request.semantics == null ? 0 : request.semantics.size()));
        List<SemanticExpression> expressions = new ArrayList<>();
        for (LinkedHashMap<String, Object> graph : request.graphs) {
            SemanticExpression expression = DrsGraphParser.parse(graph);
            expressions.add(expression);
            LOGGER.info("Sequence graph reconstructed: source=" + expression.getSourceIndex()
                    + ", expression=" + expression.toString());
        }
        SemanticExpression merged = DrsSequenceMerger.merge(expressions);
        SemanticExpression resolvedExpression = merged.resolveMerges();
        if (!(resolvedExpression instanceof DRS resolved)) {
            throw new IllegalStateException("Sequence merge did not resolve to a DRS");
        }

        SemanticExpression displayExpression = mergeForDisplay(expressions, request.resolveDrs);
        if (request.semantics != null && request.semantics.size() == request.graphs.size()
                && request.semantics.stream().allMatch(value -> value != null && !value.isBlank())) {
            List<SemanticExpression> displayExpressions = new ArrayList<>();
            for (String semantic : request.semantics) {
                displayExpressions.add(new DrsParser().parse(semantic).expression);
            }
            displayExpression = mergeForDisplay(displayExpressions, request.resolveDrs);
            LOGGER.info("Sequence display expression reconstructed from semantic strings: "
                    + displayExpression.toString());
        }
        LOGGER.info("Sequence graph merge resolved: source=" + resolved.getSourceIndex()
                + ", graphNodes=" + graphNodeCount(resolved.toJson())
                + ", graphEdges=" + graphEdgeCount(resolved.toJson()));

        String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                ? "sequence" : request.parentSolutionId;
        GswbSolution output = new GswbSolution(
                new DrsSvgRenderer().toSvg(displayExpression),
                parentId + "-drs-merge",
                merged.getSourceIndex(),
                resolved.toJson(),
                displayExpression.toString());
        output.solutionKey = request.solutionKey;
        output.mcSetId = request.mcSetId;
        output.semanticAnalysis = semanticAnalysis(output, output.solutionKey);
        if (request.solutionKey != null && !request.solutionKey.isBlank()) {
            output.synSemMapping.put(request.solutionKey, List.of(output.id));
        }
        return output;
    }

    private SemanticExpression mergeForDisplay(List<SemanticExpression> expressions, boolean resolveEach) {
        List<SemanticExpression> displayParts = resolveEach
                ? expressions.stream().map(SemanticExpression::resolveMerges).toList()
                : expressions;
        return DrsSequenceMerger.merge(displayParts);
    }

    private String compositeSyntaxId(List<GswbSemanticMergePart> parts) {
        return parts.stream()
                .map(part -> part.syntacticOrigin == null || part.syntacticOrigin.isBlank()
                        ? part.id : part.syntacticOrigin)
                .filter(Objects::nonNull)
                .collect(Collectors.joining("+"));
    }

    /**
     * Semantic identity is derived from the ordered parent semantic IDs.
     * The parentSolutionId fallback exists only for the legacy graphs-only
     * request, which does not transport semantic identities.
     */
    private String compositeSemanticId(List<GswbSemanticMergePart> parts, String legacyParentId) {
        List<String> parentIds = parts.stream()
                .map(part -> part.id == null || part.id.isBlank() ? part.solutionId : part.id)
                .toList();
        if (parentIds.stream().allMatch(id -> id != null && !id.isBlank())) {
            return String.join("+", parentIds);
        }
        return legacyParentId + "-drs-merge";
    }

    private GswbSemanticAnalysis semanticAnalysis(GswbSolution solution, String syntacticOrigin) {
        String semId = solution.id == null ? "semantic" : solution.id;
        return new GswbSemanticAnalysis(
                syntacticOrigin,
                semId,
                solution.semantic,
                null,
                solution.graph,
                "lfgxdrt");
    }

    @CrossOrigin
    @PostMapping(value = "/semantic_to_tptp", produces = "application/json", consumes = "application/json")
    public GswbTptpOutput semanticToTptp(@RequestBody GswbTptpRequest request) throws Exception {
        if (request == null || request.semantic == null || request.semantic.isBlank()) {
            throw new IllegalArgumentException("A resolved semantic representation is required");
        }
        SemanticExpression expression = new DrsParser().parse(request.semantic).expression;
        if (!(expression instanceof DRS drs)) {
            throw new IllegalArgumentException("TPTP translation requires a resolved DRS");
        }
        return new GswbTptpOutput(drs.toTPTPString(request.typed));
    }

    @CrossOrigin
    @PostMapping(value = "/reasoning_check_asts", produces = "application/json", consumes = "application/json")
    public GswbReasoningCheckAstsOutput reasoningCheckAsts(@RequestBody GswbReasoningCheckAstsRequest request) throws Exception {
        if (request == null || request.premiseAsts == null || request.premiseAsts.isEmpty()
                || request.hypothesisAsts == null || request.hypothesisAsts.isEmpty()) {
            throw new IllegalArgumentException("Premise and hypothesis ASTs are required");
        }

        List<SemanticExpression> premiseExpressions = request.premiseAsts.stream()
                .map(part -> {
                    try {
                        return DrsGraphParser.parse(part);
                    } catch (Exception e) {
                        throw new IllegalArgumentException("Could not parse premise AST", e);
                    }
                }).toList();
        List<SemanticExpression> hypothesisExpressions = request.hypothesisAsts.stream()
                .map(part -> {
                    try {
                        return DrsGraphParser.parse(part);
                    } catch (Exception e) {
                        throw new IllegalArgumentException("Could not parse hypothesis AST", e);
                    }
                }).toList();

        SemanticExpression premiseExpression = DrsSequenceMerger.merge(premiseExpressions).resolveMerges();
        SemanticExpression hypothesisExpression = DrsSequenceMerger.merge(hypothesisExpressions).resolveMerges();
        if (!(premiseExpression instanceof DRS premise) || !(hypothesisExpression instanceof DRS hypothesis)) {
            throw new IllegalStateException("Reasoning inputs must resolve to DRS boxes");
        }

        LinkedHashMap<String, GswbReasoningCheckAst> output = new LinkedHashMap<>();
        for (Map.Entry<ReasoningCheckType, SemanticExpression> entry :
                new DrsReasoningCheckBuilder().buildAsts(premise, hypothesis).entrySet()) {
            // Resolve only the sequence merge wrappers. Keep the resulting
            // check AST, including its operators and source indexes, for the
            // provenance-sensitive post-processing stage.
            SemanticExpression ast = entry.getValue().resolveMerges();
            output.put(entry.getKey().name().toLowerCase(),
                    new GswbReasoningCheckAst(ast.toString(), ast.toJson()));
        }
        return new GswbReasoningCheckAstsOutput(output);
    }

    @CrossOrigin
    @PostMapping(value = "/reasoning_checks", produces = "application/json", consumes = "application/json")
    public GswbReasoningChecksOutput reasoningChecks(@RequestBody GswbReasoningChecksRequest request) throws Exception {
        if (request == null || request.premiseParts == null || request.premiseParts.isEmpty()
                || request.hypothesisParts == null || request.hypothesisParts.isEmpty()) {
            throw new IllegalArgumentException("Premise and hypothesis semantic parts are required");
        }
        List<SemanticExpression> premiseExpressions = new ArrayList<>();
        for (String part : request.premiseParts) {
            premiseExpressions.add(new DrsParser().parse(part).expression);
        }
        List<SemanticExpression> hypothesisExpressions = new ArrayList<>();
        for (String part : request.hypothesisParts) {
            hypothesisExpressions.add(new DrsParser().parse(part).expression);
        }
        SemanticExpression premiseMerged = DrsSequenceMerger.merge(premiseExpressions).resolveMerges();
        SemanticExpression hypothesisMerged = DrsSequenceMerger.merge(hypothesisExpressions).resolveMerges();
        if (!(premiseMerged instanceof DRS premise) || !(hypothesisMerged instanceof DRS hypothesis)) {
            throw new IllegalStateException("Reasoning inputs must resolve to DRS boxes");
        }

        Map<String, GswbReasoningCheck> output = new LinkedHashMap<>();
        Map<ReasoningCheckType, DrsReasoningCheckBuilder.CheckResult> checks =
                new DrsReasoningCheckBuilder().build(premise, hypothesis, request.typed);
        for (ReasoningCheckType type : ReasoningCheckType.values()) {
            DrsReasoningCheckBuilder.CheckResult check = checks.get(type);
            output.put(type.name().toLowerCase(), new GswbReasoningCheck(check.tptp()));
        }
        return new GswbReasoningChecksOutput(output);
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

    private static final class ParsedProofInputs {
        private final LexicalEntries entries;
        private final Map<Integer, GswbProofInput> origins;

        private ParsedProofInputs(LexicalEntries entries, Map<Integer, GswbProofInput> origins) {
            this.entries = entries;
            this.origins = origins;
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
        // LFGxDRT AST graphs are required by the provenance-sensitive NLI
        // post-processing path, so this mode must always expose beta-reduced
        // semantic graphs even when the UI leaves the display toggle off.
        boolean betaReduce = displayLfgxDrt || prefs.betaReduce || prefs.resolveDrs;
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
            boolean includeDerivation,
            LinkedHashMap<String, Object> structure
    ) throws Exception {
        LexicalEntries mcs = gp.parseMeaningConstructorString(premiseInput, ctx.multistage);
        return runAndFormatSingle(mcs, Collections.emptyMap(), ctx, batchMode, includeDerivation, structure);
    }

    private SingleRunResult runAndFormatSingle(
            LexicalEntries mcs,
            Map<Integer, GswbProofInput> origins,
            RunContext ctx,
            boolean batchMode,
            boolean includeDerivation,
            LinkedHashMap<String, Object> structure
    ) throws Exception {

        LLProverAndLog proverAndLog = createProver(ctx.settings);
        LLProver prover = proverAndLog.prover;
        StringBuilder sb = proverAndLog.sb;

        LOGGER.info("Running prover...");

        ProofRun run = runProofsOverLexicalEntries(mcs, prover, sb, batchMode, origins);

        LexicalEntries filteredMcs = filterLexicalEntriesByKeySet(mcs, run.mcSetWithSolution);
        List<McDiscriminant> finalMcDiscriminants = filteredMcs.calculateDiscriminants();

        LOGGER.info("Formatting output...");

        SolutionsAndDiscriminants formatted =
                formatSolutionsAndDiscriminants(run.allSolutions, finalMcDiscriminants, prover.scope2instantiations,
                        prover.scope2InstantiationsByOrigin,
                        prover.scope2SourceIndexGroups, prover.scope2SourceIndexGroupsByOrigin,
                        structure, ctx.settings, origins);

        applyOptionalSemanticRendering(ctx, formatted);

        List<GswbSolution> outputSolutions = toOutputSolutions(formatted.solutionIndexToObject);

        Object derivation = null;
        if (includeDerivation) {
            derivation = buildDerivationIfEnabled(ctx.settings, prover);
        }

        List<GswbDiscriminant> outputDiscriminants =
                toOutputDiscriminants(formatted.finalScopeDiscriminants, finalMcDiscriminants, origins);

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

    private ParsedProofInputs parseProofInputs(
            List<GswbProofInput> proofInputs,
            GlueParser gp,
            boolean multistage
    ) throws Exception {
        LinkedHashMap<Integer, List<MeaningConstructor>> combined = new LinkedHashMap<>();
        Map<Integer, GswbProofInput> origins = new LinkedHashMap<>();
        int nextKey = 1;

        for (GswbProofInput proofInput : proofInputs) {
            if (proofInput == null || proofInput.meaningConstructors == null
                    || proofInput.meaningConstructors.isBlank()) {
                continue;
            }

            InputOutputProcessor.process(proofInput.meaningConstructors);
            String translated = InputOutputProcessor.translate(proofInput.meaningConstructors);
            LexicalEntries local = gp.parseMeaningConstructorString(translated, multistage);
            for (Map.Entry<Integer, List<MeaningConstructor>> localEntry : local.lexicalEntries.entrySet()) {
                GswbProofInput origin = copyProofInput(proofInput);
                String baseMcSetId = proofInput.mcSetId == null || proofInput.mcSetId.isBlank()
                        ? proofInput.proofId : proofInput.mcSetId;
                origin.mcSetId = baseMcSetId + ":" + localEntry.getKey();
                combined.put(nextKey, localEntry.getValue());
                origins.put(nextKey, origin);
                nextKey++;
            }
        }

        return new ParsedProofInputs(new LexicalEntries(combined), origins);
    }

    private GswbProofInput copyProofInput(GswbProofInput input) {
        GswbProofInput copy = new GswbProofInput();
        copy.proofId = input.proofId;
        copy.solutionKey = input.solutionKey;
        copy.mcSetId = input.mcSetId;
        copy.meaningConstructors = input.meaningConstructors;
        copy.structure = input.structure;
        return copy;
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
            boolean batchMode,
            Map<Integer, GswbProofInput> origins
    ) {
        int noOfMCs = 0;
        int countSolutions = 0;

        LinkedHashMap<Integer, List<SolutionObject>> allSolutions = new LinkedHashMap<>();
        HashSet<Integer> mcSetWithSolution = new HashSet<>();

        prover.scope2SourceIndexGroupsByOrigin.clear();
        prover.scope2InstantiationsByOrigin.clear();

        String log = "";

        for (Integer key : mcs.lexicalEntries.keySet()) {
            try {
                GswbProofInput origin = origins.get(key);
                prover.currentProofOrigin = origin == null ? null : originTag(origin);
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
                prover.currentProofOrigin = null;

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
                // Source provenance belongs to graph JSON; keep the public semantic text clean.
                so.semantic = sol.toString();

                // Keep display text/SVG independent from the graph payload.
                // The graph must remain available for source provenance even
                // when the display preference leaves DRS merges unresolved.
                so.graph = semanticGraphFor(sol);
                if (so.graph == null) {
                    LOGGER.warning("Could not render provenance graph for solution " + idx);
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
            GswbSolution output = new GswbSolution(so.solutionString, so.solutionId, so.sourceIndex, so.graph,
                    so.semantic);
            output.proofId = so.proofId;
            output.solutionKey = so.solutionKey;
            output.mcSetId = so.mcSetId;
            output.semanticAnalysis = semanticAnalysis(output, so.solutionKey);
            if (so.solutionKey != null && !so.solutionKey.isBlank()) {
                output.synSemMapping.put(so.solutionKey, List.of(output.id));
            }
            outputSolutions.add(output);
        }
        return outputSolutions;
    }

    private LinkedHashMap<String, Object> semanticGraphFor(SemanticExpression expression) {
        try {
            return expression.toJson();
        } catch (Exception ignored) {
            try {
                return expression.betaReduce().resolveMerges().toJson();
            } catch (Exception e) {
                LOGGER.warning("Could not derive a provenance graph: " + e.getMessage());
                return null;
            }
        }
    }

    private boolean hasCanonicalGraph(LinkedHashMap<String, Object> structure) {
        return structure != null && structure.get("nodes") instanceof List<?>
                && structure.get("edges") instanceof List<?>;
    }

    private int graphNodeCount(LinkedHashMap<String, Object> graph) {
        return graph.get("nodes") instanceof List<?> nodes ? nodes.size() : 0;
    }

    private int graphEdgeCount(LinkedHashMap<String, Object> graph) {
        return graph.get("edges") instanceof List<?> edges ? edges.size() : 0;
    }

    private LinkedHashMap<String, String> extractNodeNames(Map<String, Object> structure) {
        LinkedHashMap<String, String> nodeNames = new LinkedHashMap<>();
        Object nodes = structure == null ? null : structure.get("nodes");
        if (nodes instanceof Iterable<?> nodeList) {
            for (Object node : nodeList) {
                if (!(node instanceof Map<?, ?> nodeMap) || nodeMap.get("id") == null) {
                    continue;
                }
                if (nodeMap.get("avp") instanceof Map<?, ?> avp && avp.get("NAME") != null) {
                    nodeNames.put(String.valueOf(nodeMap.get("id")).trim(),
                            String.valueOf(avp.get("NAME")).trim());
                }
            }
        }
        collectNodeNames(structure, nodeNames);
        return nodeNames;
    }

    private void collectNodeNames(Object value, LinkedHashMap<String, String> nodeNames) {
        if (value instanceof Map<?, ?> map) {
            if ("NAME".equals(String.valueOf(map.get("relationLabel")))) {
                Object source = map.containsKey("fsNode") ? map.get("fsNode") : map.get("sourceNode");
                Object target = map.containsKey("fsValue") ? map.get("fsValue") : map.get("targetNode");
                if (source != null && target != null) {
                    nodeNames.put(String.valueOf(source).trim(), String.valueOf(target).trim());
                }
            }
            for (Object child : map.values()) {
                collectNodeNames(child, nodeNames);
            }
        } else if (value instanceof Iterable<?> iterable) {
            for (Object child : iterable) {
                collectNodeNames(child, nodeNames);
            }
        }
    }

    private LinkedHashMap<String, LinkedHashSet<String>> extractPossibleAntFacts(
            Map<String, Object> structure, Map<String, String> nodeNames) {
        LinkedHashMap<String, LinkedHashSet<String>> candidates = new LinkedHashMap<>();
        collectPossibleAntFacts(structure, nodeNames, candidates);
        return candidates;
    }

    private void collectPossibleAntFacts(Object value,
                                         Map<String, String> nodeNames,
                                         LinkedHashMap<String, LinkedHashSet<String>> candidates) {
        if (value instanceof Map<?, ?> map) {
            Object relation = map.get("relationLabel");
            Object source = map.containsKey("fsNode") ? map.get("fsNode") : map.get("sourceNode");
            Object target = map.containsKey("fsValue") ? map.get("fsValue") : map.get("targetNode");
            if ("POSSIBLE-ANT".equals(String.valueOf(relation)) && source != null && target != null) {
                String sourceId = String.valueOf(source).trim();
                String targetId = String.valueOf(target).trim();
                String sourceName = nodeNames.get(sourceId);
                String targetName = nodeNames.get(targetId);
                if (sourceName == null || targetName == null) {
                    LOGGER.warning("Ignoring POSSIBLE-ANT without NAME features: source="
                            + sourceId + ", target=" + targetId);
                } else if (!sourceName.isEmpty() && !targetName.isEmpty()) {
                    candidates.computeIfAbsent(sourceName, ignored -> new LinkedHashSet<>()).add(targetName);
                }
            }
            for (Object child : map.values()) {
                collectPossibleAntFacts(child, nodeNames, candidates);
            }
        } else if (value instanceof Iterable<?> iterable) {
            for (Object child : iterable) {
                collectPossibleAntFacts(child, nodeNames, candidates);
            }
        }
    }

    private List<LinkedHashMap<String, String>> expandAnaphoraMappings(
            LinkedHashMap<String, LinkedHashSet<String>> candidates) {
        List<LinkedHashMap<String, String>> results = new ArrayList<>();
        results.add(new LinkedHashMap<>());
        for (Map.Entry<String, LinkedHashSet<String>> entry : candidates.entrySet()) {
            List<LinkedHashMap<String, String>> next = new ArrayList<>();
            for (LinkedHashMap<String, String> partial : results) {
                for (String antecedent : entry.getValue()) {
                    LinkedHashMap<String, String> expanded = new LinkedHashMap<>(partial);
                    expanded.put(entry.getKey(), antecedent);
                    next.add(expanded);
                }
            }
            results = next;
        }
        return results;
    }

    private AnaphoraMapping toAnaphoraMapping(Map<String, String> mapping) {
        AnaphoraMapping result = new AnaphoraMapping();
        for (Map.Entry<String, String> entry : mapping.entrySet()) {
            result.addRelation(new AnaphoraRelation(
                    new DiscourseReferent(entry.getKey()), entry.getValue()));
        }
        return result;
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
            LinkedHashMap<String, LinkedHashMap<String, LinkedHashSet<String>>> scope2InstantiationsByOrigin,
            LinkedHashMap<String, List<LinkedHashSet<Integer>>> scope2SourceIndexGroups,
            LinkedHashMap<String, LinkedHashMap<String, List<LinkedHashSet<Integer>>>> scope2SourceIndexGroupsByOrigin,
            LinkedHashMap<String, Object> structure,
            Settings settings,
            Map<Integer, GswbProofInput> origins
    ) {
        Map<Integer, SolutionObject> solutionIndexToObject = new HashMap<>();
        List<String> solutions = new ArrayList<>();

        HashMap<String, ScopeDiscriminant> scopeDiscriminants = new HashMap<>();

        int solutionIndex = 0;
        int scopeDiscriminantIndex = 0;

        for (Integer key : allSolutions.keySet()) {
            for (int i = 0; i < allSolutions.get(key).size(); i++) {

                SolutionObject currentSO = allSolutions.get(key).get(i);
                GswbProofInput origin = origins.get(key);
                if (origin != null) {
                    currentSO.proofId = origin.proofId;
                    currentSO.solutionKey = origin.solutionKey;
                    currentSO.mcSetId = origin.mcSetId;
                }

                String currentSolution = buildSolutionString(settings, key, i, currentSO).trim();

                for (McDiscriminant d : finalMcDiscriminants) {
                    if (d.mcSetIds.contains(key)) {
                        d.associatedSolutions.add("s" + solutionIndex);
                    }
                }

                for (String sd : currentSO.scopeDiscriminants) {
                    ScopeDiscriminant existing = scopeDiscriminants.get(sd);
                    if (existing == null) {
                        LinkedHashSet<String> instantiations = instantiationsFor(sd, origin,
                                scope2instantiations, scope2InstantiationsByOrigin);
                        ScopeDiscriminant newSD =
                        new ScopeDiscriminant("sc" + scopeDiscriminantIndex, sd, new HashSet<>(), instantiations);
                        newSD.solutionIds.add("s" + solutionIndex);
                        addOrigin(newSD, origin);
                        scopeDiscriminants.put(sd, newSD);
                        scopeDiscriminantIndex++;
                    } else {
                        existing.solutionIds.add("s" + solutionIndex);
                        addOrigin(existing, origin);
                        existing.instantiations.addAll(instantiationsFor(sd, origin,
                                scope2instantiations, scope2InstantiationsByOrigin));
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
        reconcileDiscriminantOrigins(finalScopeDiscriminants, solutionIndexToObject,
                scope2InstantiationsByOrigin, !origins.isEmpty());

        enrichScopeSurfaceLabels(finalScopeDiscriminants, scope2SourceIndexGroups,
                        scope2SourceIndexGroupsByOrigin, structure, origins);

        return new SolutionsAndDiscriminants(solutions, solutionIndexToObject, finalScopeDiscriminants);
    }

    private void reconcileDiscriminantOrigins(
            List<ScopeDiscriminant> discriminants,
            Map<Integer, SolutionObject> solutionsByIndex,
            LinkedHashMap<String, LinkedHashMap<String, LinkedHashSet<String>>> instantiationsByOrigin,
            boolean hasStructuredOrigins
    ) {
        if (!hasStructuredOrigins) {
            return;
        }
        for (ScopeDiscriminant discriminant : discriminants) {
            discriminant.originIds.clear();
            discriminant.instantiations.clear();
            for (String solutionId : discriminant.solutionIds) {
                SolutionObject solution = solutionsByIndex.values().stream()
                        .filter(candidate -> solutionId.equals(candidate.solutionId))
                        .findFirst()
                        .orElse(null);
                String originId = solution == null
                        ? null
                        : (solution.mcSetId == null || solution.mcSetId.isBlank()
                        ? solution.proofId : solution.mcSetId);
                if (originId == null || originId.isBlank()) {
                    continue;
                }
                discriminant.originIds.add(originId);
                LinkedHashMap<String, LinkedHashSet<String>> values = instantiationsByOrigin.get(originId);
                if (values != null) {
                    discriminant.instantiations.addAll(values.getOrDefault(
                            discriminant.scopeConstraint, new LinkedHashSet<>()));
                }
            }
        }
    }

    private LinkedHashSet<String> instantiationsFor(
            String scope,
            GswbProofInput origin,
            LinkedHashMap<String, LinkedHashSet<String>> allInstantiations,
            LinkedHashMap<String, LinkedHashMap<String, LinkedHashSet<String>>> instantiationsByOrigin
    ) {
        String originId = originTag(origin);
        if (originId != null) {
            LinkedHashMap<String, LinkedHashSet<String>> originValues = instantiationsByOrigin.get(originId);
            if (originValues != null && originValues.containsKey(scope)) {
                return new LinkedHashSet<>(originValues.get(scope));
            }
        }
        return new LinkedHashSet<>(allInstantiations.getOrDefault(scope, new LinkedHashSet<>()));
    }

    private void enrichScopeSurfaceLabels(List<ScopeDiscriminant> discriminants,
                                          LinkedHashMap<String, List<LinkedHashSet<Integer>>> groupsByScope,
                                          LinkedHashMap<String, LinkedHashMap<String, List<LinkedHashSet<Integer>>>> groupsByOrigin,
                                          LinkedHashMap<String, Object> structure,
                                          Map<Integer, GswbProofInput> origins) {
        if (discriminants.isEmpty()) {
            return;
        }

        for (ScopeDiscriminant discriminant : discriminants) {
            boolean hasGlobalGroups = groupsByScope.containsKey(discriminant.scopeConstraint);
            boolean hasOriginGroups = groupsByOrigin.values().stream()
                    .anyMatch(groups -> groups.containsKey(discriminant.scopeConstraint));
            if (!hasGlobalGroups && !hasOriginGroups) {
                continue;
            }
            if (discriminant.originIds.isEmpty()) {
                List<LinkedHashSet<Integer>> sourceGroups = groupsByScope.get(discriminant.scopeConstraint);
                if (sourceGroups == null) {
                    sourceGroups = groupsByOrigin.values().stream()
                            .map(groups -> groups.get(discriminant.scopeConstraint))
                            .filter(Objects::nonNull)
                            .findFirst()
                            .orElse(null);
                }
                String label = resolveSurfaceLabel(discriminant, sourceGroups, structure);
                if (label != null) {
                    discriminant.surfaceLabel = label;
                }
                continue;
            }
            for (String originId : discriminant.originIds) {
                LinkedHashMap<String, List<LinkedHashSet<Integer>>> originGroups = groupsByOrigin.get(originId);
                List<LinkedHashSet<Integer>> sourceGroups = originGroups == null
                        ? groupsByScope.get(discriminant.scopeConstraint)
                        : originGroups.get(discriminant.scopeConstraint);
                LinkedHashMap<String, Object> originStructure = origins.values().stream()
                        .filter(origin -> originId.equals(originTag(origin)))
                        .map(origin -> origin.structure)
                        .filter(Objects::nonNull)
                        .findFirst()
                        .orElse(null);
                String label = resolveSurfaceLabel(discriminant, sourceGroups, originStructure);
                if (label != null) {
                    discriminant.surfaceLabelsByOrigin.put(originId, label);
                    if (discriminant.surfaceLabel == null) {
                        discriminant.surfaceLabel = label;
                    }
                }
            }
        }
    }

    private String resolveSurfaceLabel(ScopeDiscriminant discriminant,
                                       List<LinkedHashSet<Integer>> sourceGroups,
                                       LinkedHashMap<String, Object> structure) {
        if (structure == null || sourceGroups == null || sourceGroups.isEmpty()) {
            return null;
        }
        try {
            Map<String, Object> request = new LinkedHashMap<>();
            request.put("structure", structure);
            request.put("sourceIndexGroups", sourceGroups.stream().map(ArrayList::new).toList());
            Map<?, ?> response = restTemplate.postForObject(ligerApiUrl + "/resolve_source_spans", request, Map.class);
            List<?> spans = response == null || !(response.get("spans") instanceof List<?> values)
                    ? List.of() : values;
            List<String> parts = new ArrayList<>();
            for (Object spanValue : spans) {
                if (spanValue instanceof Map<?, ?> span) {
                    Object rawText = span.get("text");
                    String text = rawText == null ? "" : String.valueOf(rawText).trim();
                    Object start = span.get("start");
                    Object end = span.get("end");
                    if (!text.isEmpty()) {
                        parts.add(text + (start != null && end != null ? "[" + start + "-" + end + "]" : ""));
                    }
                }
            }
            return parts.isEmpty() ? null : String.join(" > ", parts);
        } catch (Exception e) {
            LOGGER.warning("Could not resolve scope discriminant surface label for "
                    + discriminant.discriminantID + ": " + e.getMessage());
            return null;
        }
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
            List<McDiscriminant> mcDiscriminants,
            Map<Integer, GswbProofInput> origins
    ) {
        List<GswbDiscriminant> out = new ArrayList<>();
        for (ScopeDiscriminant d : scopeDiscriminants) {
            GswbDiscriminant output = new GswbDiscriminant(d.discriminantID, "scope", d.scopeConstraint,
                    d.solutionIds, d.instantiations, d.surfaceLabel);
            output.originIds = d.originIds;
            output.surfaceLabelsByOrigin = d.surfaceLabelsByOrigin;
            out.add(output);
        }
        for (McDiscriminant mc : mcDiscriminants) {
            GswbDiscriminant output = new GswbDiscriminant(mc.discriminantID, "MCs", mc.meaningConstructor,
                    mc.associatedSolutions);
            for (Integer mcSetId : mc.mcSetIds) {
                GswbProofInput origin = origins.get(mcSetId);
                String originId = originTag(origin);
                if (originId != null) {
                    mc.originIds.add(originId);
                }
            }
            output.originIds = mc.originIds;
            out.add(output);
        }
        return out;
    }

    private void addOrigin(ScopeDiscriminant discriminant, GswbProofInput origin) {
        String originId = originTag(origin);
        if (originId == null || originId.isBlank()) {
            return;
        }
        discriminant.originIds.add(originId);
    }

    private String originTag(GswbProofInput origin) {
        if (origin == null) {
            return null;
        }
        return origin.mcSetId == null || origin.mcSetId.isBlank()
                ? origin.proofId
                : origin.mcSetId;
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
