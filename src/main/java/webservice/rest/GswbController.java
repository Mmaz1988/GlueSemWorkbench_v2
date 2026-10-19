package webservice.rest;

import Discriminants.ScopeDiscriminant;
import de.ukon.lfgxdrt.SemanticExpression;
import de.ukon.lfgxdrt.DrsParser;
import de.ukon.lfgxdrt.DrsGraphParser;
import de.ukon.lfgxdrt.DrsSequenceMerger;
import de.ukon.lfgxdrt.DrsReasoningCheckBuilder;
import de.ukon.lfgxdrt.ReasoningCheckType;
import de.ukon.lfgxdrt.ReadingRebase;
import de.ukon.lfgxdrt.drs_elements.Collapse;
import de.ukon.lfgxdrt.drs_elements.PresuppositionBox;
import de.ukon.lfgxdrt.drs_elements.DRS;
import de.ukon.lfgxdrt.drs_elements.AnaphoraMapping;
import de.ukon.lfgxdrt.drs_elements.AnaphoraRelation;
import de.ukon.lfgxdrt.drs_elements.DiscourseReferent;
import de.ukon.lfgxdrt.drs_elements.PresuppositionMapping;
import de.ukon.lfgxdrt.drs_elements.PresuppositionRelation;
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
    {
        // Carry this request's id to LiGER, so its log lines for the call share our tag.
        restTemplate.getInterceptors().add((outgoing, body, execution) -> {
            String id = org.slf4j.MDC.get(webservice.RequestIdFilter.MDC_ID);
            if (id != null) {
                outgoing.getHeaders().set(webservice.RequestIdFilter.HEADER, id);
            }
            return execution.execute(outgoing, body);
        });
    }
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

        List<String> ids = sortedBatchKeys(request.items.keySet());
        long started = System.currentTimeMillis();
        int solutions = 0;
        LOGGER.info("Batch of " + ids.size() + " item(s), session " + sessionKey);

        for (String id : ids) {
          try (var tag = webservice.RequestIdFilter.item(id)) {
            // The same payload /deduce takes, so a batch parse yields the same per-sentence
            // result -- structure, proofs, per-origin provenance and surface labels
            // included. A batch endpoint is N single calls with shared setup (the run
            // context, the parser, and progressive persistence below); it may not take or
            // return less.
            GswbRequest item = request.items.get(id);
            if (item == null) {
                continue;
            }
            SingleRunResult run = deduceOne(item, ctx, gp, true, false);

            analyses.put(id, run.output);
            solutions += run.countSolutions;

            reportBuilder.append(String.format("%s\t\t%s\t\t\t%s", id, run.noOfMCs, run.countSolutions));
            reportBuilder.append(System.lineSeparator());

            gswbRedisSessionService.saveBatchOutput(sessionKey, new GswbBatchOutput(new HashMap<>(analyses), reportBuilder.toString()));
          }
        }

        LOGGER.info("Batch done: " + analyses.size() + " item(s), " + solutions + " solution(s), "
                + (System.currentTimeMillis() - started) + " ms");
        GswbBatchOutput batchOutput = new GswbBatchOutput(analyses, reportBuilder.toString());
        gswbRedisSessionService.saveBatchOutput(sessionKey, batchOutput);
        return batchOutput;
    }

    @CrossOrigin
    @PostMapping(value = "/deduce", produces = "application/json", consumes = "application/json")
    public GswbOutput glueDeduce(@RequestBody GswbRequest request) throws Exception {

        RunContext ctx = buildRunContext(request.gswbPreferences);
        LOGGER.fine("Received /deduce request: proofs="
                + (request.proofs == null ? 0 : request.proofs.size())
                + ", premisesLength=" + (request.premises == null ? 0 : request.premises.length())
                + ", hasStructure=" + (request.structure != null));

        GlueParser gp = new GlueParser(ctx.settings);
        SingleRunResult run = deduceOne(request, ctx, gp, false, true);

        LOGGER.fine("Completed /deduce request: solutions=" + run.output.solutions.size()
                + ", discriminants=" + run.output.discriminants.size());
        return run.output;
    }

    /** One sentence's deduction: the structured-proofs path when the caller supplies
     *  {@code proofs} (one {@link GswbProofInput} per syntactic origin), the flat-premises
     *  path otherwise.
     *
     *  Extracted so {@code /gswb_batch_proof} is literally N of these rather than a
     *  parallel implementation that could only take flat premises. Without a structure and
     *  origins, {@code resolveSurfaceLabel} returns null before it even calls LiGER's
     *  {@code /resolve_source_spans}, so every batch discriminant came back with no
     *  surface label and no origin ids, and every solution id was a bare {@code s0}
     *  repeated across sentences. See
     *  xleplusglue/docs/plans/SHARED_PIPELINE_PLAN.md, Stage 4 and invariant I6.
     */
    private SingleRunResult deduceOne(GswbRequest request, RunContext ctx, GlueParser gp,
                                      boolean batchMode, boolean includeDerivation) throws Exception {
        long started = System.currentTimeMillis();
        SingleRunResult run = deduceOneQuietly(request, ctx, gp, batchMode, includeDerivation);
        long elapsedMs = System.currentTimeMillis() - started;
        if (run.output != null) {
            run.output.elapsedMs = elapsedMs;
        }
        // One line per deduction, /deduce or batch item alike, like LiGER's per-sentence lines.
        int origins = request.proofs == null || request.proofs.isEmpty() ? 1 : request.proofs.size();
        int discriminants = run.output == null || run.output.discriminants == null ? 0 : run.output.discriminants.size();
        LOGGER.info("Deduced " + describeInput(request) + ": " + run.noOfMCs + " meaning constructor(s) from "
                + origins + " syntactic origin(s), " + run.countSolutions + " solution(s), "
                + discriminants + " discriminant(s), " + elapsedMs + " ms");
        return run;
    }

    /** The sentence(s) a deduction is about, for its log line: the structure's text, quoted;
     *  a sequence ('A.' 'B.') as A. | B. -- unless the deduction proves one sentence's part of
     *  a sequence, which is named on its own (see describeSequencePart). */
    private static String describeInput(GswbRequest request) {
        String part = describeSequencePart(request.proofs);
        if (part != null) {
            return part;
        }
        Object text = request.structure == null ? null : request.structure.get("text");
        if ((text == null || text.toString().isBlank()) && request.proofs != null) {
            text = request.proofs.stream()
                    .map(proof -> proof.structure == null ? null : proof.structure.get("text"))
                    .filter(value -> value != null && !value.toString().isBlank())
                    .findFirst().orElse(null);
        }
        if (text == null || text.toString().isBlank()) {
            return "(no text)";
        }
        return quoted(text.toString());
    }

    /** A deduction of one sentence's part of a sequence proves that sentence alone, though its
     *  structure is the whole sequence's: named by the sentence's text and its place, e.g.
     *  "Every Swede is a Scandinavian." (part 2 of 3). Null unless every proof carries the same
     *  sentence and a sequence of more than one. */
    private static String describeSequencePart(List<GswbProofInput> proofs) {
        if (proofs == null || proofs.isEmpty()) {
            return null;
        }
        Object text = proofs.get(0).sentenceAnalysis == null ? null : proofs.get(0).sentenceAnalysis.get("text");
        if (text == null || text.toString().isBlank()) {
            return null;
        }
        for (GswbProofInput proof : proofs) {
            if (proof.sentenceAnalysis == null || !text.equals(proof.sentenceAnalysis.get("text"))) {
                return null;
            }
        }
        Object sentences = proofs.get(0).sequenceAnalysis == null ? null : proofs.get(0).sequenceAnalysis.get("sentences");
        if (!(sentences instanceof List<?> list) || list.size() < 2) {
            return null;
        }
        Object id = proofs.get(0).sentenceAnalysis.get("id");
        int index = -1;
        for (int i = 0; i < list.size() && index < 0; i++) {
            if (list.get(i) instanceof Map<?, ?> sentence
                    && (id != null ? id.equals(sentence.get("id")) : text.equals(sentence.get("text")))) {
                index = i;
            }
        }
        return quoted(text.toString()) + (index < 0 ? " (part of " : " (part " + (index + 1) + " of ")
                + list.size() + ")";
    }

    private static String quoted(String text) {
        String oneLine = text.replaceAll("\\s+", " ").trim()
                .replace("' '", " | ").replaceAll("^'|'$", "");
        return "\"" + (oneLine.length() > 120 ? oneLine.substring(0, 117) + "..." : oneLine) + "\"";
    }

    private SingleRunResult deduceOneQuietly(GswbRequest request, RunContext ctx, GlueParser gp,
                                             boolean batchMode, boolean includeDerivation) throws Exception {
        if (request.proofs != null && !request.proofs.isEmpty()) {
            ParsedProofInputs parsed = parseProofInputs(request.proofs, gp, ctx.multistage);
            LOGGER.fine("Parsed structured proof inputs: records=" + request.proofs.size()
                    + ", mcSets=" + parsed.entries.lexicalEntries.size()
                    + ", origins=" + parsed.origins.size());
            return runAndFormatSingle(
                    parsed.entries, parsed.origins, ctx, batchMode, includeDerivation, request.structure);
        }
        InputOutputProcessor.process(request.premises);
        String input = InputOutputProcessor.translate(request.premises);
        return runAndFormatSingle(input, ctx, gp, batchMode, includeDerivation, request.structure);
    }

    @CrossOrigin
    @PostMapping(value = "/generate_pcdrs", produces = "application/json", consumes = "application/json")
    public GswbPcdrsOutput generatePcdrs(@RequestBody GswbPcdrsRequest request) throws Exception {
        if (request == null || (!hasCanonicalGraph(request.mergedStructure)
                && (request.semantic == null || request.semantic.isBlank()))) {
            throw new IllegalArgumentException("A semantic DRS or canonical graph is required to generate PCDRS solutions.");
        }

        LOGGER.info("PCDRS request received: parentSolutionId=" + request.parentSolutionId
                + ", mergedStructureKeys="
                + (request.mergedStructure == null ? 0 : request.mergedStructure.keySet().size()));
        LOGGER.fine("PCDRS request semantic: " + request.semantic
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
        LOGGER.fine("PCDRS semantic node names: " + nodeNames);
        // The rules' candidates (I5), by the NAME the graph gives referents and states, which is
        // what the PCDRS prints: antecedents of pronouns and bind referents, accommodation sites
        // of p-boxes.
        LinkedHashMap<String, LinkedHashSet<String>> candidates = extractFacts(request.mergedStructure, nodeNames, "POSSIBLE-ANT");
        LinkedHashMap<String, LinkedHashSet<String>> sites = extractFacts(request.mergedStructure, nodeNames, "POSSIBLE-ACC");
        LOGGER.fine("PCDRS possible-ant candidates: " + candidates + ", possible-acc sites: " + sites);
        List<Reading> readings = readings(drs, candidates, sites);
        LOGGER.info("PCDRS readings: " + readings.size());

        String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                ? "solution"
                : request.parentSolutionId;
        List<GswbSolution> solutions = new ArrayList<>();
        List<GswbSolution> rejected = new ArrayList<>();
        // I10: readings with the same collapse and the same P: are one reading.
        Set<String> meanings = new HashSet<>();
        int same = 0;
        for (Reading reading : readings) {
            AnaphoraMapping mapping = toAnaphoraMapping(reading.anaphora());
            PresuppositionMapping presuppositions = new PresuppositionMapping();
            reading.sites().forEach((presupposition, site) ->
                    presuppositions.addRelation(new PresuppositionRelation(presupposition, site)));
            int branch = solutions.size() + rejected.size() + 1;
            String id = parentId + "-pcdrs-" + branch;
            try {
                // LiGER names referents as the graph does, which is as the PCDRS prints them. P:
                // gets an entry for every p-box, a bound one's derived from A: (I6).
                DRS pcdrs = drs.withAnaphoraMapping(mapping).bindAnaphoraMappingByPrintedNames()
                        .withPresuppositionMapping(presuppositions).withCompletePresuppositionMapping();
                String semantic = pcdrs.toString();
                // As the PCDRS prints it: the pronoun with its state, the antecedent read on its path.
                String printedMapping = pcdrs.anaphoraMapping == null ? mapping.toString() : pcdrs.anaphoraMapping.toString();
                String svg = new DrsSvgRenderer().toSvg(pcdrs);
                // The collapse renumbers the referents it shares with the PCDRS: print first.
                Collapse collapse = pcdrs.collapse();
                if (!meanings.add(collapse.drs().toString() + " " + pcdrs.presuppositionMapping)) {
                    same++;
                    continue;
                }
                GswbSolution solution = new GswbSolution(svg, id, drs.getSourceIndex(), null, semantic);
                solution.anaphoraMapping = printedMapping;
                solution.anaphoraRelations = AnaphoraMappingConverter.toDto(mapping);
                solution.presuppositionMapping = pcdrs.presuppositionMapping.toString();
                solution.collapseReport = collapse.reportText();
                solutions.add(solution);
                LOGGER.fine("PCDRS reading " + id + ": " + semantic + " -- " + solution.collapseReport);
            } catch (IllegalStateException e) {
                // I7: a reading that fails a check is rejected, and says so; it is not a branch.
                GswbSolution solution = new GswbSolution(null, id);
                // The rules' names, with their states: a rejected binding may name an antecedent
                // off the pronoun's path, which a plain name cannot.
                solution.anaphoraMapping = mapping.toString();
                solution.presuppositionMapping = presuppositions.toString();
                solution.rejection = e.getMessage();
                rejected.add(solution);
                LOGGER.fine("PCDRS reading " + id + " rejected: " + e.getMessage());
            }
        }
        LOGGER.info("PCDRS readings: accepted=" + solutions.size() + ", rejected=" + rejected.size()
                + ", same as an accepted one=" + same);
        GswbPcdrsOutput output = new GswbPcdrsOutput(request.parentSolutionId, solutions);
        output.rejected = rejected;
        return output;
    }

    /** A reading's choices (I10): antecedents by pronoun or bind referent, sites by p-box. */
    private record Reading(LinkedHashMap<String, String> anaphora, LinkedHashMap<String, String> sites) {

        Reading with(Reading choice) {
            LinkedHashMap<String, String> anaphora = new LinkedHashMap<>(this.anaphora);
            anaphora.putAll(choice.anaphora);
            LinkedHashMap<String, String> sites = new LinkedHashMap<>(this.sites);
            sites.putAll(choice.sites);
            return new Reading(anaphora, sites);
        }
    }

    /**
     * Every reading (I10): one choice for every pronoun -- one of its antecedents -- and for every
     * p-box: in situ, another site the rules allow (one per site, I3: the states of one chain
     * within one segment are one site), or a binder for each of its bind referents. A binder does
     * not suppress accommodation. The collapse decides which readings stand.
     */
    private List<Reading> readings(DRS drs, LinkedHashMap<String, LinkedHashSet<String>> antecedents,
                                   LinkedHashMap<String, LinkedHashSet<String>> sites) {
        List<PresuppositionBox> boxes = drs.presuppositions();
        Map<String, String> siteOf = drs.sites();
        Set<String> bindReferents = boxes.stream().flatMap(box -> box.bindReferents().stream())
                .collect(Collectors.toSet());
        List<List<Reading>> choicePoints = new ArrayList<>();
        antecedents.forEach((pronoun, candidates) -> {
            if (!bindReferents.contains(pronoun)) {
                choicePoints.add(candidates.stream().map(antecedent -> new Reading(
                        new LinkedHashMap<>(Map.of(pronoun, antecedent)), new LinkedHashMap<>())).toList());
            }
        });
        for (PresuppositionBox box : boxes) {
            List<Reading> choices = new ArrayList<>();
            choices.add(new Reading(new LinkedHashMap<>(), new LinkedHashMap<>(Map.of(box.label(), box.label()))));
            // In situ is the host's site; every other site once, by its first state.
            Set<String> offered = new HashSet<>(Set.of(siteOf.getOrDefault(box.host(), box.host()), box.label()));
            for (String site : sites.getOrDefault(box.label(), new LinkedHashSet<>())) {
                String first = siteOf.getOrDefault(site, site);
                if (offered.add(first)) {
                    choices.add(new Reading(new LinkedHashMap<>(), new LinkedHashMap<>(Map.of(box.label(), first))));
                }
            }
            if (!box.bindReferents().isEmpty() && box.bindReferents().stream().allMatch(antecedents::containsKey)) {
                LinkedHashMap<String, LinkedHashSet<String>> binders = new LinkedHashMap<>();
                box.bindReferents().forEach(referent -> binders.put(referent, antecedents.get(referent)));
                for (LinkedHashMap<String, String> binding : expandAnaphoraMappings(binders)) {
                    choices.add(new Reading(binding, new LinkedHashMap<>()));
                }
            }
            choicePoints.add(choices);
        }
        List<Reading> readings = List.of(new Reading(new LinkedHashMap<>(), new LinkedHashMap<>()));
        for (List<Reading> choices : choicePoints) {
            List<Reading> next = new ArrayList<>();
            for (Reading partial : readings) {
                for (Reading choice : choices) {
                    next.add(partial.with(choice));
                }
            }
            readings = next;
        }
        return readings;
    }


    /** Renders PCDRS branches to the SVG /generate_pcdrs produced for them (see PcdrsRenderer
     *  for which semantic to send), so a client can show rendered branches without persisting
     *  every SVG. One request for many branches;
     *  an item that cannot be parsed or rendered comes back with no `solution` rather than
     *  failing the batch. */
    @CrossOrigin
    @PostMapping(value = "/render_pcdrs", produces = "application/json", consumes = "application/json")
    public GswbPcdrsOutput renderPcdrs(@RequestBody GswbRenderPcdrsRequest request) {
        List<GswbSolution> solutions = new ArrayList<>();
        List<GswbRenderPcdrsRequest.Item> items = request == null || request.items == null ? List.of() : request.items;
        for (GswbRenderPcdrsRequest.Item item : items) {
            String svg = null;
            try {
                svg = PcdrsRenderer.toSvg(item.semantic, item.anaphoraRelations);
            } catch (Exception e) {
                LOGGER.warning("Could not render PCDRS " + item.id + ": " + e.getMessage());
            }
            GswbSolution solution = new GswbSolution(svg, item.id, null, null, item.semantic);
            solution.anaphoraRelations = item.anaphoraRelations;
            solutions.add(solution);
        }
        LOGGER.info("PCDRS render complete: items=" + items.size()
                + ", rendered=" + solutions.stream().filter(solution -> solution.solution != null).count());
        return new GswbPcdrsOutput(null, solutions);
    }

    @CrossOrigin
    @PostMapping(value = "/collapse_anaphora", produces = "application/json", consumes = "application/json")
    public GswbSolution collapseAnaphora(@RequestBody GswbCollapseAnaphoraRequest request) throws Exception {
        if (request == null || request.semantic == null || request.semantic.isBlank()) {
            throw new IllegalArgumentException("A PCDRS semantic is required to collapse anaphora.");
        }

        LOGGER.info("Collapse request received: parentSolutionId=" + request.parentSolutionId);
        LOGGER.fine("Collapse request semantic: " + request.semantic);
        // A PCDRS prints its merge chain (I2), which reads back as a merge to resolve; its A: and
        // P: come inline.
        SemanticExpression expression = new DrsParser().parse(request.semantic).expression.resolveMerges();
        if (!(expression instanceof DRS pcdrs)) {
            throw new IllegalArgumentException("The collapse requires a DRS semantic expression.");
        }
        String parentId = request.parentSolutionId == null || request.parentSolutionId.isBlank()
                ? "solution"
                : request.parentSolutionId;
        GswbSolution result;
        try {
            Collapse collapse = pcdrs.collapse();
            result = new GswbSolution(new DrsSvgRenderer().toSvg(collapse.drs()), parentId + "-collapsed",
                    collapse.drs().getSourceIndex(), null, collapse.drs().toString());
            result.collapseReport = collapse.reportText();
        } catch (IllegalStateException e) {
            // I7: the reading is rejected, and says why; nothing falls back to another one.
            result = new GswbSolution(null, parentId + "-collapsed");
            result.rejection = e.getMessage();
        }
        LOGGER.info("Collapse complete: solutionId=" + result.id + (result.rejection == null ? ""
                : ", rejected: " + result.rejection));
        return result;
    }

    /**
     * The collapse and TPTP translation of one reading's reasoning items, in one request: the
     * discourse ("sequence", the PCDRS with its A: and P: inline), the prior on its own
     * ("context") and the four checks. The reading is the discourse's (I11): each check gets it
     * rebased ({@link ReadingRebase}), the context Q's own part of it. An item whose collapse
     * rejects the reading comes back without TPTP and says why; nothing falls back.
     */
    @CrossOrigin
    @PostMapping(value = "/collapse_and_tptp_batch", produces = "application/json", consumes = "application/json")
    public GswbCollapseAndTptpBatchOutput collapseAndTptpBatch(@RequestBody GswbCollapseAndTptpBatchRequest request) throws Exception {
        if (request == null || request.items == null || request.items.isEmpty()) {
            throw new IllegalArgumentException("At least one semantic item is required to collapse and translate.");
        }
        Map<String, DRS> parsed = new LinkedHashMap<>();
        for (GswbTptpBatchItem item : request.items) {
            if (item == null || item.name == null || item.semantic == null || item.semantic.isBlank()) {
                throw new IllegalArgumentException("Every batch item requires a name and a non-blank semantic.");
            }
            // Resolving is how a merge is translated, also one inside a condition (the antecedent
            // of "every" in a prior sent with resolveDrs off); a resolved DRS stays as it is.
            if (!(new DrsParser().parse(item.semantic).expression.resolveMerges() instanceof DRS drs)) {
                throw new IllegalArgumentException("Batch item " + item.name + " is no DRS.");
            }
            parsed.put(item.name, drs);
        }
        DRS discourse = parsed.get("sequence");

        LinkedHashMap<String, GswbTptpBatchResult> results = new LinkedHashMap<>();
        for (Map.Entry<String, DRS> item : parsed.entrySet()) {
            GswbTptpBatchResult result = new GswbTptpBatchResult();
            try {
                DRS reading = item.getValue();
                ReasoningCheckType check = checkType(item.getKey());
                if (discourse != null && check != null) {
                    ReadingRebase.Rebased rebased = ReadingRebase.intoCheck(discourse, reading, check);
                    reading = rebased.reading();
                    result.stateMap = rebased.states();
                } else if (discourse != null && "context".equals(item.getKey())) {
                    reading = ReadingRebase.intoContext(discourse, reading);
                }
                result.anaphoraMapping = reading.anaphoraMapping == null ? null : reading.anaphoraMapping.toString();
                result.presuppositionMapping = reading.presuppositionMapping == null ? null
                        : reading.presuppositionMapping.toString();
                // A pronoun the reading does not bind is read as a new referent (I7.4); named here.
                List<DiscourseReferent> unresolved = new ArrayList<>();
                DRS marked = reading.withoutUnmappedAnaphora(unresolved);
                result.unresolvedPronouns = unresolved.stream().map(DiscourseReferent::toSimpleString).toList();
                Collapse collapse = marked.collapse();
                result.collapseReport = collapse.reportText();
                result.semantic = collapse.drs().toString();
                result.tptp = collapse.drs().toTPTPString(request.typed);
            } catch (IllegalStateException e) {
                result.tptp = "";
                result.rejection = e.getMessage();
                LOGGER.warning("Collapse/TPTP batch item rejected for parentSolutionId=" + request.parentSolutionId
                        + ", item=" + item.getKey() + ": " + e.getMessage());
            }
            results.put(item.getKey(), result);
        }

        // One line per call, counts and ids only: this endpoint runs once per reading, so
        // anything DRS-bearing here belongs at FINE.
        LOGGER.info("Collapse/TPTP batch complete: parentSolutionId=" + request.parentSolutionId
                + ", items=" + results.size()
                + ", translated=" + results.values().stream().filter(result -> !result.tptp.isBlank()).count()
                + ", unresolvedPronouns=" + results.values().stream()
                        .mapToInt(result -> result.unresolvedPronouns.size()).sum());

        GswbCollapseAndTptpBatchOutput output = new GswbCollapseAndTptpBatchOutput();
        output.parentSolutionId = request.parentSolutionId;
        output.results = results;
        return output;
    }

    /** The check a batch item named {@code name} is, or null. */
    private static ReasoningCheckType checkType(String name) {
        return Arrays.stream(ReasoningCheckType.values())
                .filter(type -> type.externalName().equals(name)).findFirst().orElse(null);
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
                LOGGER.fine("Sequence part reconstructed: source=" + expression.getSourceIndex()
                        + ", representation=" + (part.graph != null ? "graph" : "semantic")
                        + ", expression=" + expression);
            }

            SemanticExpression merged = DrsSequenceMerger.merge(expressions);
            SemanticExpression resolvedExpression = merged.resolveMerges();
            if (!(resolvedExpression instanceof DRS resolved)) {
                throw new IllegalStateException("Sequence merge did not resolve to a DRS");
            }
            SemanticExpression displayExpression = mergeForDisplay(expressions, request.resolveDrs);
            LOGGER.fine("Sequence display expression: resolveEach=" + request.resolveDrs
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
            LOGGER.fine("Sequence graph reconstructed: source=" + expression.getSourceIndex()
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
            LOGGER.fine("Sequence display expression reconstructed from semantic strings: "
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

    /** Sequence-merge outputs are LFGxDRT-only by construction (sequencing is only exposed
     *  for that semantic type), so their SVG (already rendered into solution.solution) and
     *  semType are unconditional. */
    private GswbSemanticAnalysis semanticAnalysis(GswbSolution solution, String syntacticOrigin) {
        return semanticAnalysis(solution, syntacticOrigin, solution.solution, SemanticModel.LFGXDRT.value());
    }

    private GswbSemanticAnalysis semanticAnalysis(GswbSolution solution, String syntacticOrigin,
                                                   String svg, String semType) {
        String semId = solution.id == null ? "semantic" : solution.id;
        return new GswbSemanticAnalysis(
                syntacticOrigin,
                semId,
                solution.semantic,
                null,
                solution.graph,
                semType,
                svg);
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

        LOGGER.fine("Running prover...");

        ProofRun run = runProofsOverLexicalEntries(mcs, prover, sb, batchMode, origins);

        LexicalEntries filteredMcs = filterLexicalEntriesByKeySet(mcs, run.mcSetWithSolution);
        List<McDiscriminant> finalMcDiscriminants = filteredMcs.calculateDiscriminants();

        LOGGER.fine("Formatting output...");

        SolutionsAndDiscriminants formatted =
                formatSolutionsAndDiscriminants(run.allSolutions, finalMcDiscriminants, prover.scope2instantiations,
                        prover.scope2InstantiationsByOrigin,
                        prover.scope2SourceIndexGroups, prover.scope2SourceIndexGroupsByOrigin,
                        structure, ctx.settings, origins);

        applyOptionalSemanticRendering(ctx, formatted);

        List<GswbSolution> outputSolutions = toOutputSolutions(formatted.solutionIndexToObject, ctx.displayLfgxDrt);

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
        copy.sentenceId = input.sentenceId;
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
        return new ProofRun(orderMcSetsByViolations(allSolutions, origins), mcSetWithSolution, noOfMCs,
                countSolutions, log);
    }

    /** The prover orders readings within one MC set; this orders a sentence's MC sets among
     *  themselves, by their best reading (fewest insitu, then fewest surface-order violations),
     *  so the most surface-faithful reading leads. Sets stay whole -- readings are never
     *  interleaved across sets -- and the sort is stable. Each sentence's sets keep the slots
     *  they had in the request, so sentences are not reordered against each other. */
    public static LinkedHashMap<Integer, List<SolutionObject>> orderMcSetsByViolations(
            LinkedHashMap<Integer, List<SolutionObject>> allSolutions,
            Map<Integer, GswbProofInput> origins) {
        List<Integer> keys = new ArrayList<>(allSolutions.keySet());
        Map<String, List<Integer>> slotsBySentence = new LinkedHashMap<>();
        for (int slot = 0; slot < keys.size(); slot++) {
            GswbProofInput origin = origins == null ? null : origins.get(keys.get(slot));
            String sentence = origin == null ? null : origin.sentenceId;
            slotsBySentence.computeIfAbsent(sentence, ignored -> new ArrayList<>()).add(slot);
        }
        // A set's best reading is its first: the prover already sorted within the set. Sets
        // without a reading go last.
        Comparator<Integer> byBestReading = Comparator
                .comparingInt((Integer key) -> allSolutions.get(key).isEmpty()
                        ? Integer.MAX_VALUE : allSolutions.get(key).get(0).insituViolations)
                .thenComparingInt(key -> allSolutions.get(key).isEmpty()
                        ? Integer.MAX_VALUE : allSolutions.get(key).get(0).surfaceOrderViolations);
        Integer[] ordered = new Integer[keys.size()];
        for (List<Integer> slots : slotsBySentence.values()) {
            List<Integer> sentenceKeys = new ArrayList<>();
            for (Integer slot : slots) {
                sentenceKeys.add(keys.get(slot));
            }
            sentenceKeys.sort(byBestReading);
            for (int i = 0; i < slots.size(); i++) {
                ordered[slots.get(i)] = sentenceKeys.get(i);
            }
        }
        LinkedHashMap<Integer, List<SolutionObject>> result = new LinkedHashMap<>();
        for (Integer key : ordered) {
            result.put(key, allSolutions.get(key));
        }
        return result;
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

    private List<GswbSolution> toOutputSolutions(Map<Integer, SolutionObject> solutionsByIndex, boolean isLfgxdrt) {
        List<GswbSolution> outputSolutions = new ArrayList<>();
        for (Integer idx : solutionsByIndex.keySet().stream().sorted().toList()) {
            SolutionObject so = solutionsByIndex.get(idx);
            GswbSolution output = new GswbSolution(so.solutionString, so.solutionId, so.sourceIndex, so.graph,
                    so.semantic);
            output.proofId = so.proofId;
            output.solutionKey = so.solutionKey;
            output.mcSetId = so.mcSetId;
            output.insituViolations = so.insituViolations;
            output.surfaceOrderViolations = so.surfaceOrderViolations;
            // solutionString is only ever rendered as SVG in LFGxDRT display mode
            // (applyOptionalSemanticRendering); in prolog-drt mode it's plain DRT/solution text.
            String svg = isLfgxdrt ? output.solution : null;
            String semType = isLfgxdrt ? SemanticModel.LFGXDRT.value() : SemanticModel.PROLOG_DRT.value();
            output.semanticAnalysis = semanticAnalysis(output, so.solutionKey, svg, semType);
            if (so.solutionKey != null && !so.solutionKey.isBlank()) {
                output.synSemMapping.put(so.solutionKey, List.of(output.id));
            }
            outputSolutions.add(output);
        }
        return outputSolutions;
    }

    /**
     * The graph post-processing reads: the resolved DRS, whatever the display shows (I5) -- so
     * its state and referent names are the labels the PCDRS will print. Resolving folds merges
     * of boxes without referents, which renumbers, so an unresolved graph would name states the
     * PCDRS does not have. Built from a copy: resolving must not renumber the referents the
     * display still prints (I8).
     */
    private LinkedHashMap<String, Object> semanticGraphFor(SemanticExpression expression) {
        try {
            SemanticExpression copy;
            try {
                copy = de.ukon.lfgxdrt.DrsAstCopier.copyStructure(expression);
            } catch (IllegalArgumentException lambdaTerm) {
                // Not beta-reduced yet (a display preference): reduce, then copy.
                copy = de.ukon.lfgxdrt.DrsAstCopier.copyStructure(expression.betaReduce());
            }
            return copy.resolveMerges().toJson();
        } catch (Exception e) {
            LOGGER.warning("Could not derive a provenance graph: " + e.getMessage());
            return null;
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
        // A referent is named with the state that introduces it, as A: writes it (s4:x2):
        // sibling branches reuse names (I3).
        Map<String, String> plain = new HashMap<>(nodeNames);
        forEachFact(structure, "IN", (state, referent) -> {
            if (plain.get(state) != null && plain.get(referent) != null) {
                nodeNames.put(referent, plain.get(state) + ":" + plain.get(referent));
            }
        });
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

    /** The {@code relation} facts of a structure, source NAME -> target NAMEs in the order found. */
    private LinkedHashMap<String, LinkedHashSet<String>> extractFacts(
            Map<String, Object> structure, Map<String, String> nodeNames, String relation) {
        LinkedHashMap<String, LinkedHashSet<String>> candidates = new LinkedHashMap<>();
        collectFacts(structure, nodeNames, relation, candidates);
        return candidates;
    }

    private void collectFacts(Object value,
                              Map<String, String> nodeNames,
                              String wanted,
                              LinkedHashMap<String, LinkedHashSet<String>> candidates) {
        forEachFact(value, wanted, (sourceId, targetId) -> {
            String sourceName = nodeNames.get(sourceId);
            String targetName = nodeNames.get(targetId);
            if (sourceName == null || targetName == null) {
                LOGGER.warning("Ignoring " + wanted + " without NAME features: source="
                        + sourceId + ", target=" + targetId);
            } else if (!sourceName.isEmpty() && !targetName.isEmpty()) {
                candidates.computeIfAbsent(sourceName, ignored -> new LinkedHashSet<>()).add(targetName);
            }
        });
    }

    /** Every {@code wanted} fact of a structure, source id and target id. */
    private void forEachFact(Object value, String wanted, java.util.function.BiConsumer<String, String> action) {
        if (value instanceof Map<?, ?> map) {
            Object relation = map.get("relationLabel");
            Object source = map.containsKey("fsNode") ? map.get("fsNode") : map.get("sourceNode");
            Object target = map.containsKey("fsValue") ? map.get("fsValue") : map.get("targetNode");
            if (wanted.equals(String.valueOf(relation)) && source != null && target != null) {
                action.accept(String.valueOf(source).trim(), String.valueOf(target).trim());
            }
            for (Object child : map.values()) {
                forEachFact(child, wanted, action);
            }
        } else if (value instanceof Iterable<?> iterable) {
            for (Object child : iterable) {
                forEachFact(child, wanted, action);
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
                    currentSO.sentenceId = origin.sentenceId;
                }

                String currentSolution = buildSolutionString(settings, key, i, currentSO).trim();

                String idPrefix = origin != null && origin.sentenceId != null && !origin.sentenceId.isBlank()
                        ? origin.sentenceId
                        : origin != null && origin.solutionKey != null && !origin.solutionKey.isBlank()
                        ? origin.solutionKey
                        : null;
                String currentSolutionId = idPrefix != null ? idPrefix + "-s" + solutionIndex : "s" + solutionIndex;

                for (McDiscriminant d : finalMcDiscriminants) {
                    if (d.mcSetIds.contains(key)) {
                        d.associatedSolutions.add(currentSolutionId);
                    }
                }

                for (String sd : currentSO.scopeDiscriminants) {
                    ScopeDiscriminant existing = scopeDiscriminants.get(sd);
                    if (existing == null) {
                        LinkedHashSet<String> instantiations = instantiationsFor(sd, origin,
                                scope2instantiations, scope2InstantiationsByOrigin);
                        ScopeDiscriminant newSD =
                        new ScopeDiscriminant("sc" + scopeDiscriminantIndex, sd, new HashSet<>(), instantiations);
                        newSD.solutionIds.add(currentSolutionId);
                        addOrigin(newSD, origin);
                        scopeDiscriminants.put(sd, newSD);
                        scopeDiscriminantIndex++;
                    } else {
                        existing.solutionIds.add(currentSolutionId);
                        addOrigin(existing, origin);
                        existing.instantiations.addAll(instantiationsFor(sd, origin,
                                scope2instantiations, scope2InstantiationsByOrigin));
                    }
                }

                currentSO.solutionString = currentSolution;
                currentSO.solutionId = currentSolutionId;

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
        setScopeSurfaceKeys(finalScopeDiscriminants, scope2SourceIndexGroups, scope2SourceIndexGroupsByOrigin);

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

    /** Where each scope discriminant's two elements start in the sentence -- outscoping first --
     *  so a view can list the scopings that follow the surface order first. */
    private void setScopeSurfaceKeys(List<ScopeDiscriminant> discriminants,
                                     LinkedHashMap<String, List<LinkedHashSet<Integer>>> groupsByScope,
                                     LinkedHashMap<String, LinkedHashMap<String, List<LinkedHashSet<Integer>>>> groupsByOrigin) {
        for (ScopeDiscriminant discriminant : discriminants) {
            List<LinkedHashSet<Integer>> groups = groupsByScope.get(discriminant.scopeConstraint);
            if (groups == null) {
                groups = groupsByOrigin.values().stream()
                        .map(byScope -> byScope.get(discriminant.scopeConstraint))
                        .filter(Objects::nonNull)
                        .findFirst()
                        .orElse(null);
            }
            if (groups == null || groups.size() < 2) {
                continue;
            }
            discriminant.surfaceKey = Arrays.asList(
                    groups.get(0).stream().min(Integer::compareTo).orElse(null),
                    groups.get(1).stream().min(Integer::compareTo).orElse(null));
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
            output.surfaceKey = d.surfaceKey;
            if (d.surfaceKey != null && d.surfaceKey.get(0) != null && d.surfaceKey.get(1) != null) {
                output.surfaceAligned = d.surfaceKey.get(0) < d.surfaceKey.get(1);
            }
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
