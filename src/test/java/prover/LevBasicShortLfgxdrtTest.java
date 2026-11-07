package prover;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import glueSemantics.parser.SemanticParser;
import main.Settings;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import utilities.LexVariableHandler;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbPreferences;
import webservice.rest.dtos.GswbProofInput;
import webservice.rest.dtos.GswbRequest;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The basic-short meaning constructors (see {@link LevBasicShortTest}) through {@code /deduce} the way
 * the client sends them: Lev prover, LFGxDRT output, semantics unparsed, beta reduction and DRS
 * resolution on. Each case's response must equal the baseline: the readings, their DRS graphs with
 * the SRC provenance of every node, source indices, syn-sem mapping and discriminants. Left out:
 * the SVG renderings, the log and the timing.
 *
 * Recorded 2026-11-07 before the discharge wrapping of LLProver1 and LLProver2 was moved into one
 * handler. Re-record (only on purpose) with
 * {@code mvn test -Dtest=LevBasicShortLfgxdrtTest -Dlev.writeLfgxdrtReference=true}.
 */
class LevBasicShortLfgxdrtTest {

    private static final String REFERENCE = "/lev/reference-llprover1-lfgxdrt.txt";
    private static final Path REFERENCE_SOURCE = Path.of("src/test/resources" + REFERENCE);
    private static final Set<String> LEFT_OUT = Set.of("svg", "log", "elapsedMs");
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);

    @TestFactory
    Stream<DynamicTest> responsesMatchTheBaseline() throws Exception {
        List<LevBasicShortTest.Case> cases = LevBasicShortTest.cases();
        if (Boolean.getBoolean("lev.writeLfgxdrtReference")) {
            writeReference(cases);
        }
        Map<String, String> reference = readReference();
        return cases.stream().map(c -> DynamicTest.dynamicTest(c.toString(), () -> {
            assertTrue(reference.containsKey(c.label()), "no baseline for case " + c);
            String found = assertTimeoutPreemptively(Duration.ofSeconds(60), () -> response(c));
            assertEquals(reference.get(c.label()), found, c.toString());
        }));
    }

    static String response(LevBasicShortTest.Case c) throws Exception {
        GswbPreferences prefs = new GswbPreferences();
        prefs.prover = Settings.LEV;
        prefs.outputstyle = Settings.LFGXDRT;
        prefs.parseSem = false;
        prefs.betaReduce = true;
        prefs.resolveDrs = true;
        GswbRequest request = new GswbRequest();
        request.gswbPreferences = prefs;
        GswbProofInput input = new GswbProofInput();
        input.proofId = c.label();
        input.meaningConstructors = String.join("\n", c.lines());
        request.proofs = new ArrayList<>(List.of(input));

        // The controller replaces the static SemanticParser.settings per request; restore it so the
        // other tests keep their output style.
        Settings staticSettings = SemanticParser.settings;
        try {
            LexVariableHandler.resetVars();
            JsonNode tree = MAPPER.valueToTree(new GswbController().glueDeduce(request));
            strip(tree);
            return MAPPER.writeValueAsString(MAPPER.convertValue(tree, Object.class));
        } finally {
            SemanticParser.settings = staticSettings;
            LexVariableHandler.resetVars();
        }
    }

    private static void strip(JsonNode node) {
        if (node instanceof ObjectNode object) {
            Iterator<Map.Entry<String, JsonNode>> fields = object.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> field = fields.next();
                if (field.getValue().isNull() || LEFT_OUT.contains(field.getKey())) {
                    fields.remove();
                } else {
                    strip(field.getValue());
                }
            }
        } else if (node instanceof ArrayNode array) {
            array.forEach(LevBasicShortLfgxdrtTest::strip);
        }
    }

    /** Reference format: a line "== <case label>", then the case's response as one line of JSON. */
    private static void writeReference(List<LevBasicShortTest.Case> cases) throws Exception {
        StringBuilder out = new StringBuilder();
        for (LevBasicShortTest.Case c : cases) {
            out.append("== ").append(c.label()).append('\n').append(response(c)).append('\n');
        }
        Files.writeString(REFERENCE_SOURCE, out.toString(), StandardCharsets.UTF_8);
        throw new IllegalStateException("baseline re-recorded to " + REFERENCE_SOURCE.toAbsolutePath()
                + "; rerun without -Dlev.writeLfgxdrtReference to check against it");
    }

    private static Map<String, String> readReference() throws IOException {
        Map<String, String> out = new LinkedHashMap<>();
        String label = null;
        try (InputStream in = LevBasicShortLfgxdrtTest.class.getResourceAsStream(REFERENCE)) {
            if (in == null) {
                throw new IOException("missing test resource " + REFERENCE);
            }
            for (String line : Arrays.asList(new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\\R"))) {
                if (line.startsWith("== ")) {
                    label = line.substring(3).trim();
                } else if (label != null && !line.isEmpty()) {
                    out.put(label, line);
                    label = null;
                }
            }
        }
        return out;
    }
}
