package prover;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import main.Settings;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import utilities.LexVariableHandler;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unstaged live input: the meaning constructors of inference-basic-short.lfg with the LFGxDRT
 * inference grammar (see the header of lev/basic-short-inference.txt), one case per parse. The
 * solutions must equal the baseline recorded from LLProver1 on 2026-11-07, before the multistage
 * prover was merged into it. Settings as the client sends them: Lev prover, semantics unparsed,
 * relaxed retry allowed.
 *
 * Re-record the baseline (only on purpose) with
 * {@code mvn test -Dtest=LevBasicShortTest -Dlev.writeReference=true}.
 */
class LevBasicShortTest {

    private static final String SUITE = "/lev/basic-short-inference.txt";
    private static final String REFERENCE = "/lev/reference-llprover1.txt";
    private static final Path REFERENCE_SOURCE = Path.of("src/test/resources" + REFERENCE);

    record Case(String label, String sentence, List<String> lines) {
        @Override
        public String toString() {
            return label + ": " + sentence;
        }
    }

    @TestFactory
    Stream<DynamicTest> solutionsMatchTheBaseline() throws Exception {
        List<Case> cases = cases();
        if (Boolean.getBoolean("lev.writeReference")) {
            writeReference(cases);
        }
        Map<String, List<String>> reference = readReference();
        return cases.stream().map(c -> DynamicTest.dynamicTest(c.toString(), () -> {
            assertTrue(reference.containsKey(c.label()), "no baseline for case " + c);
            List<String> found = assertTimeoutPreemptively(Duration.ofSeconds(60), () -> solutions(c, Settings.LEV));
            assertEquals(reference.get(c.label()), found, c.toString());
        }));
    }

    static List<String> solutions(Case c, int proverType) throws Exception {
        LexVariableHandler.resetVars();
        Settings settings = new Settings();
        settings.setProverType(proverType);
        settings.setAllowRelaxedGraph(true);
        LexicalEntries entries = new GlueParser(false).parseMeaningConstructorList(c.lines());
        LLProver prover = proverType == Settings.Multi ? new LLProver3(settings, new StringBuilder())
                : new LLProver1(settings, new StringBuilder());
        try {
            return prover.searchProof(1, entries, true).stream()
                    .map(so -> so.solution.toString())
                    .sorted()
                    .collect(Collectors.toList());
        } finally {
            LexVariableHandler.resetVars();
        }
    }

    /** Reference format: a line "== <case label>", then one solution per line. */
    private static void writeReference(List<Case> cases) throws Exception {
        StringBuilder out = new StringBuilder();
        for (Case c : cases) {
            out.append("== ").append(c.label()).append('\n');
            for (String solution : solutions(c, Settings.LEV)) {
                out.append(solution).append('\n');
            }
        }
        Files.writeString(REFERENCE_SOURCE, out.toString(), StandardCharsets.UTF_8);
        throw new IllegalStateException("baseline re-recorded to " + REFERENCE_SOURCE.toAbsolutePath()
                + "; rerun without -Dlev.writeReference to check against it");
    }

    private static Map<String, List<String>> readReference() throws IOException {
        Map<String, List<String>> out = new LinkedHashMap<>();
        List<String> current = null;
        for (String line : resourceLines(REFERENCE)) {
            if (line.startsWith("== ")) {
                current = new ArrayList<>();
                out.put(line.substring(3).trim(), current);
            } else if (current != null && !line.isEmpty()) {
                current.add(line);
            }
        }
        return out;
    }

    /** Cases: a line "== <label> <sentence>", followed by one braced MC set. */
    static List<Case> cases() throws IOException {
        List<Case> out = new ArrayList<>();
        String label = null;
        String sentence = null;
        List<String> block = new ArrayList<>();
        for (String line : resourceLines(SUITE)) {
            if (line.startsWith("== ")) {
                if (label != null) {
                    out.add(new Case(label, sentence, block));
                }
                String[] parts = line.substring(3).split(" ", 2);
                label = parts[0];
                sentence = parts.length > 1 ? parts[1] : "";
                block = new ArrayList<>();
            } else if (label != null && !line.isBlank()) {
                block.add(line);
            }
        }
        if (label != null) {
            out.add(new Case(label, sentence, block));
        }
        return out;
    }

    private static List<String> resourceLines(String resource) throws IOException {
        try (InputStream in = LevBasicShortTest.class.getResourceAsStream(resource)) {
            if (in == null) {
                throw new IOException("missing test resource " + resource);
            }
            return Arrays.asList(new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\\R"));
        }
    }
}
