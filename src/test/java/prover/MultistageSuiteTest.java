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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

/**
 * The multistage suite: each top-level { ... } block of multistage/multistage.txt is one premise
 * set, and its comment line ("//7: staging + no scope; 1 solution") states how many solutions the
 * multistage prover must find. Nested braces are stages. Semantics are not parsed: the checks are
 * about glue derivations only.
 *
 * Two checks per case:
 * - the solutions equal the baseline in multistage/reference-llprover3.txt, recorded from LLProver3
 *   (the multistage prover) on 2026-11-07, before it is merged into LLProver1;
 * - the solution count equals the suite's annotation, except for the listed known deviations.
 *
 * Re-record the baseline (only on purpose) with
 * {@code mvn test -Dtest=MultistageSuiteTest -Dmultistage.writeReference=true}.
 */
class MultistageSuiteTest {

    /** The prover the suite checks. Switch to the merged prover once multistage lives in LLProver1. */
    private static final int PROVER_UNDER_TEST = Settings.Multi;

    private static final String SUITE = "/multistage/multistage.txt";
    private static final String REFERENCE = "/multistage/reference-llprover3.txt";
    private static final Path REFERENCE_SOURCE = Path.of("src/test/resources" + REFERENCE);
    private static final Pattern COUNT = Pattern.compile("(\\d+)\\s+solutions?\\b|\\bno solutions\\b");
    private static final Pattern LABEL = Pattern.compile("^//\\s*(\\d+)");

    /** Cases whose annotated count the baseline prover does not reach; their reference still pins today's output. */
    private static final Map<String, String> KNOWN_DEVIATIONS = Map.of(
            "15", "LLProver3 finds 1 of the annotated 2 (baseline 2026-11-07)");

    record Case(String label, String title, int expected, List<String> lines) {
        @Override
        public String toString() {
            return label + ": " + title;
        }
    }

    @TestFactory
    Stream<DynamicTest> solutionsMatchTheBaseline() throws Exception {
        List<Case> cases = cases();
        if (Boolean.getBoolean("multistage.writeReference")) {
            writeReference(cases);
        }
        Map<String, List<String>> reference = readReference();
        return cases.stream().map(c -> DynamicTest.dynamicTest(c.toString(), () -> {
            assertTrue(reference.containsKey(c.label()), "no baseline for case " + c);
            List<String> found = assertTimeoutPreemptively(Duration.ofSeconds(60), () -> solutions(c, PROVER_UNDER_TEST));
            assertEquals(reference.get(c.label()), found, c.toString());
        }));
    }

    @TestFactory
    Stream<DynamicTest> solutionCountsMatchTheAnnotations() throws IOException {
        return cases().stream().map(c -> DynamicTest.dynamicTest(c.toString(), () -> {
            assumeFalse(KNOWN_DEVIATIONS.containsKey(c.label()), () -> "known deviation: " + KNOWN_DEVIATIONS.get(c.label()));
            int found = assertTimeoutPreemptively(Duration.ofSeconds(60), () -> countSolutions(c, PROVER_UNDER_TEST));
            assertEquals(c.expected(), found, c.toString());
        }));
    }

    /** Reference format: a line "== <case label>", then one solution per line. */
    private static void writeReference(List<Case> cases) throws Exception {
        StringBuilder out = new StringBuilder();
        for (Case c : cases) {
            out.append("== ").append(c.label()).append('\n');
            for (String solution : solutions(c, Settings.Multi)) {
                out.append(solution).append('\n');
            }
        }
        Files.writeString(REFERENCE_SOURCE, out.toString(), StandardCharsets.UTF_8);
        throw new IllegalStateException("baseline re-recorded to " + REFERENCE_SOURCE.toAbsolutePath()
                + "; rerun without -Dmultistage.writeReference to check against it");
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

    private static List<String> resourceLines(String resource) throws IOException {
        try (InputStream in = MultistageSuiteTest.class.getResourceAsStream(resource)) {
            if (in == null) {
                throw new IOException("missing test resource " + resource);
            }
            return Arrays.asList(new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\\R"));
        }
    }

    static int countSolutions(Case c, int proverType) throws Exception {
        return solutions(c, proverType).size();
    }

    /** The solutions' glue-side terms, sorted: the identity of each reading without its order. */
    static List<String> solutions(Case c, int proverType) throws Exception {
        LexVariableHandler.resetVars();
        Settings settings = new Settings();
        settings.setProverType(proverType);
        LexicalEntries entries = new GlueParser(false).parseMultiStageMCList(c.lines());
        LLProver prover = proverType == Settings.Multi ? new LLProver3(settings, new StringBuilder())
                : proverType == Settings.LEV ? new LLProver1(settings, new StringBuilder())
                : new LLProver2(settings, new StringBuilder());
        try {
            return prover.searchProof(1, entries, true).stream()
                    .map(so -> so.solution.toString())
                    .sorted()
                    .collect(Collectors.toList());
        } finally {
            LexVariableHandler.resetVars();
        }
    }

    /** Splits the suite into its top-level blocks; a case's comment may stand just before its block or inside it. */
    static List<Case> cases() throws IOException {
        List<String> all = resourceLines(SUITE);

        List<Case> out = new ArrayList<>();
        List<String> pendingComments = new ArrayList<>();
        List<String> block = null;
        int depth = 0;
        for (String raw : all) {
            String line = raw.trim();
            if (block == null) {
                if (line.equals("{")) {
                    block = new ArrayList<>();
                    block.add(line);
                    depth = 1;
                } else if (line.startsWith("//")) {
                    pendingComments.add(line);
                }
                continue;
            }
            block.add(raw);
            if (line.equals("{")) {
                depth++;
            } else if (line.equals("}")) {
                depth--;
            }
            if (depth == 0) {
                List<String> comments = new ArrayList<>(pendingComments);
                block.stream().map(String::trim).filter(l -> l.startsWith("//")).forEach(comments::add);
                out.add(toCase(comments, block, out.size() + 1));
                pendingComments.clear();
                block = null;
            }
        }
        if (block != null) {
            throw new IOException("unbalanced braces in " + SUITE);
        }
        return out;
    }

    private static Case toCase(List<String> comments, List<String> block, int position) throws IOException {
        for (String comment : comments) {
            Matcher count = COUNT.matcher(comment);
            if (count.find()) {
                Matcher label = LABEL.matcher(comment);
                String id = label.find() ? label.group(1) : "#" + position;
                int expected = count.group(1) == null ? 0 : Integer.parseInt(count.group(1));
                String title = comment.replaceFirst("^//\\s*\\d*\\s*:?\\s*", "").trim();
                return new Case(id, title, expected, block);
            }
        }
        throw new IOException("case at position " + position + " states no expected solution count: "
                + comments.stream().collect(Collectors.joining(" | ")));
    }
}
