package main;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import org.junit.jupiter.api.Test;
import prover.LLProver2;
import utilities.LexVariableHandler;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Hepple records the compiled premises for the CLI's fail explanation in a static buffer. It
 * records only when an explanation was asked for (explainFail, -proveGoal), and each deduction
 * starts the record afresh: the buffer used to grow with every web request and mix earlier proofs
 * into later explanations (2026-11-09).
 */
class FailExplainerRecordTest {

    private static final List<String> FIRST = List.of("{", "dog : (g_e -o g_t)", "rex : g_e", "}");
    private static final List<String> SECOND = List.of("{", "cat : (h_e -o h_t)", "tom : h_e", "}");

    @Test
    void nothingIsRecordedWhenNoExplanationIsAskedFor() throws Exception {
        failExplainer.reset();
        deduce(FIRST, new Settings());
        assertEquals("", failExplainer.recorded());
    }

    @Test
    void eachDeductionStartsTheRecordAfresh() throws Exception {
        Settings settings = new Settings();
        settings.setExplainFail(true);
        deduce(FIRST, settings);
        assertTrue(failExplainer.recorded().contains("g_e"));
        deduce(SECOND, settings);
        assertTrue(failExplainer.recorded().contains("h_e"));
        assertFalse(failExplainer.recorded().contains("g_e"), failExplainer.recorded());
    }

    @Test
    void aGoalSearchAsksForAnExplanation() {
        Settings settings = new Settings();
        assertFalse(settings.isExplanationRequested());
        settings.setGoalSearch(true);
        assertTrue(settings.isExplanationRequested());
    }

    private static void deduce(List<String> mcs, Settings settings) throws Exception {
        LexVariableHandler.resetVars();
        settings.setProverType(Settings.HEPPLE);
        LexicalEntries entries = new GlueParser(false).parseMeaningConstructorList(mcs);
        new LLProver2(settings, new StringBuilder()).searchProof(1, entries, true);
        LexVariableHandler.resetVars();
    }
}
