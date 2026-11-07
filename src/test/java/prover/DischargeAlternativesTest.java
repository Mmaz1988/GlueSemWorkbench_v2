package prover;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import glueSemantics.parser.SemanticParser;
import main.Settings;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import utilities.LexVariableHandler;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * A meaning that is a set of alternatives ({@code {a, b}}, parsed semantics) under a discharged
 * hypothesis: the discharge wraps each alternative in its own lambda, so after beta reduction the
 * alternatives are whole readings, a set of formulas. Both provers must do this. Hepple used to
 * wrap the set as a whole and left it inside the formula, as a conjunct (2026-11-07).
 */
class DischargeAlternativesTest {

    private static final List<String> EVERY_DOG_CHASED_A_CAT = List.of(
            "{",
            "[1] [/P_<e,t>.[/Q_<e,t>.Ax_e[P(x) -> Q(x)]]] : ((g_e -o g_t) -o ((g_e -o f_t) -o f_t))",
            "[2] [/x_e.dog(x)] : (g_e -o g_t)",
            "[3] [/P_<e,t>.[/Q_<e,t>.Ex_e[P(x) & Q(x)]]] : ((h_e -o h_t) -o ((h_e -o f_t) -o f_t))",
            "[4] [/x_e.cat(x)] : (h_e -o h_t)",
            "[5] [/x_e.[/y_e.{chase(x,y),follow(x,y)}]] : (g_e -o (h_e -o f_t))",
            "}");

    @ParameterizedTest(name = "prover {0}")
    @ValueSource(ints = {Settings.HEPPLE, Settings.LEV})
    void eachAlternativeBecomesAWholeReading(int proverType) throws Exception {
        assertEquals(List.of(
                        "{∀x2[dog(x2) → ∃x[cat(x) ∧ chase(x2,x)]], ∀x2[dog(x2) → ∃x[cat(x) ∧ follow(x2,x)]]}",
                        "{∃x[cat(x) ∧ ∀x2[dog(x2) → chase(x2,x)]], ∃x[cat(x) ∧ ∀x2[dog(x2) → follow(x2,x)]]}"),
                readings(proverType));
    }

    private static List<String> readings(int proverType) throws Exception {
        Settings staticSettings = SemanticParser.settings;
        try {
            LexVariableHandler.resetVars();
            Settings settings = new Settings();
            settings.setProverType(proverType);
            settings.setBetaReduce(true);
            SemanticParser.settings = settings;
            LexicalEntries entries = new GlueParser(true).parseMeaningConstructorList(EVERY_DOG_CHASED_A_CAT);
            LLProver prover = proverType == Settings.HEPPLE
                    ? new LLProver2(settings, new StringBuilder())
                    : new LLProver1(settings, new StringBuilder());
            List<String> out = new ArrayList<>();
            for (SolutionObject so : prover.searchProof(1, entries, true)) {
                out.add(so.solution.getSemTerm().toString());
            }
            Collections.sort(out);
            return out;
        } finally {
            SemanticParser.settings = staticSettings;
            LexVariableHandler.resetVars();
        }
    }
}
