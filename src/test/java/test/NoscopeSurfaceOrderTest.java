package test;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver1;
import prover.SolutionObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

// A noscope stage keeps the maximal histories (noScopeCompress). Two scope orders of the same
// premises have the same index set; of those the surface order stays, whichever was derived
// first. The meaning constructors are the lfgxdrt inference grammar's for "Every guest who did not
// bring a present left." in the parse with "not" and "a present" both scoping in the relative
// clause (f14): noscope [3] (the relative clause) is applied to both scopings, so only one comes
// out. It is "not" over "a present" (no present brought), not "a present" over "not".
class NoscopeSurfaceOrderTest {

    private static final List<String> PREMISES = List.of(
            "[1] (\\P.(\\Q.([],[([x],[])+P@x->Q@x]))) : ((f20_e -o f20_t) -o ((f20_e -o f22_t) -o f22_t))",
            "[2] (\\x.([],[guest(x)])) : (f20_e -o f20_t)",
            "[3] (\\P.(\\Q.(\\x.(P@x+Q@x)))) : ((f20_e -o f20_t) -o ((f12_e -o g16_t) -o (f20_e -o f20_t))) || noscope",
            "[4] (\\P.([],[~(P)])) : (f14_t -o f14_t)",
            "[5] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g15_v -o g15_t) -o (f10_e -o (g15_v -o g15_t))) || noscope",
            "[6] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g15_v -o g15_t) -o (f12_e -o (g15_v -o g15_t))) || noscope",
            "[7] (\\V.([e:v],[])+V@e) : ((g15_v -o g15_t) -o f14_t)",
            "[8] (\\v.([],[bring(v)])) : (g15_v -o g15_t)",
            "[9] (\\P.P) : (f14_t -o g16_t)",
            "[10] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f14_t) -o f14_t))",
            "[11] (\\x.([],[present(x)])) : (f10_e -o f10_t)",
            "[12] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g23_v -o g23_t) -o (f20_e -o (g23_v -o g23_t))) || noscope",
            "[13] (\\V.([e:v],[])+V@e) : ((g23_v -o g23_t) -o f22_t)",
            "[14] (\\v.([],[leave(v)])) : (g23_v -o g23_t)",
            "[15] (\\P.P) : (f22_t -o g24_t)");

    private static List<String> prove(List<String> premises) throws Exception {
        List<String> input = new ArrayList<>();
        input.add("{");
        input.addAll(premises);
        input.add("}");
        Settings settings = new Settings();
        LexicalEntries lexicalEntries = new GlueParser(settings).parseMeaningConstructorList(input);
        LLProver1 prover = new LLProver1(settings);
        List<String> readings = new ArrayList<>();
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            for (SolutionObject solution : prover.searchProof(key, lexicalEntries, true)) {
                readings.add(solution.solution.getSemTerm().toString());
            }
        }
        return readings;
    }

    private static void assertNotOverAPresent(List<String> readings) {
        assertEquals(1, readings.size(), readings.toString());
        String reading = readings.get(0);
        assertTrue(reading.indexOf("~") < reading.indexOf("present("), reading);
    }

    @Test
    void theSurfaceScopeOrderSurvivesTheNoscopeStage() throws Exception {
        assertNotOverAPresent(prove(PREMISES));
    }

    @Test
    void whicheverScopeOrderIsDerivedFirst() throws Exception {
        // The premises listed the other way round: the derivation order changes, the labels (and
        // so the surface order) do not.
        List<String> reversed = new ArrayList<>(PREMISES);
        Collections.reverse(reversed);
        assertNotOverAPresent(prove(reversed));
    }
}
