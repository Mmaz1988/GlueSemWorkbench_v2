package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbPreferences;
import webservice.rest.dtos.GswbRequest;
import webservice.rest.dtos.GswbSemanticAnalysis;
import webservice.rest.dtos.GswbSemanticMergePart;
import webservice.rest.dtos.GswbSequenceMergeRequest;
import webservice.rest.dtos.GswbSolution;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * xleplusglue docs/plans/DERIVE_ONCE_REBASE_PLAN.md, R3/R4: a sentence derived once on its own and
 * appended with its part's {@code sourceIndexOffset} merges to the reading its in-sequence
 * derivation gives. Fixture: "Kim saw a man with a telescope. He smiled." as LiGER's
 * lfgxdrt_inference_grammar gives it (first parse); in the sequence, "He smiled."'s meaning
 * constructors are its own with every index moved by 13.
 */
class SequenceMergeSourceIndexOffsetTest {

    private static final String KIM_SAW_A_MAN = ""
            + "{\n"
            + "//Grammar\n"
            + "[1] (\\x.([],[x='kim'])) : (f19_e -o f19_t)\n"
            + "[2] (\\P.(\\Q.p(([x],[bind(x)])+P@x)+Q@x)) : ((f19_e -o f19_t) -o ((f19_e -o f21_t) -o f21_t)) || insitu\n"
            + "[3] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g22_v -o g22_t) -o (f15_e -o (g22_v -o g22_t))) || noscope\n"
            + "[4] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g22_v -o g22_t) -o (f19_e -o (g22_v -o g22_t))) || noscope\n"
            + "[5] (\\V.([e:v],[])+V@e) : ((g22_v -o g22_t) -o f21_t)\n"
            + "[6] (\\v.([],[see(v)])) : (g22_v -o g22_t)\n"
            + "[7] (\\P.P) : (f21_t -o g23_t)\n"
            + "[8] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f15_e -o f15_t) -o ((f15_e -o f21_t) -o f21_t))\n"
            + "[9] (\\x.([],[man(x)])) : (f15_e -o f15_t)\n"
            + "[10] (\\U.(\\V.(\\e.(U@e+V@e)))) : ((f15_e -o f8_t) -o ((f15_e -o f15_t) -o (f15_e -o f15_t))) || noscope\n"
            + "[11] (\\y.(\\x.([],[with(x,y)]))) : (f7_e -o (f15_e -o f8_t))\n"
            + "[12] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f7_e -o f7_t) -o ((f7_e -o f21_t) -o f21_t))\n"
            + "[13] (\\x.([],[telescope(x)])) : (f7_e -o f7_t)\n"
            + "}\n";

    private static final String HE_SMILED = ""
            + "{\n"
            + "//Grammar\n"
            + "[1] (\\x.([],[ant(x),male(x)])) : (f2_e -o f2_t)\n"
            + "[2] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f2_e -o f2_t) -o ((f2_e -o f4_t) -o f4_t)) || insitu\n"
            + "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g5_v -o g5_t) -o (f2_e -o (g5_v -o g5_t))) || noscope\n"
            + "[4] (\\V.([e:v],[])+V@e) : ((g5_v -o g5_t) -o f4_t)\n"
            + "[5] (\\v.([],[smile(v)])) : (g5_v -o g5_t)\n"
            + "[6] (\\P.P) : (f4_t -o g6_t)\n"
            + "}\n";

    private static final String HE_SMILED_IN_SEQUENCE = ""
            + "{\n"
            + "//Grammar\n"
            + "[14] (\\x.([],[ant(x),male(x)])) : (f2_e -o f2_t)\n"
            + "[15] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f2_e -o f2_t) -o ((f2_e -o f4_t) -o f4_t)) || insitu\n"
            + "[16] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g5_v -o g5_t) -o (f2_e -o (g5_v -o g5_t))) || noscope\n"
            + "[17] (\\V.([e:v],[])+V@e) : ((g5_v -o g5_t) -o f4_t)\n"
            + "[18] (\\v.([],[smile(v)])) : (g5_v -o g5_t)\n"
            + "[19] (\\P.P) : (f4_t -o g6_t)\n"
            + "}\n";

    private static final int OFFSET = 13;

    private static GswbSemanticAnalysis reading(String meaningConstructors) throws Exception {
        GswbPreferences preferences = new GswbPreferences();
        preferences.outputstyle = 5;
        preferences.betaReduce = true;
        preferences.resolveDrs = true;
        List<GswbSolution> solutions = new GswbController()
                .glueDeduce(new GswbRequest(meaningConstructors, preferences)).solutions;
        assertEquals(1, solutions.size(), "readings");
        return solutions.get(0).semanticAnalysis;
    }

    private static GswbSemanticMergePart part(String id, GswbSemanticAnalysis reading, int offset) {
        GswbSemanticMergePart part = new GswbSemanticMergePart();
        part.id = id;
        part.syntacticOrigin = id;
        part.semantic = reading.semString;
        part.graph = reading.graph;
        part.sourceIndexOffset = offset;
        return part;
    }

    private static GswbSemanticAnalysis merge(GswbSemanticMergePart... parts) throws Exception {
        GswbSequenceMergeRequest request = new GswbSequenceMergeRequest();
        request.parts = List.of(parts);
        return new GswbController().mergeSequenceSemantics(request).semanticAnalysis;
    }

    @Test
    void aReadingDerivedOnItsOwnAndMovedByItsOffsetMergesLikeItsInSequenceDerivation() throws Exception {
        GswbSemanticAnalysis context = reading(KIM_SAW_A_MAN);
        GswbSemanticAnalysis inSequence = merge(part("s1", context, 0), part("s2", reading(HE_SMILED_IN_SEQUENCE), 0));
        GswbSemanticAnalysis rebased = merge(part("s1", context, 0), part("s2", reading(HE_SMILED), OFFSET));

        assertEquals(inSequence.graph, rebased.graph);
        assertEquals(inSequence.semString, rebased.semString);
    }

    @Test
    void offsetZeroMergesTheGraphAsSent() throws Exception {
        GswbSemanticAnalysis context = reading(KIM_SAW_A_MAN);
        GswbSemanticAnalysis own = reading(HE_SMILED);
        Object sentSourceIndex = own.graph.get("sourceIndex");
        GswbSemanticAnalysis inSequence = merge(part("s1", context, 0), part("s2", reading(HE_SMILED_IN_SEQUENCE), 0));
        GswbSemanticAnalysis unmoved = merge(part("s1", context, 0), part("s2", own, 0));
        merge(part("s1", context, 0), part("s2", own, OFFSET));

        // The negative control: without its offset the new sentence's indices are not the
        // sequence's, which is what the offset is for.
        assertNotEquals(inSequence.graph, unmoved.graph);
        assertEquals(sentSourceIndex, own.graph.get("sourceIndex"), "the request's graph is left as sent");
    }

    @Test
    void aPartWithAnOffsetButNoGraphIsRefused() throws Exception {
        GswbSemanticMergePart part = part("s2", reading(HE_SMILED), OFFSET);
        part.graph = null;
        assertThrows(IllegalArgumentException.class, () -> merge(part("s1", reading(KIM_SAW_A_MAN), 0), part));
    }
}
