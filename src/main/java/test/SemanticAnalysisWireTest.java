package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbOutput;
import webservice.rest.dtos.GswbPreferences;
import webservice.rest.dtos.GswbRequest;
import webservice.rest.dtos.GswbSemanticAnalysis;
import webservice.rest.dtos.GswbSemanticMergePart;
import webservice.rest.dtos.GswbSequenceMergeRequest;
import webservice.rest.dtos.GswbSolution;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * W1/W2 (xleplusglue docs/plans/SEMANTIC_ANALYSIS_WIRE_PLAN.md): a derived or merged reading
 * travels once, as its semanticAnalysis -- semString, graph, and svg (LFGxDRT display mode) or
 * prologRender (the other modes) -- and its flat semantic, graph and solution stay unset.
 */
class SemanticAnalysisWireTest {

    /** "Kim arrived", as LiGER's glue-basic-drt grammar derives it. */
    private static final String KIM_ARRIVED = "{\n"
            + "[1] (\\Q.(\\R.([x],[])+Q@x+R@x)) : ((f2_e -o f2_t) -o ((f2_e -o f3_t) -o f3_t)) || insitu\n"
            + "[2] (\\x.([],[x=Kim])) : (f2_e -o f2_t)\n"
            + "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g4_v -o g4_t) -o (f2_e -o (g4_v -o g4_t))) || noscope\n"
            + "[4] (\\Q.([e],[])+Q@e) : ((g4_v -o g4_t) -o f3_t)\n"
            + "[5] (\\e.([],[arrive(e)])) : (g4_v -o g4_t)\n"
            + "}";

    private static final int LFGXDRT = 5;
    private static final int PROLOG_DRT = 4;

    private static GswbOutput deduce(int outputstyle) throws Exception {
        GswbPreferences preferences = new GswbPreferences();
        preferences.outputstyle = outputstyle;
        preferences.betaReduce = true;
        preferences.resolveDrs = true;
        return new GswbController().glueDeduce(new GswbRequest(KIM_ARRIVED, preferences));
    }

    private static void assertNoFlatReading(GswbSolution solution) {
        assertNull(solution.semantic, "semantic");
        assertNull(solution.graph, "graph");
        assertNull(solution.solution, "solution");
    }

    private static void assertLfgxdrtReading(GswbSemanticAnalysis analysis) {
        assertNotNull(analysis);
        assertEquals("lfgxdrt", analysis.semType);
        assertFalse(analysis.semString == null || analysis.semString.isBlank(), "semString");
        assertNotNull(analysis.graph, "graph");
        assertTrue(analysis.svg != null && analysis.svg.contains("<svg"), "svg");
        assertNull(analysis.prologRender, "prologRender");
    }

    @Test
    void anLfgxdrtDeductionSendsEachReadingOnceAsItsSemanticAnalysis() throws Exception {
        GswbOutput output = deduce(LFGXDRT);
        assertFalse(output.solutions.isEmpty());
        for (GswbSolution solution : output.solutions) {
            assertNoFlatReading(solution);
            assertLfgxdrtReading(solution.semanticAnalysis);
            assertEquals(solution.id, solution.semanticAnalysis.semId);
        }
    }

    @Test
    void aPrologDrtDeductionSendsItsTextAsPrologRender() throws Exception {
        GswbOutput output = deduce(PROLOG_DRT);
        assertFalse(output.solutions.isEmpty());
        for (GswbSolution solution : output.solutions) {
            assertNoFlatReading(solution);
            GswbSemanticAnalysis analysis = solution.semanticAnalysis;
            assertEquals("prolog-drt", analysis.semType);
            assertNull(analysis.svg, "svg");
            assertFalse(analysis.prologRender == null || analysis.prologRender.isBlank(), "prologRender");
        }
    }

    private static GswbSemanticMergePart part(String id, String origin, GswbSemanticAnalysis reading) {
        GswbSemanticMergePart part = new GswbSemanticMergePart();
        part.id = id;
        part.solutionId = id;
        part.syntacticOrigin = origin;
        part.semantic = reading.semString;
        part.graph = reading.graph;
        return part;
    }

    @Test
    void aSequenceMergeSendsItsReadingOnceAsItsSemanticAnalysis() throws Exception {
        GswbSemanticAnalysis reading = deduce(LFGXDRT).solutions.get(0).semanticAnalysis;
        GswbSequenceMergeRequest request = new GswbSequenceMergeRequest();
        request.parts = List.of(part("sentence-1-s0", "S0", reading), part("sentence-2-s0", "S1", reading));

        GswbSolution merged = new GswbController().mergeSequenceSemantics(request);

        assertNoFlatReading(merged);
        assertLfgxdrtReading(merged.semanticAnalysis);
        assertEquals("sentence-1-s0+sentence-2-s0", merged.id);
        assertEquals(merged.id, merged.semanticAnalysis.semId);
        assertEquals("S1", merged.semanticAnalysis.syntacticOrigin);
        assertEquals(List.of(merged.id), merged.synSemMapping.get("S0+S1"));
    }

    @Test
    void aGraphsOnlySequenceMergeSendsItsReadingOnceAsItsSemanticAnalysis() throws Exception {
        GswbSemanticAnalysis reading = deduce(LFGXDRT).solutions.get(0).semanticAnalysis;
        GswbSequenceMergeRequest request = new GswbSequenceMergeRequest();
        request.graphs = List.of(reading.graph, reading.graph);
        request.parentSolutionId = "seq";
        request.solutionKey = "S0+S1";

        GswbSolution merged = new GswbController().mergeSequenceSemantics(request);

        assertNoFlatReading(merged);
        assertLfgxdrtReading(merged.semanticAnalysis);
        assertEquals("seq-drs-merge", merged.semanticAnalysis.semId);
        assertEquals("S0+S1", merged.semanticAnalysis.syntacticOrigin);
    }
}
