/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package test;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import glueSemantics.parser.ParserInputException;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver1;
import prover.ProverException;
import prover.SolutionObject;
import prover.VariableBindingException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

// Default solution order: by how far a reading's scope order departs from the surface order.
// Unlike insitu, this filters nothing -- both readings of the two proper names survive -- it only
// decides which one comes first. Same 14-MC fixture as InsituScopeOrderingTest, no insitu marks.
class SurfaceScopeOrderingTest {

    private static final List<String> PREMISES = Arrays.asList(
            "{",
            "[a2] (\\P.(\\d.(\\x.([],[fast(x,d)])))) : ((f11_v -o f11_t) -o (a1_d -o (f11_v -o f11_t)))",
            "[a1] (\\P.(\\Q.(\\e.([d:d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f11_v -o f11_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f11_v -o f11_t))) || noscope",
            "[f4] (\\d.(\\e.([],[fast(e,d)]))) : (a3_d -o (a4_v -o f4_t))",
            "[f5] (\\V.(\\x.(\\e.(V@e + ([],[arg1(e,x)]))))) : ((a4_v -o f4_t) -o (f10_e -o (a4_v -o f4_t))) || noscope",
            "[1] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f17_e -o f17_t) -o ((f17_e -o f19_t) -o f19_t))",
            "[2] (\\x.([],[x='pc-6082'])) : (f17_e -o f17_t)",
            "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g20_v -o g20_t) -o (f17_e -o (g20_v -o g20_t))) || noscope",
            "[4] (\\V.([e],[])+V@e) : ((g20_v -o g20_t) -o f19_t)",
            "[5] (\\v.([],[be(v)])) : (g20_v -o g20_t)",
            "[6] (\\P.P) : (f19_t -o g21_t)",
            "[7] (\\Q.(\\R.(\\x.(Q@x+R@x)))) : ((f11_v -o f11_t) -o ((g20_v -o g20_t) -o (g20_v -o g20_t))) || noscope",
            "[8] (\\x.([],[fast(x)])) : (f11_v -o f11_t)",
            "[9] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f19_t) -o f19_t))",
            "[10] (\\x.([],[x='itel-xz'])) : (f10_e -o f10_t)",
            "}");

    private static List<SolutionObject> prove() throws ParserInputException, VariableBindingException, ProverException {
        Settings settings = new Settings();
        LexicalEntries lexicalEntries = new GlueParser(settings).parseMeaningConstructorList(PREMISES);
        LLProver1 prover = new LLProver1(settings);
        List<SolutionObject> solutions = new ArrayList<>();
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            solutions.addAll(prover.searchProof(key, lexicalEntries, true));
        }
        return solutions;
    }

    private static boolean surfaceScope(SolutionObject solution) {
        String semantics = solution.solution.getSemTerm().toString();
        return semantics.indexOf("pc-6082") < semantics.indexOf("itel-xz");
    }

    @Test
    void theSurfaceScopeReadingComesFirst() throws Exception {
        List<SolutionObject> solutions = prove();

        assertEquals(2, solutions.size(), "without insitu both scopings survive");
        assertTrue(surfaceScope(solutions.get(0)),
                "[1] precedes [9] in the string, so the reading where it outscopes [9] comes first: "
                        + solutions.get(0).solution.getSemTerm());
        assertEquals(0, solutions.get(0).surfaceOrderViolations);
        assertTrue(solutions.get(1).surfaceOrderViolations > 0, "the inverse scoping counts a violation");
        assertEquals(0, solutions.get(0).insituViolations, "a strict proof has no insitu violations");
    }

    /** Predictable, not fully deterministic: whatever order equally ranked readings come in,
     *  no reading comes after one with more violations. */
    @Test
    void readingsAreOrderedByTheirViolationCounts() throws Exception {
        List<SolutionObject> solutions = prove();
        for (int i = 1; i < solutions.size(); i++) {
            SolutionObject before = solutions.get(i - 1);
            SolutionObject after = solutions.get(i);
            assertTrue(before.insituViolations < after.insituViolations
                    || (before.insituViolations == after.insituViolations
                        && before.surfaceOrderViolations <= after.surfaceOrderViolations),
                    "reading " + i + " ranks ahead of reading " + (i - 1));
        }
    }
}
