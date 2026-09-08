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

// "The PC-6082 is faster than the ITEL-XZ" with both proper names marked insitu, i.e. the same
// 14-MC fixture as ScopeDiscriminantSourceIndexTest: 4 LiGER comparative-rule MCs ([a1]/[a2]/
// [f4]/[f5], alphabetical labels and therefore no SYN-ID) precede the 10 real grammar MCs
// ([1]..[10]).
//
// Insitu premises are supposed to keep surface scope order, so exactly one proof should survive:
// [1] ('pc-6082', SYN-ID 1) outscoping [9] ('itel-xz', SYN-ID 9).
//
// Before the fix, LLProver1.insituViolationCount ranked scoping modifiers by their agenda
// position and compared insitu premises against *every* scoping modifier, including [a2] - a
// LiGER MC with no SYN-ID that is an impure X-o-X and so lands in scopingModifiers. [a2]'s agenda
// position (0) is lower than both quantifiers' and it is consumed late in every derivation, so it
// scored one unavoidable violation against each insitu premise. That floor of 2 made the strict
// pass reject both readings, and the relaxed retry then handed back both of them - insitu looked
// unenforced when it was in fact unsatisfiable.
class InsituScopeOrderingTest {

    private static final String PC_6082 =
            "[1] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f17_e -o f17_t) -o ((f17_e -o f19_t) -o f19_t))";
    private static final String ITEL_XZ =
            "[9] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f19_t) -o f19_t))";

    private static List<String> premises(String pc6082, String itelXz) {
        return Arrays.asList(
                "{",
                "[a2] (\\P.(\\d.(\\x.([],[fast(x,d)])))) : ((f11_v -o f11_t) -o (a1_d -o (f11_v -o f11_t)))",
                "[a1] (\\P.(\\Q.(\\e.([d:d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f11_v -o f11_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f11_v -o f11_t))) || noscope",
                "[f4] (\\d.(\\e.([],[fast(e,d)]))) : (a3_d -o (a4_v -o f4_t))",
                "[f5] (\\V.(\\x.(\\e.(V@e + ([],[arg1(e,x)]))))) : ((a4_v -o f4_t) -o (f10_e -o (a4_v -o f4_t))) || noscope",
                pc6082,
                "[2] (\\x.([],[x='pc-6082'])) : (f17_e -o f17_t)",
                "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g20_v -o g20_t) -o (f17_e -o (g20_v -o g20_t))) || noscope",
                "[4] (\\V.([e],[])+V@e) : ((g20_v -o g20_t) -o f19_t)",
                "[5] (\\v.([],[be(v)])) : (g20_v -o g20_t)",
                "[6] (\\P.P) : (f19_t -o g21_t)",
                "[7] (\\Q.(\\R.(\\x.(Q@x+R@x)))) : ((f11_v -o f11_t) -o ((g20_v -o g20_t) -o (g20_v -o g20_t))) || noscope",
                "[8] (\\x.([],[fast(x)])) : (f11_v -o f11_t)",
                itelXz,
                "[10] (\\x.([],[x='itel-xz'])) : (f10_e -o f10_t)",
                "}");
    }

    private List<SolutionObject> prove(List<String> premises)
            throws ParserInputException, VariableBindingException, ProverException {
        Settings settings = new Settings();
        GlueParser parser = new GlueParser(settings);
        LexicalEntries lexicalEntries = parser.parseMeaningConstructorList(premises);

        LLProver1 prover = new LLProver1(settings);
        List<SolutionObject> solutions = new ArrayList<>();
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            solutions.addAll(prover.searchProof(key, lexicalEntries, true));
        }
        return solutions;
    }

    // The 'pc-6082' quantifier binds its variable before the 'itel-xz' one exactly when it
    // outscopes it, so the earlier of the two constants in the solution string identifies the
    // wide-scope quantifier.
    private void assertSurfaceScopeOrder(SolutionObject solution) {
        String semantics = solution.solution.getSemTerm().toString();
        int pc6082 = semantics.indexOf("pc-6082");
        int itelXz = semantics.indexOf("itel-xz");
        assertTrue(pc6082 >= 0 && itelXz >= 0,
                "Expected both proper names in the solution, got: " + semantics);
        assertTrue(pc6082 < itelXz,
                "Insitu premises must keep surface scope order, so [1] ('pc-6082', SYN-ID 1) has "
                        + "to outscope [9] ('itel-xz', SYN-ID 9), but got: " + semantics);
    }

    @Test
    void twoInsituPremisesInOneSccKeepSurfaceScopeOrder()
            throws ParserInputException, VariableBindingException, ProverException {
        List<SolutionObject> solutions = prove(premises(PC_6082 + " || insitu", ITEL_XZ + " || insitu"));

        assertEquals(1, solutions.size(),
                "Both quantifiers are insitu and sit in the same SCC, so surface scope order should "
                        + "leave exactly one proof; more than one means the insitu filter was "
                        + "bypassed - most likely because the strict pass found nothing and the "
                        + "relaxed retry returned every reading.");
        assertSurfaceScopeOrder(solutions.get(0));
    }

    // A single insitu premise still has to respect the scoping modifiers around it. This is what
    // distinguishes ranking by surface position from merely comparing insitu premises against each
    // other: with only one of them marked, an insitu-only comparison has nothing to compare and
    // admits both readings.
    @Test
    void singleInsituPremiseCannotOutscopeASurfaceEarlierModifier()
            throws ParserInputException, VariableBindingException, ProverException {
        List<SolutionObject> solutions = prove(premises(PC_6082, ITEL_XZ + " || insitu"));

        assertEquals(1, solutions.size(),
                "[9] is insitu and [1] precedes it in the string, so the reading where [9] "
                        + "outscopes [1] has to be filtered out.");
        assertSurfaceScopeOrder(solutions.get(0));
    }

    // A LiGER-contributed MC has no SYN-ID and therefore no surface position to order by, so an
    // insitu marker on one cannot constrain anything. Parsing must still succeed (the prover just
    // ignores the marker) - GlueParser logs a warning for it.
    @Test
    void insituOnAPremiseWithoutASourceLabelIsIgnoredRatherThanFatal()
            throws ParserInputException, VariableBindingException, ProverException {
        List<String> premises = new ArrayList<>(premises(PC_6082 + " || insitu", ITEL_XZ + " || insitu"));
        premises.set(1, premises.get(1) + " || insitu");

        List<SolutionObject> solutions = prove(premises);

        assertEquals(1, solutions.size(),
                "An insitu marker on the unindexed [a2] carries no surface position and must be "
                        + "ignored, leaving the same single proof as without it.");
        assertSurfaceScopeOrder(solutions.get(0));
    }
}
