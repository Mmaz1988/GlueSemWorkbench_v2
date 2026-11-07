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
import prover.VariableBindingException;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

// Reproduces "The PC-6082 is faster than the ITEL-XZ" through lfgxdrt_inference_grammar
// with degree_rules_lfgxdrt.liger's comparative rule active: 4 LiGER-rule MCs ([a1]/[a2]/[f4]/[f5],
// no real SYN-ID) precede the 10 real grammar MCs ([1]..[10]; [2] is x='pc-6082', [10] is
// x='itel-xz') - the same order GlueSemantics.returnMeaningConstructors emits them in
// (../liger/src/main/java/de/ukon/liger/semantics/GlueSemantics.java:196-229, "//Liger" MCs
// before "//Grammar" MCs), and the same 14-MC fixture GlueParserSourceIndexTest already
// verifies parses cleanly.
//
// Before the fix, LLProver1.toSyntheticSourceIndices treated each premise's position in its
// internal agenda list as "SYN-ID - 1", which only holds when no LiGER-rule MC precedes a
// grammar MC. With 4 such MCs prepended here, every grammar MC's agenda position is offset by
// 4 from its real SYN-ID, so scope discriminants built from those positions point at the wrong
// (or out-of-range) f-structure nodes - exactly why the scope discriminant for the two proper
// names' relative scope resolved to a surfaceLabel matching neither "PC-6082" nor "ITEL-XZ".
class ScopeDiscriminantSourceIndexTest {

    private static final String PREMISES = String.join("\n",
            "{",
            "[a1] (\\P.(\\Q.(\\e.([d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f11_v -o f11_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f11_v -o f11_t))) || noscope",
            "[a2] (\\P.(\\d.(\\x.([],[fast(x,d)])))) : ((f11_v -o f11_t) -o (a1_d -o (f11_v -o f11_t)))",
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
            "}"
    );

    @Test
    void scopeSourceIndexGroupsStayWithinRealSynIdRange()
            throws ParserInputException, VariableBindingException, ProverException {
        Settings settings = new Settings();
        GlueParser parser = new GlueParser(settings);
        LexicalEntries lexicalEntries = parser.parseMeaningConstructorList(Arrays.asList(PREMISES.split("\n")));

        LLProver1 prover = new LLProver1(settings);
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            prover.searchProof(key, lexicalEntries, true);
        }

        assertFalse(prover.scope2SourceIndexGroups.isEmpty(),
                "Expected at least one scope discriminant to be recorded for this ambiguous sentence");

        for (Map.Entry<String, List<LinkedHashSet<Integer>>> entry : prover.scope2SourceIndexGroups.entrySet()) {
            for (LinkedHashSet<Integer> group : entry.getValue()) {
                for (Integer sourceIndex : group) {
                    assertTrue(sourceIndex >= 1 && sourceIndex <= 10,
                            "Scope discriminant '" + entry.getKey() + "' recorded out-of-range source index "
                                    + sourceIndex + " in group " + group + " - real grammar SYN-IDs only run "
                                    + "1..10 here; anything else means a LiGER-rule MC's agenda position leaked "
                                    + "in as if it were a real source index.");
                }
            }
        }
    }
}
