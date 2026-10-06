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

// "The PC-6082 is faster than every ITEL computer." through lfgxdrt_inference_grammar with the
// degree rules (MCs copied from a stored regression session, S0). Its scope discriminants were
// labelled "every computer > The is faster every" and "The PC-6082 > The is faster every".
class ScopeLabelSourceGroupTest {

    static final String PREMISES = String.join("\n",
            "{",
            "[f7] (\\P.(\\x.([],[kind('itel',x)]) + P@x)) : ((f11_e -o f11_t) -o (f11_e -o f11_t)) || noscope",
            "[a2] (\\P.(\\d.(\\x.([],[fast(x,d)])))) : ((f12_v -o f12_t) -o (a1_d -o (f12_v -o f12_t)))",
            "[a1] (\\P.(\\Q.(\\e.([d:d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f12_v -o f12_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f12_v -o f12_t))) || noscope",
            "[f4] (\\d.(\\e.([],[fast(e,d)]))) : (a3_d -o (a4_v -o f4_t))",
            "[f5] (\\V.(\\x.(\\e.(V@e + ([],[arg1(e,x)]))))) : ((a4_v -o f4_t) -o (f11_e -o (a4_v -o f4_t))) || noscope",
            "[1] (\\Q.(\\R.([x],[])+Q@x+R@x)) : ((f18_e -o f18_t) -o ((f18_e -o f20_t) -o f20_t)) || insitu",
            "[2] (\\x.([],[x='pc-6082'])) : (f18_e -o f18_t)",
            "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g21_v -o g21_t) -o (f18_e -o (g21_v -o g21_t))) || noscope",
            "[4] (\\V.([e],[])+V@e) : ((g21_v -o g21_t) -o f20_t)",
            "[5] (\\v.([],[be(v)])) : (g21_v -o g21_t)",
            "[6] (\\P.P) : (f20_t -o g22_t)",
            "[7] (\\Q.(\\R.(\\x.(Q@x+R@x)))) : ((f12_v -o f12_t) -o ((g21_v -o g21_t) -o (g21_v -o g21_t))) || noscope",
            "[8] (\\x.([],[fast(x)])) : (f12_v -o f12_t)",
            "[9] (\\P.(\\Q.([],[([x],[])+P@x->Q@x]))) : ((f11_e -o f11_t) -o ((f11_e -o f20_t) -o f20_t))",
            "[10] (\\x.([],[computer(x)])) : (f11_e -o f11_t)",
            "}"
    );

    static LLProver1 prove() throws ParserInputException, VariableBindingException, ProverException {
        Settings settings = new Settings();
        GlueParser parser = new GlueParser(settings);
        LexicalEntries lexicalEntries = parser.parseMeaningConstructorList(Arrays.asList(PREMISES.split("\n")));
        LLProver1 prover = new LLProver1(settings);
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            prover.searchProof(key, lexicalEntries, true);
        }
        return prover;
    }

    /** The degree predicate's group must hold its own words, not the determiners of the two
     *  quantifiers whose hypothesised variables it consumed ([1] "The", [9] "every"). */
    @Test
    void predicateGroupsHoldNoQuantifierSynIds() throws Exception {
        LLProver1 prover = prove();
        assertFalse(prover.scope2SourceIndexGroups.isEmpty());
        boolean sawPredicate = false;
        for (Map.Entry<String, List<LinkedHashSet<Integer>>> entry : prover.scope2SourceIndexGroups.entrySet()) {
            if (!entry.getKey().contains("a1_d")) {
                continue;
            }
            sawPredicate = true;
            LinkedHashSet<Integer> predicate = entry.getValue().get(1);
            assertFalse(predicate.contains(1) || predicate.contains(9),
                    "'" + entry.getKey() + "' labels the predicate with a quantifier's SYN-ID: " + predicate);
            assertTrue(predicate.contains(8), "the predicate's own word ([8] fast) stays: " + predicate);
        }
        assertTrue(sawPredicate, "expected a scope discriminant between a quantifier and the degree predicate");
    }
}
