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
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.MeaningConstructor;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

// Reproduces (and now guards against a regression of)
// xleplusglue/docs/bug_reports/liger_rule_mc_indices_leak_into_meaning.md against
// GlueParser.parseMeaningConstructor directly. GlueParser.SOURCE_INDEX_PREFIX
// (GlueParser.java:45) used to only match purely-numeric bracket labels
// ("^\s*\[(\d+)\]\s*(.*)$"). LiGER-rule-added MCs can carry non-numeric labels (e.g.
// [a2], [f4]) that regex never matched; when that happened the bracketed label was
// never stripped and survived as literal text inside the parsed meaning
// (GlueParser.java:78-84,150-154). Fixed: the regex now matches any bracket content,
// stripping it from the meaning unconditionally and only populating the source index
// when the label is actually numeric.
class GlueParserSourceIndexTest {
    private GlueParser parser = new GlueParser();

    // Grammar MCs: numeric labels, expected to parse and strip cleanly today.

    @Test
    void testGrammarMc1SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[1] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f17_e -o f17_t) -o ((f17_e -o f19_t) -o f19_t))");
        assertEquals(1, entry.getSourceIndex());
        assertEquals("(\\P.(\\Q.([x],[])+P@x+Q@x))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc2SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[2] (\\x.([],[x='pc-6082'])) : (f17_e -o f17_t)");
        assertEquals(2, entry.getSourceIndex());
        assertEquals("(\\x.([],[x='pc-6082']))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc3SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[3] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g20_v -o g20_t) -o (f17_e -o (g20_v -o g20_t))) || noscope");
        assertEquals(3, entry.getSourceIndex());
        assertEquals("(\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)])))))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc4SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[4] (\\V.([e],[])+V@e) : ((g20_v -o g20_t) -o f19_t)");
        assertEquals(4, entry.getSourceIndex());
        assertEquals("(\\V.([e],[])+V@e)", entry.getSem().toString());
    }

    @Test
    void testGrammarMc5SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[5] (\\v.([],[be(v)])) : (g20_v -o g20_t)");
        assertEquals(5, entry.getSourceIndex());
        assertEquals("(\\v.([],[be(v)]))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc6SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[6] (\\P.P) : (f19_t -o g21_t)");
        assertEquals(6, entry.getSourceIndex());
        assertEquals("(\\P.P)", entry.getSem().toString());
    }

    @Test
    void testGrammarMc7SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[7] (\\Q.(\\R.(\\x.(Q@x+R@x)))) : ((f11_v -o f11_t) -o ((g20_v -o g20_t) -o (g20_v -o g20_t))) || noscope");
        assertEquals(7, entry.getSourceIndex());
        assertEquals("(\\Q.(\\R.(\\x.(Q@x+R@x))))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc8SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[8] (\\x.([],[fast(x)])) : (f11_v -o f11_t)");
        assertEquals(8, entry.getSourceIndex());
        assertEquals("(\\x.([],[fast(x)]))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc9SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[9] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f19_t) -o f19_t))");
        assertEquals(9, entry.getSourceIndex());
        assertEquals("(\\P.(\\Q.([x],[])+P@x+Q@x))", entry.getSem().toString());
    }

    @Test
    void testGrammarMc10SourceIndex() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[10] (\\x.([],[x='itel-xz'])) : (f10_e -o f10_t)");
        assertEquals(10, entry.getSourceIndex());
        assertEquals("(\\x.([],[x='itel-xz']))", entry.getSem().toString());
    }

    // LiGER-rule MCs: non-numeric labels. The bracketed label is stripped from the
    // meaning regardless of its content (getSourceIndex() is null, same as if no
    // label were present at all - MeaningConstructor.sourceIndex is typed Integer,
    // so a non-numeric label like "a2" is dropped rather than preserved; a separate,
    // still-open design question). Inputs below reflect the now-fixed LiGER pipeline:
    // Rule.splitGoal() no longer strips the "\" from lambda binders (was stripping
    // it unconditionally before ANY character; see liger's Rule.java), and
    // degree_rules_lfgxdrt.liger's two comparative-degree rules now use "~" instead
    // of the mistaken Prolog-notation "not" for negation (liger_resources/rules/degree_rules_lfgxdrt.liger).

    @Test
    void testLigerMcA2IndexLeak() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[a2] (\\P.(\\d.(\\x.([],[fast(x,d)])))) : ((f11_v -o f11_t) -o (a1_d -o (f11_v -o f11_t)))");
        assertEquals("(\\P.(\\d.(\\x.([],[fast(x,d)]))))", entry.getSem().toString());
        assertEquals(null, entry.getSourceIndex());
    }

    @Test
    void testLigerMcA1IndexLeak() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[a1] (\\P.(\\Q.(\\e.([d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f11_v -o f11_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f11_v -o f11_t))) || noscope");
        assertEquals("(\\P.(\\Q.(\\e.([d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)]))))", entry.getSem().toString());
        assertEquals(null, entry.getSourceIndex());
    }

    @Test
    void testLigerMcF4IndexLeak() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[f4] (\\d.(\\e.([],[fast(e,d)]))) : (a3_d -o (a4_v -o f4_t))");
        assertEquals("(\\d.(\\e.([],[fast(e,d)])))", entry.getSem().toString());
        assertEquals(null, entry.getSourceIndex());
    }

    @Test
    void testLigerMcF5IndexLeak() throws ParserInputException {
        MeaningConstructor entry = parser.parseMeaningConstructor(
                "[f5] (\\V.(\\x.(\\e.(V@e + ([],[arg1(e,x)]))))) : ((a4_v -o f4_t) -o (f10_e -o (a4_v -o f4_t))) || noscope");
        assertEquals("(\\V.(\\x.(\\e.(V@e + ([],[arg1(e,x)])))))", entry.getSem().toString());
        assertEquals(null, entry.getSourceIndex());
    }
}
