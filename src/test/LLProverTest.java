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

import glueSemantics.linearLogic.LLTerm;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LinearLogicParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.LexicalEntry;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver2;
import prover.ProverException;
import prover.VariableBindingException;
import prover.categoryGraph.History;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LLProverTest {
    private GlueParser parser = new GlueParser();


    private Sequent loadAndParseTestFormulas(String path) {
        List<String> lines = new LinkedList<>();
        List<LexicalEntry> lexicalEntries = new LinkedList<>();
        try {
            lines = Files.readAllLines(Paths.get(path));
        } catch (IOException e) {
            e.printStackTrace();
        }
        for (String line : lines) {
            try {
                lexicalEntries.add(parser.parseMeaningConstructor(line));
            } catch (ParserInputException e) {
                e.printStackTrace();
            }
        }

        return new Sequent(lexicalEntries);
    }

    @Test
    void testLinearLogicParser() throws ParserInputException {
        String testFormula = "AX_t.AY_t.((X_t -o Y_t) -o (X_t -o Y_t))";
        LinearLogicParser lp = new LinearLogicParser();

        LLTerm result = lp.parse(testFormula);

        System.out.println(result.toPlainString());
    }




    @Test
    void testDependency() {

    }

}