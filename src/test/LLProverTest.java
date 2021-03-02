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
    void testProlog() {
        LLProver2 lp = new LLProver2(new Settings(false,Settings.PROLOG));
        // Test transitive sentence with quantifiers
        System.out.println("\nTesting sentence with transitive verb and quantifiers in Prolog mode:");
        Sequent transQuantPl = loadAndParseTestFormulas("/Users/red_queen/IdeaProjects/glueSemWorkbench2/src/test/trans_quant_prolog.txt");
        List<History> transPlSolutions = null;
        try {
            lp.deduce(transQuantPl);
            transPlSolutions = lp.getSolutions();
        } catch (ProverException e) {
            e.printStackTrace();
        } catch (VariableBindingException e) {
            e.printStackTrace();
        }
        assertEquals(2,transPlSolutions.size());
        System.out.println("Found the following deduction(s): ");
        for (History sol : transPlSolutions) {
            System.out.println(sol.premise.toString());
        }

    }



    @Test
    void testPlain() {
        try {
            LLProver2 lp = new LLProver2(new Settings());

            // Test intransitive sentence with quantifier
            System.out.println("\nTesting sentence with intransitive verband quantifier:");
            Sequent intransQuant = loadAndParseTestFormulas("/Users/red_queen/IdeaProjects/glueSemWorkbench2/src/test/intrans_quant.txt");
            lp.deduce(intransQuant);
            List<History> solutions = lp.getSolutions();
            assertEquals(1,solutions.size());
            assertEquals("/P./Q./x.every(x,P(x),Q(x))([λx_e./x.dog(x)(x)])([λy_e./y.sleep(y)(y)])",solutions.get(0).premise.getSemTerm().toString());
            assertEquals("f",solutions.get(0).premise.getGlueTerm().toString());
            assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).premise.getPremiseIDs());

            System.out.println("Found the following deduction(s): ");
            for (History sol : solutions) {
                System.out.println(sol.premise.toString());
            }

            lp = new LLProver2(new Settings());

            // Test transitive sentence with quantifiers
            System.out.println("\nTesting sentence with transitive verb and quantifiers:");
            Sequent transQuant = loadAndParseTestFormulas("/Users/red_queen/IdeaProjects/glueSemWorkbench2/src/test/trans_quant.txt");
            lp.deduce(transQuant);
            List<History> transSolutions = lp.getSolutions();
            assertEquals(2,transSolutions.size());
            //assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).getPremiseIDs());
            System.out.println("Found the following deduction(s): ");
            for (History sol : transSolutions) {
                System.out.println(sol.premise.toString());
            }

            lp = new LLProver2(new Settings());

            // Test intransitve sentence with quantifier and adjective
            System.out.println("\nTesting sentence with intransitive verb, quantifier and adjective:");
            Sequent adjIntransQuant = loadAndParseTestFormulas("/Users/red_queen/IdeaProjects/glueSemWorkbench2/src/test/intrans_quant_adj.txt");
            lp.deduce(adjIntransQuant);
            List<History> adjIntransSolutions = lp.getSolutions();
            assertEquals(1,adjIntransSolutions.size());
            System.out.println("Found the following deduction(s): ");
            for (History sol : adjIntransSolutions) {
                System.out.println(sol.premise.toString());
            }

        }
        catch (VariableBindingException | ProverException e) {
            e.printStackTrace();
        }
    }

    @Test
    void testDependency() {

    }

}