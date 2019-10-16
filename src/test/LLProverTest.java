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

import glueSemantics.lexicon.LexicalEntry;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver2;
import prover.ProverException;
import prover.VariableBindingException;

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

    void testProlog() {
        LLProver2 lp = new LLProver2(new Settings(false,Settings.PROLOG));
        // Test transitive sentence with quantifiers
        System.out.println("\nTesting sentence with transitive verb and quantifiers in Prolog mode:");
        Sequent transQuantPl = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\trans_quant_prolog.txt");
        List<Premise> transPlSolutions = null;
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
        for (Premise sol : transPlSolutions) {
            System.out.println(sol.toString());
        }

    }



    @Test
    void testPlain() {
        try {
            LLProver2 lp = new LLProver2(new Settings());

            // Test intransitive sentence with quantifier
            System.out.println("\nTesting sentence with intransitive verband quantifier:");
            Sequent intransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant.txt");
            lp.deduce(intransQuant);
            List<Premise> solutions = lp.getSolutions();
            assertEquals(1,solutions.size());
            assertEquals("/P./Q./x.every(x,P(x),Q(x))(λy_t./y.sleep(y)(y))(λx_t./x.dog(x)(x))",solutions.get(0).getSemTerm().toString());
            assertEquals("f",solutions.get(0).getGlueTerm().toString());
            assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).getPremiseIDs());

            System.out.println("Found the following deduction(s): ");
            for (Premise sol : solutions) {
                System.out.println(sol.toString());
            }

            // Test transitive sentence with quantifiers
            System.out.println("\nTesting sentence with transitive verb and quantifiers:");
            Sequent transQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\trans_quant.txt");
            lp.deduce(transQuant);
            List<Premise> transSolutions = lp.getSolutions();
            assertEquals(2,transSolutions.size());
            //assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).getPremiseIDs());
            System.out.println("Found the following deduction(s): ");
            for (Premise sol : transSolutions) {
                System.out.println(sol.toString());
            }

            // Test intransitve sentence with quantifier and adjective
            System.out.println("\nTesting sentence with intransitive verb, quantifier and adjective:");
            Sequent adjIntransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant_adj.txt");
            lp.deduce(adjIntransQuant);
            List<Premise> adjIntransSolutions = lp.getSolutions();
            assertEquals(1,adjIntransSolutions.size());
            System.out.println("Found the following deduction(s): ");
            for (Premise sol : adjIntransSolutions) {
                System.out.println(sol.toString());
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