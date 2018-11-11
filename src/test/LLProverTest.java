/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package test;

import glueSemantics.lexicon.LexicalEntry;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.ParserInputException;
import glueSemantics.parser.GlueParser;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

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
        LLProver lp = new LLProver(new Settings(false,Settings.PROLOG));
        // Test transitive sentence with quantifiers
        System.out.println("\nTesting sentence with transitive verb and quantifiers in Prolog mode:");
        Sequent transQuantPl = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\trans_quant_prolog.txt");
        List<Premise> transPlSolutions = null;
        try {
            transPlSolutions = lp.deduce(transQuantPl);
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
            LLProver lp = new LLProver(new Settings());

            // Test intransitive sentence with quantifier
            System.out.println("\nTesting sentence with intransitive verband quantifier:");
            Sequent intransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant.txt");
            List<Premise> solutions = lp.deduce(intransQuant);
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
            List<Premise> transSolutions = lp.deduce(transQuant);
            assertEquals(2,transSolutions.size());
            //assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).getPremiseIDs());
            System.out.println("Found the following deduction(s): ");
            for (Premise sol : transSolutions) {
                System.out.println(sol.toString());
            }

            // Test intransitve sentence with quantifier and adjective
            System.out.println("\nTesting sentence with intransitive verb, quantifier and adjective:");
            Sequent adjIntransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant_adj.txt");
            List<Premise> adjIntransSolutions = lp.deduce(adjIntransQuant);
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