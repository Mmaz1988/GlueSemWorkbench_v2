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
    private LLProver lp = new LLProver();


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
    void deduce() {
        try {
            // Test intransitive sentence with quantifier
            Sequent intransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant.txt");
            List<Premise> solutions = lp.deduce(intransQuant);
            assertEquals(solutions.size(),1);
            assertEquals("/P./Q./x.every(x,P(x),Q(x))(λy_t./y.sleep(y)(y))(λx_t./x.dog(x)(x))",solutions.get(0).getSemTerm().toString());
            assertEquals("f",solutions.get(0).getGlueTerm().toString());
            assertEquals(new HashSet<>(Arrays.asList(0,1,2,3,4)),solutions.get(0).getPremiseIDs());

            Sequent adjIntransQuant = loadAndParseTestFormulas("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant_adj.txt");
            solutions = lp.deduce(adjIntransQuant);

        }
        catch (VariableBindingException | ProverException e) {
            e.printStackTrace();
        }
    }

}