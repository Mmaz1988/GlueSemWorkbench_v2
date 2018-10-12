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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class LLProverTest {
    private String[] input1 = {
            "/x.dog(x) : (g_e -o g_t)",
            "/P./Q./x.every(x,P(x),Q(x)) : ((g_e -o g_t) -o AX_t.(h_e -o X_t) -o X_t)",
            "/y.sleep(y) : (h_e -o f_t)"
    };
    private GlueParser parser = new GlueParser();
    private Sequent seq;
    private LLProver lp = new LLProver();

    //private final SemanticRepresentation sem1;
    //private Premise solution1 = new Premise();


    LLProverTest() {
        List<LexicalEntry> lexEntries = new ArrayList<>();
        for (String f : input1) {
            try {
                lexEntries.add(parser.parseMeaningConstructor(f));
            } catch (ParserInputException e) {
                e.printStackTrace();
            }
        }
        this.seq = new Sequent(lexEntries);
    }

    @Test
    void deduce() {
        try {
            List<Premise> solutions = lp.deduce(seq);
            assertEquals(solutions.size(),1);
            assertEquals(solutions.get(0).getSemTerm().toString(),"/P./Q./x.every(x,P(x),Q(x)) (λy_t./y.sleep(y) (y))(λx_t./x.dog(x) (x))");
            assertEquals(solutions.get(0).getGlueTerm().toString(),"f");
            assertEquals(solutions.get(0).getPremiseIDs(),new HashSet<>(Arrays.asList(0,1,2,3,4)));
        }
        catch (VariableBindingException | ProverException e) {
            e.printStackTrace();
        }
    }

}