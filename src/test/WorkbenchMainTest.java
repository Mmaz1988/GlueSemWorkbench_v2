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
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class WorkbenchMainTest {

    private List<String> loadTestFormulas(String path) {
        List<String> lines = new LinkedList<>();
        try {
            lines = Files.readAllLines(Paths.get(path));
        } catch (IOException e) {
            e.printStackTrace();
        }

        return lines;
    }

    @Test
    /**
     * General testing method for manual mode. Loads and tests different kinds of files
     */
    void initiateManualMode() {
        List<String> lines = new LinkedList<>();
        List<LexicalEntry> lexicalEntries = new LinkedList<>();
        GlueParser parser = new GlueParser();

        // Test bad formulas file
        String bad_formulas = "C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\bad_formulas.txt";
        lines = loadTestFormulas(bad_formulas);
        try {
            LexicalEntry f1 = parser.parseMeaningConstructor(lines.get(0));
            assertEquals("/x.dog(x)",f1.getSem().toString());
            // Test glue side

            List<String> finalLines = lines;
            assertThrows(ParserInputException.class,() -> {parser.parseMeaningConstructor(finalLines.get(1));});
        } catch (ParserInputException e) {
            e.printStackTrace();
        }


        String intrans_quant = "C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\bad_formulas.txt";
        // Test intransitive with quantifier


    }
}