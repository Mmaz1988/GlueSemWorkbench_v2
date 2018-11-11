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
import glueSemantics.synInterface.dependency.LexicalParserException;
import glueSemantics.synInterface.lfg.FStructureParser;
import main.Settings;
import main.WorkbenchMain;
import org.junit.jupiter.api.Test;
import prover.LLProver;
import prover.VariableBindingException;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;

import static main.WorkbenchMain.searchProof;
import static org.junit.jupiter.api.Assertions.*;

class WorkbenchMainTest {
    private GlueParser parser = new GlueParser();



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
    void testManualMode() {
        List<String> lines = new LinkedList<>();
        List<LexicalEntry> lexicalEntries = new LinkedList<>();

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


    @Test
    void testDependencyMode() {
        try {
            WorkbenchMain.initiateDependencyMode("Every man owns a black dog");
            WorkbenchMain.initiateDependencyMode("John owns a big black dog.");
        } catch (LexicalParserException e) {
            e.printStackTrace();
        }
    }

    @Test
    void testLFGMode() {
        Path p = Paths.get("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\every_black_dog_barks_webXLE.pl");
        try {
            LLProver prover = new LLProver(new Settings());
            searchProof(prover, new FStructureParser(p).getLexicalEntries());

            p = Paths.get("C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\john_cries_webXLE.pl");
            searchProof(prover, new FStructureParser(p).getLexicalEntries());

        } catch (VariableBindingException | LexicalParserException e) {
            e.printStackTrace();
        }
    }
}