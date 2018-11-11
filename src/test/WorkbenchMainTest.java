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

        String intrans_quant_adj = "C:\\Users\\User\\IdeaProjects\\glueSemWorkbench\\src\\test\\intrans_quant_adj.txt";
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