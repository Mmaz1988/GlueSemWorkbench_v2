package main;/*
 * Copyright 2019 Mark-Matthias Zymla & Moritz Messmer
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

import glueSemantics.lexicon.LexicalEntry;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.semantics.lambda.SemanticExpression;
import glueSemantics.synInterface.dependency.LexicalParserException;
import glueSemantics.synInterface.dependency.SentenceMeaning;
import glueSemantics.synInterface.lfg.FStructureParser;
import prover.LLProver2;
import prover.ProverException;
import prover.VariableBindingException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;


public class WorkbenchMain {
    // Initialize with default settings
    public static Settings settings = new Settings();
    public static List<String> solutions = new ArrayList<>();
    public static List<String> partial = new ArrayList<>();

    public static void main(String[] args) {
        settings = new Settings();
        System.out.println("The Glue Semantics Workbench\n"+
                            "copyright 2018 Moritz Messmer & Mark-Matthias Zymla\n");

        // Check program arguments for prover settings
        for (String arg : args) {
            switch (arg) {
                case ("-prolog"):
                    settings.setSemanticOutputStyle(Settings.PROLOG);
                    break;
                case ("-noreduce"):
                    settings.setBetaReduce(false);
                    break;
                case ("-debugging"):
                    settings.setDebugging(true);
                    break;
                case ("-p"):
                    settings.setPartial(true);

            }
        }

        String betaReduce = "on", outputMode = "plain";
        if (!settings.isBetaReduce())
            betaReduce = "off";

        if (settings.getSemanticOutputStyle() == 1)
            outputMode = "prolog";

        System.out.println(String.format("Current settings: automatic beta reduction: %s\t\toutput mode: %s", betaReduce, outputMode));

        // Check program parameters for a mode setting
        if (args.length > 0 && args[0].equals("-lfg")) {
            try {
                initiateLFGMode();
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }
        else if (args.length > 0 && args[0].equals("-dp")){
            try {
                initiateDependencyMode();
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }

        else if (args.length > 0 && args[0].equals("-i")){
            try {

                File inFile = new File(args[1]);

                    if (inFile.exists())
                    {
                        List<String> lines = null;
                        try {
                            lines = Files.readAllLines(inFile.toPath());
                        } catch (IOException e) {
                            throw new LexicalParserException("Error while trying to open file '"
                                    + inFile + "'");
                        }

                        initiateManualMode(lines);

                        if (args[2].equals("-o")){
                            try{
                                File outFile = new File(args[3]);

                                if (outFile.exists())
                                {
                                    outFile.delete();
                                    outFile.createNewFile();
                                }
                                else
                                {
                                    outFile.createNewFile();
                                }

                                if (outFile.exists()) {
                                    BufferedWriter w = new BufferedWriter(new FileWriter(outFile,true));
                                    for (String solution : solutions) {
                                        w.append(solution);
                                        w.append(System.lineSeparator());
                                    }

                                    if (settings.isPartial())
                                    {
                                        w.append("The following partial solutions were found:");
                                        w.append(System.lineSeparator());

                                        for (String partialSol : partial) {
                                            w.append(partialSol);
                                            w.append(System.lineSeparator());
                                        }

                                    }

                                w.close();
                                }

                                System.out.println("Wrote solutions to " + outFile.toString());


                            } catch(Exception e)
                            {
                                System.out.println("Error while generating output file. Maybe no valid path was given.");
                            }
                        }
                    }
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }
        else {
            try {
                initiateManualMode();
            }
            catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }
    }

    public static void initiateLFGMode() throws VariableBindingException, LexicalParserException {
            System.out.println("Starting LFG mode...\n");
            File f = null;
            final JFileChooser fc = new JFileChooser();
            fc.setDialogTitle("Choose an f-structure file");
            fc.addChoosableFileFilter(
                    new FileNameExtensionFilter("Prolog f-structure files", "pl"));
            int returnVal = fc.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                System.out.println("Selected file " + fc.getSelectedFile().getName());
                f = fc.getSelectedFile();
            } else {
                System.out.println("No file selected");

            }
            Path p;
            if (f != null) {
                p = FileSystems.getDefault().getPath(f.getAbsolutePath());

                searchProof(new FStructureParser(p).getLexicalEntries());

            }
            else
                System.out.println("No file selected");
    }


    public static void initiateDependencyMode() throws VariableBindingException, LexicalParserException {
        System.out.println("Starting interactive dependency mode...\n");
        Scanner s = new Scanner(System.in);
        String input;
        while (true) {
            System.out.println("Enter sentence to be analyzed or enter 'quit' to exit the program.");
            input = s.nextLine();
            if (input.equals("quit"))
                break;
            try {
                searchProof(new SentenceMeaning(input).getLexicalEntries());
            }
            catch (NoClassDefFoundError e) {
                System.out.println("Could not initialize dependency parser. Please refer to the README for more information");
                return;
            }
        }
    }

    public static void initiateManualMode() throws LexicalParserException, VariableBindingException {
        System.out.println("Starting manual entry mode...\n");
        File f = null;
        final JFileChooser fc = new JFileChooser();
        fc.setDialogTitle("Choose a file containing lexical entries");
        fc.addChoosableFileFilter(
                new FileNameExtensionFilter("Text files", "txt"));
        int returnVal = fc.showOpenDialog(null);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("Selected file " + fc.getSelectedFile().getName());
            f = fc.getSelectedFile();
        } else {
            System.out.println("No file selected");
        }
        Path p;
        if (f != null) {
            p = FileSystems.getDefault().getPath(f.getAbsolutePath());
            List<String> lines = null;

            try {
                lines = Files.readAllLines(p);
            } catch (IOException e) {
                throw new LexicalParserException("Error while trying to open file '"
                        + p + "'");
            }

            initiateManualMode(lines);
        }
        else
            System.out.println("No file selected");
    }

    public static void initiateManualMode(List<String> formulas) throws LexicalParserException, VariableBindingException {
        List<LexicalEntry> lexicalEntries = new LinkedList<>();
        GlueParser parser = new GlueParser();
        for (String s : formulas) {
            try {
                lexicalEntries.add(parser.parseMeaningConstructor(s));
            } catch (ParserInputException e) {
                System.out.println(String.format("Error: glue parser could not parse line %d of input file. Skipping this line.",formulas.indexOf(s)));
            }
        }
        if (lexicalEntries.isEmpty()) {
            System.out.println("No lexical entries found.");
        }
        else {
            System.out.println(String.format("Found %d lexical entries.",lexicalEntries.size()));
            searchProof(lexicalEntries);
        }
    }

    public static void initiateDependencyMode(String sentence) throws LexicalParserException {
        try {
            SentenceMeaning sm = new SentenceMeaning(sentence);
            searchProof(sm.getLexicalEntries());
        }
        catch (VariableBindingException e) {
            e.printStackTrace();
        }

    }

    /*
    public static void searchProof(List<LexicalEntry> lexicalEntries) throws VariableBindingException {

        LLProver prover = new LLProver(settings);

        searchProof(prover,lexicalEntries);
    }
    */

    public static void searchProof(List<LexicalEntry> lexicalEntries) throws VariableBindingException {

        LLProver2 prover = new LLProver2(settings);

        List<Premise> result;
        try {

            System.out.println("Searching for valid proofs...");

            Sequent testseq = new Sequent(lexicalEntries);

            System.out.println("Sequent:" + testseq.toString());

            prover.deduce(testseq);



            result = prover.getSolutions();






            System.out.println("Found the following deduction(s): ");
            for (Premise sol : result) {
                solutions.add(sol.toString());
                sol.setSemTerm((SemanticExpression) sol.getSemTerm().betaReduce());
                System.out.println(sol.toString());
            }

            /*
            if (settings.isPartial()) {
                for (Premise part : prover.getDatabase())
                {
                    if (part.getPremiseIDs().size() > 1)
                    {
                        partial.add(part.toString());
                    }
                }

                for (Premise part : prover.getModifiers())
                {
                    if (part.getPremiseIDs().size() > 1)
                    {
                        partial.add(part.toString());
                    }
                }
            }

            */

            System.out.println("Done!\n");
            if (settings.isDebugging()) {
                System.out.println("Debugging report:");
                System.out.println(prover.db.toString());
            }

        } catch (ProverException e) {
            e.printStackTrace();
        }


    }

}
