/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

import glueSemantics.lexicon.LexicalEntry;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LinearLogicParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.synInterface.dependency.LexicalParserException;
import glueSemantics.synInterface.dependency.SentenceMeaning;
import glueSemantics.synInterface.lfg.FStructureParser;
import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.File;
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
    Settings settings;

    public static void main(String[] args) {

        System.out.println("The Glue Semantics Workbench\n"+
                            "copyright 2018 Moritz Messmer & Mark-Matthias Zymla\n");
        if (args.length > 0 && args[0].equals("lfg")) {
            try {
                initiateLFGMode();
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }
        else if (args.length > 0 && args[0].equals("dp")){
            try {
                initiateDependencyMode();
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
            searchProof(new SentenceMeaning(input).getLexicalEntries());
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

    private static void initiateDependencyMode(String sentence) throws LexicalParserException {
        try {
            SentenceMeaning sm = new SentenceMeaning(sentence);
            searchProof(sm.getLexicalEntries());
        }
        catch (VariableBindingException e) {
            e.printStackTrace();
        }

    }

    private static void searchProof(List<LexicalEntry> lexicalEntries) throws VariableBindingException {
        Sequent testseq = new Sequent(lexicalEntries);

        System.out.println(testseq.toString());

        System.out.println("Searching for valid proofs...");
        LLProver prover = new LLProver();
        List<Premise> result = null;
        try {
            result = prover.deduce(testseq);
            System.out.println("Found the following deduction(s): ");
            for (Premise sol : result) {
                System.out.println(sol.toString());
            }
        } catch (ProverException e) {
            e.printStackTrace();
        }

        System.out.println("Done!\n");
    }

}
