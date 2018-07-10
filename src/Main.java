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
import glueSemantics.synInterface.dependency.LexicalParserException;
import glueSemantics.synInterface.dependency.SentenceMeaning;
import glueSemantics.synInterface.lfg.FStructureParser;
import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.List;
import java.util.Scanner;

/**
 * This software was created by Mark-Matthias Zymla and Moritz Messmer
 */

public class Main {


    public static void main(String[] args) {

        if (args.length > 0 && args[0].equals("lfg")) {
            try {
                initiateLFGMode();
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }
        }
        else {
            try {
                initiateDependencyMode();
            } catch (VariableBindingException | LexicalParserException e) {
                e.printStackTrace();
            }

        }


    }

    private static void initiateLFGMode() throws VariableBindingException, LexicalParserException {
            File f = null;
            final JFileChooser fc = new JFileChooser();
            fc.setDialogTitle("Choose an f-structure file");
            fc.addChoosableFileFilter(
                    new FileNameExtensionFilter("Prolog f-structure files", "pl"));
            int returnVal = fc.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                f = fc.getSelectedFile();
            } else {
                System.out.println("No file selected");

            }
            Path p;
            if (f != null) {
                p = FileSystems.getDefault().getPath(f.getAbsolutePath());
                new FStructureParser(p);
            }
            else
                System.out.println("No file selected");
    }


    private static void initiateDependencyMode() throws VariableBindingException, LexicalParserException {

        Scanner s = new Scanner(System.in);
        String input;
        while (true) {
            System.out.println("Enter sentence to be analyzed or enter 'quit' to exit the program.");
            input = s.nextLine();
            if (input.equals("quit"))
                break;
            new SentenceMeaning(input);
        }
    }

    private static void initiateDependencyMode(String sentence) throws LexicalParserException {
        try {
            System.out.println();
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
        LLProver prover = new LLProver(testseq);
        List<Premise> result = null;
        try {
            result = prover.deduce();
            System.out.println("Found valid deduction(s): ");
            for (Premise sol : result) {
                System.out.println(sol.toString());
            }
        } catch (ProverException e) {
            e.printStackTrace();
        }

        System.out.println("Done!");
    }

}
