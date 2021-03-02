package main;/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.parser.GlueParser;
import glueSemantics.parser.ParserInputException;
import glueSemantics.parser.SemanticParser;
import glueSemantics.semantics.LexicalEntry;
import prover.LLProver2;
import prover.ProverException;
import prover.VariableBindingException;
import prover.categoryGraph.History;
import utilities.LexicalParserException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class WorkbenchMain {
    // Initialize with default settings
    public static Settings settings = new Settings();
    public static LinkedHashMap<Integer, List<Premise>> solutions = new LinkedHashMap<>();
    public static List<String> partial = new ArrayList<>();
    public static StringBuilder outputFileBuilder = new StringBuilder();
    public static List<History> result = new ArrayList<>();

    public static void main(String[] args) {
        settings = new Settings();
        System.out.println("The Glue Semantics Workbench\n" +
                "copyright 2018 Moritz Messmer & Mark-Matthias Zymla\n");


        if (args[0].equals("-test")) {
            SemanticParser semParser = new SemanticParser();
            semParser.testParseExpression2(args[1]);
        } else {
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
                        break;
                    case ("-go"):
                        settings.setGlueOnly(true);
                    case ("-parseSem"): {
                        settings.setParseSemantics(true);
                    }
                    case ("-s"): {
                        settings.setSolutionOnly(true);
                    }

                }
            }

            String betaReduce = "on", outputMode = "plain";
            if (!settings.isBetaReduce())
                betaReduce = "off";

            if (settings.getSemanticOutputStyle() == 1)
                outputMode = "prolog";

            System.out.println(String.format("Current settings: automatic beta reduction: %s\t\toutput mode: %s", betaReduce, outputMode));

            if (args.length > 0 && args[0].equals("-i")) {
                try {

                    File inFile = new File(args[1]);

                    if (inFile.exists()) {
                        List<String> lines = null;
                        try {
                            lines = Files.readAllLines(inFile.toPath());
                        } catch (IOException e) {
                            throw new LexicalParserException("Error while trying to open file '"
                                    + inFile + "'");
                        }

                        initiateManualMode(lines);

                        if (args[2].equals("-o")) {
                            try {
                                File outFile = new File(args[3]);

                                if (outFile.exists()) {
                                    outFile.delete();
                                    outFile.createNewFile();
                                } else {
                                    outFile.createNewFile();
                                }

                                if (outFile.exists()) {
                                    BufferedWriter w = new BufferedWriter(new FileWriter(outFile, true));


                                    for (Integer key : solutions.keySet()) {


                                        for (int i = 0; i < solutions.get(key).size(); i++) {
                                            Premise solution = solutions.get(key).get(i);
                                            if (settings.PROLOG == 1) {
                                                w.append("solution" + "(" + key.toString() + i + ",");
                                                w.append(solution.getSemTerm().toString());
                                                w.append(").");
                                                w.append(System.lineSeparator());
                                            } else {
                                                w.append(solution.toString());
                                                w.append(System.lineSeparator());
                                            }
                                        }

                                    }

                                    if (!settings.getSolutionOnly()) {
                                        w.append(System.lineSeparator());
                                        w.append("Proof:");
                                        w.append(System.lineSeparator());

                                        w.append(outputFileBuilder.toString());

                                        if (settings.isPartial()) {
                                            w.append("The following partial solutions were found:");
                                            w.append(System.lineSeparator());

                                            for (String partialSol : partial) {
                                                w.append(partialSol);
                                                w.append(System.lineSeparator());
                                            }

                                        }
                                    }

                                    w.close();
                                }

                                System.out.println("Wrote solutions to " + outFile.toString());


                            } catch (Exception e) {
                                System.out.println("Error while generating output file. Maybe no valid path was given.");
                            }
                        }

                    }
                } catch (VariableBindingException | LexicalParserException e) {
                    e.printStackTrace();
                }
            } else {
                try {
                    initiateManualMode();
                } catch (VariableBindingException | LexicalParserException e) {
                    e.printStackTrace();
                }
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
        LinkedHashMap<Integer,List<LexicalEntry>> lexicalEntries = new LinkedHashMap<>();
        GlueParser parser = new GlueParser(settings.isParseSemantics());
        Integer sets = 0;
        Pattern wrapperStart = Pattern.compile("\\t*\\{\\t*");
        Pattern wrapperEnd = Pattern.compile("\\t*\\}\\t*");

        for (int i = 0; i < formulas.size(); i++) {
            Matcher startMatcher = wrapperStart.matcher(formulas.get(i));

            if (startMatcher.matches()) {
                List<LexicalEntry> currentLexicalEntries = new LinkedList<>();
                i++;
                Boolean newEntry = true;
                while (newEntry) {
                    Matcher endMatcher = wrapperEnd.matcher(formulas.get(i));

                    if (endMatcher.matches()) {
                        newEntry = false;
                        lexicalEntries.put(sets, currentLexicalEntries);
                        sets++;
                        break;
                    }

                    try {
                        currentLexicalEntries.add(parser.parseMeaningConstructor(formulas.get(i)));
                    } catch (ParserInputException e) {
                        System.out.println(String.format("Error: " +
                                "glue parser could not parse line %d of input file. " +
                                "Skipping this line.", formulas.indexOf(formulas.get(i))));
                    }
                    i++;
                }


            }
        }

        List<LexicalEntry> singleSet = new ArrayList<>();
        if (lexicalEntries.keySet().isEmpty()) {

        for (String s : formulas) {
            try {
                singleSet.add(parser.parseMeaningConstructor(s));
            } catch (ParserInputException e) {
                System.out.println(String.format("Error: glue parser could not parse line %d of input file. Skipping this line.",formulas.indexOf(s)));
            }
        }
        if (singleSet.isEmpty()) {
            System.out.println("No lexical entries found.");
        }
        else {
            System.out.println(String.format("Found %d lexical entries.",singleSet.size()));
            searchProof(0,singleSet);
            }

              } else {
                //TODO fix output to accomodate for multiple entries
                System.out.println(String.format("Found %d lexical entries.", lexicalEntries.size()));

               for (Integer key : lexicalEntries.keySet()) {

                   searchProof(key,lexicalEntries.get(key));
               }
            }

            }

    /*
    public static void searchProof(List<LexicalEntry> lexicalEntries) throws VariableBindingException {

        LLProver prover = new LLProver(settings);

        searchProof(prover,lexicalEntries);
    }
    */

    public static void searchProof(Integer key, List<LexicalEntry> lexicalEntries) throws VariableBindingException {

        LLProver2 prover = new LLProver2(settings,outputFileBuilder);

        try {

            System.out.println("Searching for valid proofs...");

            Sequent testseq = new Sequent(lexicalEntries);

            System.out.println("Sequent:" + testseq.toString());

            prover.deduce(testseq);


            result = prover.getSolutions();

            System.out.println("Found the following deduction(s): ");
            for (History sol : result) {

                if (solutions.keySet().contains(key))
                {
                    solutions.get(key).add(sol.premise);
                }
                else
                {
                    solutions.put(key,new ArrayList<>(Arrays.asList(sol.premise)));
                }

        //        sol.setSemTerm((SemanticExpression) sol.getSemTerm().betaReduce());
                System.out.println(key + ": " + sol.premise.toString());
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
