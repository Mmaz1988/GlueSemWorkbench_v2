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
import prover.*;
import utilities.LexicalParserException;
import utilities.MyFormatter;

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
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class WorkbenchMain {
    // Initialize with default settings
    public static Settings settings = new Settings();
    public static LinkedHashMap<Integer, List<Premise>> solutions = new LinkedHashMap<>();
    public static List<String> partial = new ArrayList<>();
    public static StringBuilder outputFileBuilder = new StringBuilder();
    public static List<Premise> result = new ArrayList<>();
    private final static Logger LOGGER = Logger.getLogger(WorkbenchMain.class.getName());

    public static void main(String[] args) {
        settings = new Settings();

        System.out.println("Test"); 

        /*
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new SimpleFormatter());
        handler.setLevel(Level.ALL);
        LOGGER.addHandler(handler);
         */
        LOGGER.setUseParentHandlers(false);
        StreamHandler handler = new StreamHandler(System.out, new MyFormatter());
     //   handler.setFormatter(new MyFormatter());
        handler.setLevel(Level.FINE);
        LOGGER.addHandler(handler);

        LOGGER.setLevel(Level.ALL);

       LOGGER.info("The Glue Semantics Workbench -- copyright 2018 Moritz Messmer & Mark-Matthias Zymla");


            // Check program arguments for prover settings
            //for (String arg : args) {
              for (int i = 0; i < args.length; i++)
              { String arg = args[i];
                switch (arg) {
                    case ("-inputStyle"):
                        settings.setSemanticOutputStyle(Integer.parseInt(args[i+1]));
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
                        break;
                    case ("-parseSem"): {
                        settings.setParseSemantics(true);
                        break;
                    }
                    case ("-s"): {
                        settings.setSolutionOnly(true);
                        break;
                    }
                    case ("-pr"): {
                        String arg2 = args[i+1];
                        if (arg2.equals("0") || arg2.equals("HEPPLE"))
                        {
                            settings.setProverType(0);
                        } else if (arg2.equals("1") || arg2.equals("LEV"))
                        {
                            settings.setProverType(1);
                        }
                        break;
                        }

                    case ("-test"):
                    {
                        SemanticParser semParser = new SemanticParser();
                        semParser.testParseExpression2(args[i+1]);
                        System.exit(0);
                        break;
                    }
                    }

                }
            String betaReduce = "on", outputMode = "plain";
            if (!settings.isBetaReduce())
                betaReduce = "off";

            if (settings.getSemanticOutputStyle() == 1)
                outputMode = "prolog";

            if (settings.getSemanticOutputStyle() == 2)
                outputMode = "json";

            LOGGER.config(String.format("Current settings: automatic beta reduction: %s\t\toutput mode: %s", betaReduce, outputMode));

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
                        LOGGER.info("Read input file: " + inFile.toString());
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
                                            if (settings.getSemanticOutputStyle() == 1) {
                                                w.append("solution" + "(" + key.toString() + i + ",");
                                                w.append(solution.toString());
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
                                LOGGER.info("Wrote output to: " + outFile.toString());
                            } catch (Exception e) {
                                LOGGER.warning("Error while generating output file. Maybe no valid path was given.");
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
            System.exit(0);
        }



    public static void initiateManualMode() throws LexicalParserException, VariableBindingException {
        LOGGER.info("Starting manual entry mode...");
        File f = null;
        final JFileChooser fc = new JFileChooser();
        fc.setDialogTitle("Choose a file containing lexical entries");
        fc.addChoosableFileFilter(
                new FileNameExtensionFilter("Text files", "txt"));
        int returnVal = fc.showOpenDialog(null);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            LOGGER.info("Selected file " + fc.getSelectedFile().getName());
            f = fc.getSelectedFile();
        } else {
           LOGGER.info("No file selected");
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
            LOGGER.info("No file selected");
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
                       LOGGER.warning(String.format("Error: " +
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
                LOGGER.warning(String.format("Error: glue parser could not parse line %d of input file. Skipping this line.",formulas.indexOf(s)));
            }
        }
        if (singleSet.isEmpty()) {
            LOGGER.warning("No lexical entries found.");
        }
        else {
            LOGGER.info(String.format("Searching for valid proofs in proof with id S%d",0));
            searchProof(0,singleSet);
            }
        } else {
                //TODO fix output to accomodate for multiple entries
                LOGGER.info(String.format("Found %d different proofs in input file.", lexicalEntries.size()));
               for (Integer key : lexicalEntries.keySet()) {
                   LOGGER.info(String.format("Searching for valid proofs in proof with id S%d",key));
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

        LOGGER.info(String.format("Found %d lexical entries for proof with id S%d",lexicalEntries.size(),key));

        LLProver prover;

        if (settings.getProverType() == 1) {
            prover = new LLProver1(settings, outputFileBuilder);
        } else
        {
        prover = new LLProver2(settings,outputFileBuilder);
        }
        try {



            Sequent testseq = new Sequent(lexicalEntries);

            prover.deduce(testseq);
            result = prover.getSolutions();


        // LOGGER.info("Found the following deduction(s):\n");
         StringBuilder resultBuilder = new StringBuilder();
            for (Premise sol : result) {

                if (solutions.containsKey(key))
                {
                    solutions.get(key).add(sol);
                }
                else
                {
                    solutions.put(key,new ArrayList<>(Arrays.asList(sol)));
                }
        //        sol.setSemTerm((SemanticExpression) sol.getSemTerm().betaReduce());
            }


                StringBuilder solutionBuilder = new StringBuilder();
                solutionBuilder.append(String.format("Found the following solutions for proof with id S%d:\n",key));
                for (Premise p : solutions.get(key))
                {
                    solutionBuilder.append(p.toString());
                    solutionBuilder.append(System.lineSeparator());
                }

                LOGGER.info(solutionBuilder.toString());


          //  LOGGER.info(String.format("Found %d solution(s) for derivation with id S%d",solutions.size(),key) );

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


            if (settings.isDebugging()) {
                LOGGER.info(String.format("Generated debugging report for proof with id S%d:\n" + prover.db.toString(),key));
                LOGGER.info(String.format("Finished glue derivation of proof with id S%d.",key));
            }

        } catch (ProverException e) {
            e.printStackTrace();
        }


    }

}
