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
import java.io.*;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.logging.Handler;
import main.NaturalDeductionProof;

public class WorkbenchMain {
    // Initialize with default settings
    public static Settings settings = new Settings();
    public static LinkedHashMap<Integer, List<Premise>> solutions = new LinkedHashMap<>();
    public static List<String> partial = new ArrayList<>();
    public static StringBuilder outputFileBuilder = new StringBuilder();
    public static List<Premise> result = new ArrayList<>();
    private final static Logger LOGGER = Logger.getLogger(WorkbenchMain.class.getName());
    
    private static String explanation = "";
    private static boolean explainFail = false;
    private static boolean assureGlueParsing = false;
    private static boolean naturalDeduction = false;
    private static String searchForGoal = "";

    

    public static void main(String[] args) {
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

        boolean onlyMeaningSide = false;
        boolean stdIn = false;
        boolean stdOut = false;
        String inputFileName = "";
        String outputFileName = "";
        
            // Check program arguments for prover settings
            //for (String arg : args) {
            for (int i = 0; i < args.length; i++){
            	String arg = args[i];
                switch (arg) {
                	case("-i"):{
                		inputFileName = args[ i + 1 ];
                		if (inputFileName.charAt(0) == '-')
                			inputFileName = "";
                		stdIn = false;
                		break;
                	}
                	case("-o"):{
                		outputFileName = args[ i + 1 ];
                		if (outputFileName.charAt(0) == '-')
                			outputFileName = "";
                		stdOut = false;
                		break;
                	}
                    case ("-outputStyle"):
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
                    case ("-proveGoal"): {
                    	if (i+1>=args.length)
                    		searchForGoal="";
                    	else 
                    		searchForGoal = args[i+1];
                        break;
                        }
                    case ("-test"):
                    {
                        SemanticParser semParser = new SemanticParser();
                        semParser.testParseExpression2(args[i+1]);
                        System.exit(0);
                        break;
                    }
                    case ("-onlyMeaningSide"):
                    {
                        onlyMeaningSide = true;
                        break;
                    }
                    case("-explainFail"):
                    {
                    	explainFail = true;
                    	break;
                    }
                    case("-readStdIn"):
                    {
                    	stdIn = true;
                    	break;
                    }
                    case("-writeStdOut"):
                    {
                    	stdOut = true;
                    	break;
                    }
                    case("-assureGlueParsing"):
                    {
                    	assureGlueParsing = true;
                    	break;
                    }
                    case("-naturalDeduction"):
                    {
                    	naturalDeduction = true;
                    	break;
                    }                    case ("-vis"):
                    settings.setVisualize(true);
                        break;
                    }
                }
            
            String betaReduce = "on";
            String outputMode = "plain";
            if (!settings.isBetaReduce())
                betaReduce = "off";

            if (settings.getSemanticOutputStyle() == 1)
                outputMode = "prolog";

            if (settings.getSemanticOutputStyle() == 2)
                outputMode = "json";

            if (settings.getSemanticOutputStyle() == 3)
                    outputMode = "nltk";
                String outputSides = "meaning and linear logic sides";
            if (onlyMeaningSide) {
            	outputSides = "only meaning side"; 
            }


            LOGGER.config(String.format("Current settings: automatic beta reduction: %s\t\toutput mode: %s\t\toutput: %s", betaReduce, outputMode, outputSides));

            InputStream inputFileStream = null;
            StringBuilder inputStringBuilder = new StringBuilder();
            BufferedWriter w = null;
            File outFile = null;
            
            // If no output or input method is defined, or one of them is missing then initiate manual mode
			if (!stdIn && inputFileName.equals("") || !stdOut && outputFileName.equals("")) {
				try {
					inputFileName = getFileName("Choose a file containing lexical entries");
			//		outputFileName = getFileName("Choose an output file name");
					stdIn = false;
					stdOut = false;
					// initiateManualMode();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			try {
				// Decide where to output
				if (stdOut) {
					w = new BufferedWriter(new OutputStreamWriter(System.out));
					// Normally LOGGER is connected to stdout. So, 
					// simply remove all handlers from the logger and restore stderr 
					// connection back
					Handler[] currentHandlers = LOGGER.getHandlers();
					for(int i=0;i<currentHandlers.length;i++) {
						LOGGER.removeHandler(currentHandlers[i]);
					}
					LOGGER.addHandler(new StreamHandler(System.err, new MyFormatter()));
				} else if (!outputFileName.equals("")) {
					outFile = new File(outputFileName);
					if (outFile.exists()) {
						outFile.delete();
						outFile.createNewFile();
					} else {
						outFile.createNewFile();
					}
					if (outFile.exists()) {
						w = new BufferedWriter(new FileWriter(outFile, true));
					}
				}
				// Decide from where to read the input
				if (stdIn) {
					inputFileStream = System.in;
				} else if (!inputFileName.equals("")) {
					inputFileStream = new FileInputStream(new File(inputFileName));
				}
			} catch (Exception e) {
				e.printStackTrace();
			}            
            
      		/* Read the input */
       		Scanner scanner = null;
       		try {
       			scanner = new Scanner(inputFileStream);
       		    while (scanner.hasNextLine()) {
       		    	inputStringBuilder.append(scanner.nextLine() + System.lineSeparator());
       		    	}
       			}
       		catch(Exception e) {
       			e.printStackTrace();
       		}
       		finally {
       		    if(scanner != null)
       		        scanner.close();
       		}
       		InputOutputProcessor.process(inputStringBuilder.toString());
    		String input = InputOutputProcessor.translate(inputStringBuilder.toString()) ;

       		String lines[] = input.split("\\r?\\n|\\r");
       		

			try {
				initiateManualMode(Arrays.asList(lines));

				if (!solutions.keySet().isEmpty()) {
					if (onlyMeaningSide) {
						int nProofs = 0;
						for (Integer key : solutions.keySet()) 
							for (int i = 0; i < solutions.get(key).size(); i++) 
								nProofs ++;

						if (nProofs == 1) 
							w.append("% 1 proof found." + System.lineSeparator());
						else 
							w.append("% " + Integer.toString(nProofs) + " proofs found." + System.lineSeparator());
					}
					for (Integer key : solutions.keySet()) {
						for (int i = 0; i < solutions.get(key).size(); i++) {
							Premise solution = solutions.get(key).get(i);
							if (onlyMeaningSide) {
								w.append(solution.getSemTerm().toString() + System.lineSeparator());
								if(naturalDeduction)w.append(NaturalDeductionProof.getNaturalDeductionProof(solution));
							} else if (settings.getSemanticOutputStyle() == 1) {
								w.append("solution" + "(" + key.toString() + i + ",");
								w.append(solution.getSemTerm().toString());
								w.append(").");
								w.append(System.lineSeparator());
								if(naturalDeduction)w.append(NaturalDeductionProof.getNaturalDeductionProof(solution));
							} else {
								w.append(InputOutputProcessor.restoreBackLinearLogicSide(solution.toString()));
								w.append(System.lineSeparator());
								if(naturalDeduction)w.append(NaturalDeductionProof.getNaturalDeductionProof(solution));
							}
						}
					}
				} else {
					if (explainFail && !explanation.equals("")&& searchForGoal=="") {
						w.append("% No proof. Explanation: " + System.lineSeparator());
						w.append(explanation);
					}
					LOGGER.info("No solutions found for given input.");
				}

				if (!settings.getSolutionOnly()) {
					if (!onlyMeaningSide) {
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
				}
				w.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
        System.exit(0);
        }

    public static String getFileName(String message) {
        File f = null;
        final JFileChooser fc = new JFileChooser();
        fc.setDialogTitle(message);
        fc.addChoosableFileFilter(
                new FileNameExtensionFilter("Text files", "txt"));
        int returnVal = fc.showOpenDialog(null);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            LOGGER.info("Selected file " + fc.getSelectedFile().getName());
            f = fc.getSelectedFile();
        } else {
           LOGGER.info("No file selected");
        }
        Path p=null;
        if (f != null) {
            p = FileSystems.getDefault().getPath(f.getAbsolutePath());
            }
        if (p == null)
        	return "";
       	return p.toString();
    }

    public static void initiateManualMode(List<String> formulas) throws LexicalParserException, VariableBindingException {
        LinkedHashMap<Integer,List<LexicalEntry>> lexicalEntries = new LinkedHashMap<>();
        GlueParser parser = new GlueParser(settings.isParseSemantics());
        Integer sets = 0;
        Pattern wrapperStart = Pattern.compile("\\t*\\{\\t*");
        Pattern wrapperEnd = Pattern.compile("\\t*\\}\\t*");

        LOGGER.info("Now parsing input premises...");

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
                        LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
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
                if(assureGlueParsing) {
                    LOGGER.warning("Skipping this set of premises since some premises in this set could not be parsed properly.");
                	singleSet.clear();
                	break;
                }
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
                if (solutions.containsKey(key)) {
                    for (Premise p : solutions.get(key)) {
                        solutionBuilder.append(InputOutputProcessor.restoreBackLinearLogicSide(p.toString()));
                        solutionBuilder.append(System.lineSeparator());
                    }
                } else
                {
                if(searchForGoal!=""  && prover instanceof LLProver2) {
                	/* Write the output to STDERR */
                	explanation = failExplainer.getLargestGoalPremiseCombination(searchForGoal, ((LLProver2)prover).getNonAtomicChart(), ((LLProver2)prover).getAtomicChart(), naturalDeduction);
                	System.err.println(explanation);
                
                }
                else if(explainFail && prover instanceof LLProver2)
                	{
                		explanation = failExplainer.explain( ((LLProver2)prover).getNonAtomicChart(), ((LLProver2)prover).getAtomicChart(), naturalDeduction);
                	}
                    solutionBuilder.append("None!");
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

            if (settings.getProverType() == 1 && settings.isVisualize())
            {
                //visualization.getContentPane().add(((LLProver1) prover).analysis.displayGraph());
                ((LLProver1) prover).analysis.displayGraph();
            //    visualization.setVisible(true);
            }


        } catch (ProverException e) {
            e.printStackTrace();
        }

    }

}
