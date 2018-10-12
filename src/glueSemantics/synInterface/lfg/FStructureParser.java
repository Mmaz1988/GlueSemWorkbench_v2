/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.synInterface.lfg;

import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.synInterface.dependency.LexicalParserException;
import glueSemantics.lexicon.*;
import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FStructureParser {
    private final static String[] functions = {"SUBJ", "OBJ", "OBL"};
    private List<LexicalEntry> lexicalEntries = new ArrayList<>();

    public List<LexicalEntry> getLexicalEntries() {
        return lexicalEntries;
    }


    public FStructureParser (Path inputpath) throws VariableBindingException, LexicalParserException {
            lexicalEntries = extractFromFStructureFile(inputpath);

    }

    /**
     * Parses a Prolog f-structure file as created by XLE. It uses regular expressions to extract
     * the necessary information for creating lexical entries. First it extracts all arguments of the root verb
     * and then for each argument further functional dependents like quantifiers and adjuncts. When all verbal
     * arguments are parsed, instantiated as LexicalEntry objects and added to the list of lexical entries, the
     * root verb is instantiated as well and added to the list.
     * @param filepath the path to the Prolog file containing the f-structure information
     * @return the list of lexical entries created from the f-structure file.
     */
    public List<LexicalEntry> extractFromFStructureFile(Path filepath) throws  LexicalParserException, IllegalStateException {
        List<String> lines = new ArrayList<>();
        String full = "";
        // A map of lexical entries where each entry has the form <nodeID,nodeFunction>
        HashMap<String,String> verbalArgs = new HashMap<>();
        List<LexicalEntry> lexicalEntries = new ArrayList<>();
        //SubCatFrame produced from syntactic input; is used to derive meaning constructors
        LinkedHashMap<String,LexicalEntry> subCatFrame = new LinkedHashMap<>();

        try {
            lines = Files.readAllLines(filepath);
            full = new String(Files.readAllBytes(filepath));
        } catch (IOException e) {
            e.printStackTrace();
        }
        // Pattern for intermediate f-structure components, i.e. those that consist of further f-structure components
        Pattern intermediate = Pattern.compile("cf\\((\\d+),eq\\(attr\\(var\\((\\d+)\\),'(\\S+)'\\),var\\((\\d+)\\)\\)\\)");
        Pattern terminal = Pattern.compile("cf\\((\\d+),eq\\(attr\\(var\\((\\d+)\\),'(\\S+)'\\),'(\\S+)'\\)\\)");
        Matcher m = null;

        // Check for grammatical functions from the function list and add them to verbalArgs
        for (String f : functions) {
            m = intermediate.matcher(full);
            while (m.find()) {
                if (m.group(3).equals(f)) {
                    verbalArgs.put(m.group(4),f);
                }
            }
        }


        // For each lexical entry add subordinated nodes (PRED, determiners and adjuncts) and create lexical entries
        for (String i : verbalArgs.keySet()) {
            Pattern ntype= Pattern.compile("attr\\(var\\("+i+"\\),'NTYPE'\\),var\\((\\d+)\\)");

            m = ntype.matcher(full);
            if(!m.find()) { throw new LexicalParserException(m);}
            String ntypeID = m.group(1);
            String nsynID = "";
            m = Pattern.compile("attr\\(var\\("+ntypeID+"\\),'NSYN'\\),'(\\S+)'").matcher(full);
            // For proper nouns, the NSYN entry looks different, check this one as well
            if(!m.find()) {
                m = Pattern.compile("attr\\(var\\("+ntypeID+"\\),'NSYN'\\),var\\((\\d+)\\)").matcher(full);
                if(!m.find()) { throw new LexicalParserException(m);}
                nsynID = m.group(1);
            }

            String nsyn = m.group(1);

            String identifier = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomE);

            // It is a common noun find potential determiners and create the lexical entry
            // Add the appropriate role to the subcatframe (for generating the verb later)
            if (nsyn.equals("common")) {
                //Get the predicate of this lexical entry
                Pattern pred = Pattern.compile("attr\\(var\\("+i+"\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[(.+?)?],\\[]\\)\\)");
                m = pred.matcher(full);
                if(!m.find()) { throw new LexicalParserException(m);}
                String predicate = m.group(1);
                Noun main = new Noun(LexicalEntry.LexType.N_NN,identifier,predicate);
                lexicalEntries.add(main);
                switch (verbalArgs.get(i)) {
                    case "SUBJ" : subCatFrame.put("agent",main); break;
                    case "OBJ"  : subCatFrame.put("patient",main); break;
                }

                m = Pattern.compile("attr\\(var\\("+i+"\\),'SPEC'\\),var\\((\\d+)\\)").matcher(full);
                // Found a SPEC feature, proceed to extract determiners/quantifiers
                if (m.find()) {
                    String specID = m.group(1);
                    m = Pattern.compile("attr\\(var\\("+specID+"\\),'(\\S+)'\\),var\\((\\d+)\\)").matcher(full);
                    if(!m.find()) { throw new LexicalParserException(m);}
                    switch (m.group(1)) {
                        // Specifier is a quantifier
                        case "QUANT":
                            Matcher m2 =
                                    Pattern.compile("attr\\(var\\(" + m.group(2) + "\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[],\\[]\\)").matcher(full);
                            if(!m2.find()) { throw new LexicalParserException(m);}
                            lexicalEntries.add(new Determiner(identifier, m2.group(1), verbalArgs.get(i).toLowerCase()));
                            break;
                        // Specifier is a determiner
                        case "DET":
                            m2 =
                                    Pattern.compile("attr\\(var\\(" + m.group(2) + "\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[],\\[]\\)").matcher(full);
                            if(!m2.find()) { throw new LexicalParserException(m);}
                            lexicalEntries.add(new Determiner(identifier, m2.group(1), verbalArgs.get(i).toLowerCase()));
                            break;
                            // Specifier is a number determiner
                        case "NUMBER":
                            throw new LexicalParserException("Unknown lexical item: NUMBER");
                    }
                }
            }
            // It is a proper noun, create a lexical entry
            else {
                m = Pattern.compile("eq\\(var\\("+nsynID+"\\),'(\\S+)'\\)\\)").matcher(full);
                if(!m.find()) { throw new LexicalParserException(m);}
                nsyn = m.group(1);
                if (nsyn.equals("proper")) {
                    m = Pattern.compile("attr\\(var\\("+i+"\\),'PRED'\\),var\\((\\d+)\\)").matcher(full);
                    if(!m.find()) { throw new LexicalParserException(m);}
                    Matcher predMatcher = Pattern.compile("eq\\(var\\("+m.group(1)+"\\),semform\\('(\\S+)',\\d+,\\[],\\[]\\)").matcher(full);
                    if(!predMatcher.find()) { throw new LexicalParserException(m);}
                    Noun main = new Noun(LexicalEntry.LexType.N_NNP,identifier,predMatcher.group(1));
                    lexicalEntries.add(main);

                    switch (verbalArgs.get(i)) {
                        case "SUBJ" : subCatFrame.put("agent",main); break;
                        case "OBJ"  : subCatFrame.put("patient",main); break;
                    }
                }
            }

            //Find all adjuncts subordinated to this function
            m = Pattern.compile("attr\\(var\\("+i+"\\),'ADJUNCT'\\),var\\((\\d+)\\)").matcher(full);
            if (m.find()) {
                String adjID = m.group(1);
                m = Pattern.compile("in_set\\(var\\((\\d+)\\),var\\("+adjID+"\\)").matcher(full);
                while (m.find()) {
                    Matcher modMatcher = Pattern.compile("attr\\(var\\(" + m.group(1) + "\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[],\\[]\\)").matcher(full);
                    if(!modMatcher.find()) { throw new LexicalParserException(m);}
                    lexicalEntries.add(new Modifier(identifier,modMatcher.group(1)));

                }
            }
        }

        // Get the root verb and add it to the list of lexical entries.
        Pattern root = Pattern.compile("attr\\(var\\(0\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[(\\S+)],\\[]\\)");
        m = root.matcher(full);
        if(!m.find()) { throw new LexicalParserException(m);}
        Verb rootverb = new Verb(subCatFrame,m.group(1));
        lexicalEntries.add(rootverb);

        return lexicalEntries;
    }


/*    public List<TransferFact> convertToTransferFacts(Path filepath) {
        return null;
    }*/
}
