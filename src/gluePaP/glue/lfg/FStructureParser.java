package gluePaP.glue.lfg;

import gluePaP.glue.LexVariableHandler;
import gluePaP.glue.LexicalParserException;
import gluePaP.lexicon.LexicalEntry;
import gluePaP.lexicon.Noun;
import gluePaP.parser.ParserInputException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FStructureParser {
    private final static String[] functions = {"SUBJ", "OBJ", "OBL"};


    private class FStructureNode {
        private int ID;
        private String function;

    }

    /**
     * Parses a Prolog f-structure file as created by XLE
     * @param filepath
     * @return
     */
    public List<PrologPredicate> parseFStructureFile(Path filepath) throws ParserInputException, LexicalParserException {
        List<String> lines = new ArrayList<>();
        String full = "";
        // A map of lexical entries where each entry has the form <nodeID,nodeFunction>
        HashMap<String,String> lexEntries = new HashMap<>();

        try {
            lines = Files.readAllLines(filepath);
            full = new String(Files.readAllBytes(filepath));
        } catch (IOException e) {
            e.printStackTrace();
        }
        // Pattern for intermediate f-structure components, i.e. those that consist of further f-structure components
        Pattern intermediate = Pattern.compile("cf\\((\\d+),eq\\(attr\\(var\\((\\d+)\\),'(\\S+)'\\),var\\((\\d+)\\)\\)\\)");
        Pattern terminal = Pattern.compile("cf\\((\\d+),eq\\(attr\\(var\\((\\d+)\\),'(\\S+)'\\),'(\\S+)'\\)\\)");
        // Patter for extracting PRED features
        //Pattern pred = Pattern.compile("cf\\((\\d+),eq\\(attr\\(var\\((\\d+)\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[(.+?)?],\\[]\\)\\)\\),");
        Matcher m = null;

        // Check for grammatical functions from the function list and add them to lexEntries
        for (String f : functions) {
            for (String line : lines) {
                m = intermediate.matcher(line);
                if (m.group(3).equals(f)) {
                    lexEntries.put(m.group(4),f);
                }
            }
        }


        // For each lexical entry add subordinated nodes (PRED, determiners and adjuncts) and create lexical entries
        for (String i : lexEntries.keySet()) {
            Pattern pred = Pattern.compile("attr\\(var\\("+i+"\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[(.+?)?],\\[]\\)\\)");
            Pattern ntype= Pattern.compile("attr\\(var\\("+i+"\\),'NTYPE'\\),var\\((\\d+)\\)");
            //Get the predicate of this lexical entry
            m = pred.matcher(full);
            String predicate = m.group(2);

            m = ntype.matcher(full);
            if (!m.matches())
                throw new ParserInputException("LFG-Parser exception: something seems to be wrong with the f-structure file.");
            String ntypeID = m.group(2);
            String nsym = Pattern.compile("attr\\(var\\("+ntypeID+"\\),'NSYM'\\),'(\\S+)'").matcher(full).group(1);

            String identifier = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomE);

            // It is a common noun find potential determiners and create the lexical entry
            if (nsym.equals("common")) {
                Noun main = new Noun(LexicalEntry.LexType.N_NN,identifier,predicate);

                m = Pattern.compile("attr\\(var\\("+ntypeID+"\\),'SPEC'\\),var\\((\\d+)\\)").matcher(full);
                // Found a SPEC feature, proceed to extract determiners/quantifiers
                if (m.matches()) {
                    String specID = m.group(1);
                    m = Pattern.compile("attr\\(var\\("+specID+"\\),'(\\S+)'\\),var\\((\\d+)\\)").matcher(full);
                    // Specifier is a quantifier
                    if (m.group(1).equals("QUANT")) {

                    }
                    // Specifier is a determiner
                    else if (m.group(1).equals("DET")) {
                        throw new LexicalParserException("Unknown lexical item: DET");
                    }
                    // Specifier is a number determiner
                    else if (m.group(1).equals("NUMBER")) {
                        throw new LexicalParserException("Unknown lexical item: NUMBER");
                    }
                }
            }
            // It is a proper noun, create a lexical entry
            else if (nsym.equals("proper")) {
                Noun main = new Noun(LexicalEntry.LexType.N_NNP,identifier,predicate);
            }

            //Find all adjuncts subordinated to this function
            Pattern adjunct = Pattern.compile("attr\\(var\\("+i+"\\),'ADJUNCT'\\),var\\((\\d+)\\)");
            String adjID = adjunct.matcher(full).group(1);
            List<String> adjuncts = new ArrayList<>();
            Matcher adjSetMatcher = Pattern.compile("in_set\\(var\\((\\d+)\\),var\\("+adjID+"\\)").matcher(full);
            while (adjSetMatcher.find()) {
                adjuncts.add(adjSetMatcher.group(1));
                // TODO create HashMap of adjunctIDs and their predicates
            }

        }

        return null;
    }
}
