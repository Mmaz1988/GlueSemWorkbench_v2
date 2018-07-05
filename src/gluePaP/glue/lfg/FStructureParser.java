package gluePaP.glue.lfg;

import Prover.LLProver;
import Prover.ProverException;
import Prover.VariableBindingException;
import gluePaP.glue.LexVariableHandler;
import gluePaP.glue.LexicalParserException;
import gluePaP.lexicon.Determiner;
import gluePaP.lexicon.LexicalEntry;
import gluePaP.lexicon.Noun;
import gluePaP.lexicon.Verb;
import gluePaP.linearLogic.Premise;
import gluePaP.linearLogic.Sequent;
import gluePaP.parser.ParserInputException;

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


    public static void main(String[] args) {
        File f = null;
        final JFileChooser fc = new JFileChooser();
        int returnVal = fc.showOpenDialog(null);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            f = fc.getSelectedFile();
        }
        else {
            System.out.println("Something went wrongt");

        }
        Path p = FileSystems.getDefault().getPath(f.getAbsolutePath());

        try {
            FStructureParser fsp = new FStructureParser(p);
        } catch (VariableBindingException e) {
            e.printStackTrace();
        }

    }


    public FStructureParser (Path inputpath) throws VariableBindingException {
        List<LexicalEntry> lexicalEntries = null;
        try {
            lexicalEntries = parseFStructureFile(inputpath);
        } catch (LexicalParserException e) {
            e.printStackTrace();
        }
        if (lexicalEntries != null) {

        }

        Sequent testseq = new Sequent(lexicalEntries);

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
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

    /**
     * Parses a Prolog f-structure file as created by XLE. It uses regular expressions to extract
     * the necessary information for creating lexical entries. First it extracts all arguments of the root verb
     * and then for each argument further functional dependents like quantifiers and adjuncts. When all verbal
     * arguments are parsed, instantiated as LexicalEntry objects and added to the list of lexical entries, the
     * root verb is instantiated as well and added to the list.
     * @param filepath the path to the Prolog file containing the f-structure information
     * @return the list of lexical entries created from the f-structure file.
     */
    public List<LexicalEntry> parseFStructureFile(Path filepath) throws  LexicalParserException, IllegalStateException {
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
            Pattern pred = Pattern.compile("attr\\(var\\("+i+"\\),'PRED'\\),semform\\('(\\S+)',\\d+,\\[(.+?)?],\\[]\\)\\)");
            //Pattern pred = Pattern.compile("'PRED'\\),semform\\('(\\S+)',\\d+,");
            Pattern ntype= Pattern.compile("attr\\(var\\("+i+"\\),'NTYPE'\\),var\\((\\d+)\\)");
            //Get the predicate of this lexical entry
            m = pred.matcher(full);
            if(!m.find()) { throw new LexicalParserException(m);}
            String predicate = m.group(1);

            m = ntype.matcher(full);
            if(!m.find()) { throw new LexicalParserException(m);}
            String ntypeID = m.group(1);
            m = Pattern.compile("attr\\(var\\("+ntypeID+"\\),'NSYN'\\),'(\\S+)'").matcher(full);
            if(!m.find()) { throw new LexicalParserException(m); }
            String nsym = m.group(1);

            String identifier = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomE);

            // It is a common noun find potential determiners and create the lexical entry
            // Add the appropriate role to the subcatframe (for generating the verb later)
            if (nsym.equals("common")) {
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
                            throw new LexicalParserException("Unknown lexical item: DET");

                            // Specifier is a number determiner
                        case "NUMBER":
                            throw new LexicalParserException("Unknown lexical item: NUMBER");
                    }
                }
            }
            // It is a proper noun, create a lexical entry
            else if (nsym.equals("proper")) {
                lexicalEntries.add(new Noun(LexicalEntry.LexType.N_NNP,identifier,predicate));
            }

            //Find all adjuncts subordinated to this function
            m = Pattern.compile("attr\\(var\\("+i+"\\),'ADJUNCT'\\),var\\((\\d+)\\)").matcher(full);
            if (m.find()) {
                String adjID = m.group(1);
                List<String> adjuncts = new ArrayList<>();
                Matcher adjSetMatcher = Pattern.compile("in_set\\(var\\((\\d+)\\),var\\("+adjID+"\\)").matcher(full);
                while (adjSetMatcher.find()) {
                    adjuncts.add(adjSetMatcher.group(1));
                    // TODO create HashMap of adjunctIDs and their predicates
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
}
