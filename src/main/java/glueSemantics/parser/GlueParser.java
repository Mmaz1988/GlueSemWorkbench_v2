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

package glueSemantics.parser;


import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.MeaningConstructor;
import glueSemantics.semantics.MeaningRepresentation;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.SemSet;
import main.Settings;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import utilities.MyFormatter;

import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GlueParser {
    private LinearLogicParser llparser = new LinearLogicParser();
    private SemanticParser semParser = new SemanticParser();
    public static final MeaningRepresentation emptyMeaning = new MeaningRepresentation("<empty>");
    // TODO add semantic parser here
    private final static Logger LOGGER = Logger.getLogger(GlueParser.class.getName());
    private static final Pattern SOURCE_INDEX_PREFIX = Pattern.compile("^\\s*\\[([^\\]]+)\\]\\s*(.*)$");

    static {
        LOGGER.setUseParentHandlers(false);
        StreamHandler handler = new StreamHandler(System.out, new MyFormatter());
        //   handler.setFormatter(new MyFormatter());
        handler.setLevel(Level.FINE);
        LOGGER.addHandler(handler);

        LOGGER.setLevel(Level.ALL);
    }
    private boolean PARSESEMANTCS;

    public GlueParser() {
        this.PARSESEMANTCS = false;
    }

    public GlueParser(Boolean parseSemantics)
    {
        this.PARSESEMANTCS = parseSemantics;
    }

    public GlueParser(Settings settings) {
        this.PARSESEMANTCS = settings.isParseSemantics();
        this.semParser = new SemanticParser(settings);
    }

    public MeaningConstructor parseMeaningConstructor(String mc) throws ParserInputException {
        return parseMeaningConstructor(mc, "0");
    }

    public MeaningConstructor parseMeaningConstructor(String mc, String stage) throws ParserInputException {
        Integer sourceIndex = null;
        Matcher sourceIndexMatcher = SOURCE_INDEX_PREFIX.matcher(mc.trim());
        if (sourceIndexMatcher.matches()) {
            String sourceLabel = sourceIndexMatcher.group(1).trim();
            mc = sourceIndexMatcher.group(2).trim();
            if (sourceLabel.matches("\\d+")) {
                sourceIndex = Integer.valueOf(sourceLabel);
            } else {
                LOGGER.warning("Meaning constructor source label '[" + sourceLabel + "]' is not numeric; " +
                        "stripping it from the meaning without recording a source index.");
            }
        }

        String[] mcList = mc.split(":");
        if (mcList.length == 0) {
            throw new ParserInputException("Error parsing formula '" + mc + "'. " +
                    "Meaning side and glue side need to be separated with a ':'");
        }

        if (mcList.length > 2) {
           //join all elements except last one
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < mcList.length - 1; i++)
            {
                sb.append(mcList[i]);
                if (i < mcList.length - 2)
                {
                    sb.append(":");
                }
            }
            mcList[0] = sb.toString();
            mcList[1] = mcList[mcList.length - 1];
        }


        boolean noscope = false;
        boolean insitu = false;
        String glueString = "";

        try {
            String[] glueSide = mcList[1].split("\\|\\|");

            if (glueSide.length == 2)
            {
                String[] modifiers = glueSide[1].trim().split(",");
                for (String modifier : modifiers) {
                    String normalizedModifier = modifier.trim().replaceAll("[.;]$", "");
                    if (normalizedModifier.isEmpty()) {
                        continue;
                    }
                    if (normalizedModifier.equals("noscope")) {
                        noscope = true;
                    } else if (normalizedModifier.equals("insitu")) {
                        insitu = true;
                    } else {
                        throw new ParserInputException("Unsupported glue modifier: " + normalizedModifier);
                    }
                }

                if (noscope && insitu) {
                    throw new ParserInputException("noscope and insitu are mutually exclusive");
                }

                glueString = glueSide[0];
            }
            else
            {
                glueString = mcList[1];
            }

        } catch (ParserInputException e) {
            throw e;
        } catch (Exception e)
        {
         throw new ParserInputException("Error parsing formula '" + mc + "'. Glue modifiers need to be specified after a '||'");
        }



        MeaningConstructor entry = new MeaningConstructor(sourceIndex);
        LLTerm glue = llparser.callParser(glueString.trim());
        SemanticRepresentation sem = null;
        if (!PARSESEMANTCS) {
            sem = new MeaningRepresentation(mcList[0].trim());
            //TODO
            //((MeaningRepresentation) sem).setType(glue.getType());
        } else
        {
           sem = semParser.parse(mcList[0].trim());

        }

        if (sourceIndex != null && sem != null) {
            sem.addSourceIndex(sourceIndex);
        }

        //TODO experiment




        if (sem instanceof SemSet)
        {
            for (SemanticRepresentation m : ((SemSet) sem).getMembers())
            {
                if (m.getType().equals("u"))
                {
                    m.setType(glue.getType());
                }
            }
        }
        if (sem.getType().equals("u")){
            sem.setType(glue.getType());
        }

        entry.setLlTerm(glue);
        entry.setSem(sem);
        entry.setNonscope(noscope);
        entry.setInsitu(insitu);
        entry.setStage(stage);

        return entry;
    }


    public LexicalEntries parseMeaningConstructorString(String mc, boolean multistage) throws ParserInputException {
        List<String> formulas = Arrays.asList(mc.split("\n"));
        if (multistage)
        {
            return parseMultiStageMCList(formulas);
        }
        return parseMeaningConstructorList(formulas);
    }

    public LexicalEntries parseMeaningConstructorList(List<String> formulas) throws ParserInputException {

        //Split string into lines
        Graph<String, DefaultEdge> multiStageMapping = new DefaultDirectedGraph<>(DefaultEdge.class);

        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = new LinkedHashMap<>();
        Integer sets = 0;
        Pattern wrapperStart = Pattern.compile("[^\\S\\r\\n]*\\{[^\\S\\r\\n]*");
        Pattern wrapperEnd = Pattern.compile("[^\\S\\r\\n]*\\}[^\\S\\r\\n]*");


        LOGGER.info("Now parsing input premises...");

        List<MeaningConstructor> ungroupedEntries = new ArrayList<>();

        Integer stage = 0;
        Integer sister = 0;
        HashMap<Integer,Integer> sisters = new HashMap<>();
        sisters.put(stage,sister);

        for (int i = 0; i < formulas.size(); i++) {
            String current = formulas.get(i);
            Matcher startMatcher = wrapperStart.matcher(current);

            if (current.startsWith("//"))
            {
                continue;
            }

            //sets corresponds to the number of individual proofs
            if (startMatcher.matches()) {
                sets++;
                List<MeaningConstructor> currentLexicalEntries = new LinkedList<>();
                i++;
                Boolean newEntry = true;
                //Here the mcs for one proof are calculated
                while (newEntry) {
                    String newItem = formulas.get(i).trim();
                    Matcher endMatcher = wrapperEnd.matcher(newItem);
                    Matcher currentStartMatcher = wrapperStart.matcher(newItem);


                    if (endMatcher.matches()) {
                        if (stage > 0) {
                            i++;
                            String vertex = stage + "+" + sisters.get(stage);
                            String parentVertex = stage - 1 + "+" + (sisters.get(stage-1));
                            multiStageMapping.addVertex(vertex);
                            if (!multiStageMapping.vertexSet().contains(parentVertex)) {
                                multiStageMapping.addVertex(parentVertex);
                            }
                            multiStageMapping.addEdge(parentVertex,vertex);
                            sisters.put(stage,sisters.get(stage) + 1);
                            stage = stage - 1;
                            continue;
                        } else
                        if (stage == 0) {
                            multiStageMapping.addVertex("0+0");
                            newEntry = false;
                            lexicalEntries.put(sets, currentLexicalEntries);
                            continue;
                        }
                    }

                    if (currentStartMatcher.matches())
                    {
                        i++;
                        stage = stage + 1;
                        if (!sisters.containsKey(stage))
                        {
                            sisters.put(stage,0);
                        }
                        continue;
                    }
                    try {

                        if (formulas.get(i).startsWith("//"))
                        {
                            i++;
                            continue;
                        }

                        LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
                        currentLexicalEntries.add(parseMeaningConstructor(newItem,stage.toString() + "+" + sisters.get(stage)));
                    } catch (ParserInputException e) {
                        LOGGER.warning(String.format("Error: " +
                                "glue parser could not parse line %d of input file. " +
                                "Skipping this line.", formulas.indexOf(formulas.get(i))));
                    }
                    i++;
                }

                lexicalEntries.put(sets, currentLexicalEntries);

            } else
            {
            try {
                    LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
                    ungroupedEntries.add(parseMeaningConstructor(formulas.get(i)));
                } catch (ParserInputException e) {
                    LOGGER.warning(String.format("Error: " +
                            "glue parser could not parse line %d of input file. " +
                            "Skipping this line.", formulas.indexOf(formulas.get(i))));
                }
            }
        }

        if (!ungroupedEntries.isEmpty())
        {
            lexicalEntries.put(0, ungroupedEntries);
        }

        return new LexicalEntries(lexicalEntries);
    }



    public LexicalEntries parseMultiStageMCList(List<String> formulas) throws ParserInputException {

        //Split string into lines
        Graph<String, DefaultEdge> multiStageMapping = new DefaultDirectedGraph<>(DefaultEdge.class);

        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = new LinkedHashMap<>();
        Integer sets = 0;
        Pattern wrapperStart = Pattern.compile("\\t*\\{\\t*");
        Pattern wrapperEnd = Pattern.compile("\\t*\\}\\t*");


        LOGGER.info("Now parsing input premises...");

        List<MeaningConstructor> ungroupedEntries = new ArrayList<>();

        Integer stage = 0;
        Integer sister = 0;
        HashMap<Integer,Integer> sisters = new HashMap<>();
        sisters.put(stage,sister);

        for (int i = 0; i < formulas.size(); i++) {
            String current = formulas.get(i);
            Matcher startMatcher = wrapperStart.matcher(current);

            if (current.startsWith("//"))
            {
                continue;
            }

            //sets corresponds to the number of individual proofs
            if (startMatcher.matches()) {
                sets++;
                List<MeaningConstructor> currentLexicalEntries = new LinkedList<>();
                i++;
                Boolean newEntry = true;
                //Here the mcs for one proof are calculated
                while (newEntry) {
                    String newItem = formulas.get(i).trim();
                    Matcher endMatcher = wrapperEnd.matcher(newItem);
                    Matcher currentStartMatcher = wrapperStart.matcher(newItem);


                    if (endMatcher.matches()) {
                        if (stage > 0) {
                            i++;
                            String vertex = stage + "+" + sisters.get(stage);
                            String parentVertex = stage - 1 + "+" + (sisters.get(stage-1));
                            multiStageMapping.addVertex(vertex);
                            if (!multiStageMapping.vertexSet().contains(parentVertex)) {
                                multiStageMapping.addVertex(parentVertex);
                            }
                            multiStageMapping.addEdge(parentVertex,vertex);
                            sisters.put(stage,sisters.get(stage) + 1);
                            stage = stage - 1;
                            continue;
                        } else
                        if (stage == 0) {
                            multiStageMapping.addVertex("0+0");
                            newEntry = false;
                            lexicalEntries.put(sets, currentLexicalEntries);
                            continue;
                        }
                    }

                    if (currentStartMatcher.matches())
                    {
                        i++;
                        stage = stage + 1;
                        if (!sisters.containsKey(stage))
                        {
                            sisters.put(stage,0);
                        }
                        continue;
                    }
                    try {

                        if (formulas.get(i).startsWith("//"))
                        {
                            i++;
                            continue;
                        }

                        LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
                        currentLexicalEntries.add(parseMeaningConstructor(newItem,stage.toString() + "+" + sisters.get(stage)));
                    } catch (ParserInputException e) {
                        LOGGER.warning(String.format("Error: " +
                                "glue parser could not parse line %d of input file. " +
                                "Skipping this line.", formulas.indexOf(formulas.get(i))));
                    }
                    i++;
                }

                lexicalEntries.put(sets, currentLexicalEntries);

            } else
            {
                try {
                    LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
                    ungroupedEntries.add(parseMeaningConstructor(formulas.get(i)));
                } catch (ParserInputException e) {
                    LOGGER.warning(String.format("Error: " +
                            "glue parser could not parse line %d of input file. " +
                            "Skipping this line.", formulas.indexOf(formulas.get(i))));
                }
            }
        }

        if (!ungroupedEntries.isEmpty())
        {
            lexicalEntries.put(0, ungroupedEntries);
        }

        return new LexicalEntries(lexicalEntries,multiStageMapping);
    }


    /*
    public static void main(String[] args) throws VariableBindingException {
        String test1 = "AX_t.(g_e -o X_t) -o X_t";
        String test2 = "AY_t.(h_e -o Y_t) -o Y_t";
        String test3 = "(g_e -o (h_e -o f_t))";
        String test9 = "((g_e -o (h_e -o f_t)) -o (g_e -o (h_e -o f_t)))";
        String test10 = "(h_e -o h_e)";

        String test4 = "(e -o f)";
        String test6 = "((e -o f) -o (e -o f))";
        String test7 = "e";

        String test5 = "(((a -o b) -o c) -o d)";
        String test8 = "((((a -o b) -o c) -o d) -o e)";

        System.out.println("Parsing input...");

        List<String> testquant = new ArrayList<>();
        List<String> testmod = new ArrayList<>();
        List<String> testnest = new ArrayList<>();

        // Test for quantifier premise
        testquant.add(test1);
        testquant.add(test2);
        testquant.add(test3);
        testquant.add(test9);
        testquant.add(test10);
        // Test for modifier premises
        testmod.add(test4);
        //testmod.add(test5);
        testmod.add(test6);
        testmod.add(test7);
        testnest.add(test5);
        testnest.add(test8);

        LinearLogicParser parser = new LinearLogicParser(testquant);
        System.out.println("Parsed terms: " + parser.premises.toString());

    }

     */

     public static void main(String[] args) throws ParserInputException {
         //Read input from console


            System.out.println("Enter a formula to parse:");
            Scanner scanner = new Scanner(System.in);

            GlueParser parser = new GlueParser();
            MeaningConstructor mc = parser.parseMeaningConstructor(scanner.nextLine());

             System.out.println("Parsed meaning constructor: " + mc.getSem().toString() + ", " + mc.getLlTerm().toString() + ", " + mc.isNonscope() + ", " + mc.isInsitu());



    }

}
