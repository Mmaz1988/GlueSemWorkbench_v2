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
import prover.VariableBindingException;

import java.util.*;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GlueParser {
    private LinearLogicParser llparser = new LinearLogicParser();
    private SemanticParser semParser = new SemanticParser();
    public static final MeaningRepresentation emptyMeaning = new MeaningRepresentation("<empty>");
    // TODO add semantic parser here
    private final static Logger LOGGER = Logger.getLogger(GlueParser.class.getName());
    private boolean PARSESEMANTCS;

    public GlueParser() {
        this.PARSESEMANTCS = false;
    }

    public GlueParser(Boolean parseSemantics)
    {
        this.PARSESEMANTCS = parseSemantics;
    }

    public MeaningConstructor parseMeaningConstructor(String mc) throws ParserInputException {
        String[] mcList = mc.split(":");
        if (mcList.length != 2) {
            throw new ParserInputException("Error parsing formula '" + mc + "'. " +
                    "Meaning side and glue side need to be separated with a ':'");
        }
        MeaningConstructor entry = new MeaningConstructor();
        LLTerm glue = llparser.callParser(mcList[1].trim());
        SemanticRepresentation sem = null;
        if (!PARSESEMANTCS) {
            sem = new MeaningRepresentation(mcList[0].trim());
            //TODO
            //((MeaningRepresentation) sem).setType(glue.getType());
        } else
        {
           sem = semParser.parse(mcList[0].trim());

        }



        entry.setLlTerm(glue);
        entry.setSem(sem);

        return entry;
    }


    public LinkedHashMap<Integer,List<MeaningConstructor>> parseMeaningConstructorString(String mc) throws ParserInputException {
        List<String> formulas = Arrays.asList(mc.split("\n"));
        return parseMeaningConstructorList(formulas);
    }

    public LinkedHashMap<Integer,List<MeaningConstructor>> parseMeaningConstructorList(List<String> formulas) throws ParserInputException {

        //Split string into lines


        LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries = new LinkedHashMap<>();
        Integer sets = 0;
        Pattern wrapperStart = Pattern.compile("\\t*\\{\\t*");
        Pattern wrapperEnd = Pattern.compile("\\t*\\}\\t*");

        LOGGER.info("Now parsing input premises...");

        List<MeaningConstructor> ungroupedEntries = new ArrayList<>();

        for (int i = 0; i < formulas.size(); i++) {
            Matcher startMatcher = wrapperStart.matcher(formulas.get(i));

            if (startMatcher.matches()) {
                sets++;
                List<MeaningConstructor> currentLexicalEntries = new LinkedList<>();
                i++;
                Boolean newEntry = true;
                while (newEntry) {
                    Matcher endMatcher = wrapperEnd.matcher(formulas.get(i));

                    if (endMatcher.matches()) {
                        newEntry = false;
                        lexicalEntries.put(sets, currentLexicalEntries);

                        break;
                    }
                    try {
                        LOGGER.finer("Now parsing meaning constructor at position " + i + " in premise list...");
                        currentLexicalEntries.add(parseMeaningConstructor(formulas.get(i)));
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

        return lexicalEntries;
    }

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

}