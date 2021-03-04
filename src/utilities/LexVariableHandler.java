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

package utilities;

import java.util.*;

public abstract class LexVariableHandler {
    private static final int MAXINDEX = 100;

    private static Integer sentenceID;
    private static Integer connectorID;
    private static int senteceIDmax = 1000;

    public enum variableType{
        LLvar,
        LLatomT,
        LLatomE,
        SemVar,
        SemVarE,
        SemVarComp,
        SentenceID,
        connectorNode
/*
Possibly add more types, e.g. SemVarE, SemVarT etc.
*/

    }


    private static HashMap<variableType,List<String>> usedVariables = usedVars();


    //Instantiates memory for used vars
    private static HashMap usedVars() {
        HashMap<variableType, List<String>> usedVars = new HashMap<>();

        usedVars.put(variableType.LLvar,
                new ArrayList<String>());

        usedVars.put(variableType.LLatomT,
                new ArrayList<String>());

        usedVars.put(variableType.LLatomE,
                new ArrayList<String>());

        usedVars.put(variableType.SemVar,
                new ArrayList<String>());

        usedVars.put(variableType.SemVarE,
                new ArrayList<String>());

        usedVars.put(variableType.SemVarComp,
                new ArrayList<String>());

        return usedVars;

    }


    private static HashMap<variableType,List<String>> reservedVariables = reservedVars();


    private static HashMap reservedVars()
    {
        HashMap<variableType,List<String>> reservedVars = new HashMap<>();

        //Variables for linear logic
        reservedVars.put(variableType.LLvar,
                new ArrayList<String>(Arrays.asList("X","Y","Z")));


        //variables for the semantics of (partial) f-structures

        reservedVars.put(variableType.LLatomT,
                new ArrayList<String>(Arrays.asList("f")));
        reservedVars.put(variableType.LLatomE,
                new ArrayList<String>(Arrays.asList("g","h","i","j")));


        //Variables for Semantics; Entities
        reservedVars.put(variableType.SemVar,
                new ArrayList<String>(Arrays.asList("s","t","u","v")));

        //Variables for Semantics; Entities
        reservedVars.put(variableType.SemVarE,
                new ArrayList<String>(Arrays.asList("x","y","z")));

        //Variables for Semantics predicates of complex type
         reservedVars.put(variableType.SemVarComp,
                new ArrayList<String>(Arrays.asList("P","Q","R","S","T")));

        return reservedVars;
    }


    public static String returnNewVar(variableType varType)
    {
        if (varType.equals(variableType.connectorNode))
        {
            if (connectorID == null)
            {
                connectorID = 0;
            }
            else
            {
                connectorID++;
            }
            return "c" + connectorID;
        }

        if (varType.equals(variableType.SentenceID)) {

            if (sentenceID == null)
            {
                sentenceID = 0;
            }else{
                if (senteceIDmax >= sentenceID) {
                    sentenceID++;
                }else
                {
                    System.out.println("Exceeded maximum number of sentence variables");
                    return null;
                }

            }
            return "s" + sentenceID.toString();
        }

        List<String> variables = reservedVariables.get(varType);

            for (String var : variables)
            {
                if (!usedVariables.get(varType).contains(var))
                {
                    usedVariables.get(varType).add(var);
                    return var;
                }
            }

            int i = 1;
            //threshold for trying out new indices; can be set higher
            while (i < MAXINDEX) {

                for (String var : variables) {

                    String varPrime = var + i;
                    if (!usedVariables.get(varType).
                            contains(varPrime)) {
                        usedVariables.get(varType).add(varPrime);
                        return varPrime;
                    }
                }
                i++;
            }
        return null;
    }

    public static void resetVars()
    {
        usedVariables = usedVars();
    }


    //setters and Getters


    public static HashMap<variableType, List<String>> getUsedVariables() {
        return usedVariables;
    }

    public static void setUsedVariables(HashMap<variableType, List<String>> usedVariables) {
        LexVariableHandler.usedVariables = usedVariables;
    }


}
