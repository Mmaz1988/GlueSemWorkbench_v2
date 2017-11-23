package gluePaP.glue;

import java.util.*;

public abstract class LexVariableHandler {

    public enum variableType{
        LLvar,
        LLatomT,
        SemVar,
        SemVarE
/*
Possibly add more types, e.g. SemVarE, SemVarT etc.
*/

    }


    private static HashMap<variableType,List<String>> usedVariables = usedVars();


    //Instantiates memory for used vars
    private static HashMap usedVars() {
        HashMap<variableType, List<String>> reservedVars = new HashMap<>();

        reservedVars.put(variableType.LLvar,
                new ArrayList<String>());

        reservedVars.put(variableType.LLatomT,
                new ArrayList<String>());

        reservedVars.put(variableType.SemVar,
                new ArrayList<String>());

        reservedVars.put(variableType.SemVarE,
                new ArrayList<String>());

        return reservedVars;

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
                new ArrayList<String>(Arrays.asList("f","g","h","i")));

        //Variables for Semantics; Entities
        reservedVars.put(variableType.SemVarE,
                new ArrayList<String>(Arrays.asList("x","y","z")));

        return reservedVars;
    }


    public static String returnNewVar(variableType varType)
    {
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
            //threshold for trying out new indices; can be set higher?
            while (i < 3) {

                for (String var : variables) {

                    String varPrime = var + String.join("", Collections.nCopies(i, "'"));
                    if (!usedVariables.get(varType).
                            contains(varPrime)) {
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


}
