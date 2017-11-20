package gluePaP.glue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class LexVariableHandler {

    public static List<String> usedVariables = new ArrayList<>();




    public static String returnNewVar()
    {
        List<String> variables = new ArrayList<>(Arrays.asList("Y","Z"));

        if (usedVariables.isEmpty())
        {
            usedVariables.add("X");
            return "X";

        } else
        {
            for (String var : variables)
            {
                if (!usedVariables.contains(var))
                {
                    usedVariables.add(var);
                    return var;
                }
            }
        }
        return null;
    }



    //setters and Getters
    public static List<String> getUsedVariables()
    {
        return usedVariables;
    }

    public static void setUsedVariables(List<String> usedVariables)
    {
        LexVariableHandler.usedVariables = usedVariables;
    }



}
