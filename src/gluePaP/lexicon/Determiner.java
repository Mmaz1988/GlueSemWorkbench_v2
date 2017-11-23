package gluePaP.lexicon;

import gluePaP.glue.LexVariableHandler;
import gluePaP.linearLogic.*;

public class Determiner extends LexicalEntry{
    public LexicalEntry.LexType lexType;

    enum QuantType
    {
        ALL,
        EX
    }

    public Determiner(String identifier)
    {
        this.lexType = LexType.DET;

        LexVariableHandler handler = new LexVariableHandler();

        String i = identifier + "_e";
        String var = handler.returnNewVar(LexVariableHandler.variableType.LLvar) + "_t";

        this.llFormula = "A" +var + ".(" + i + " -o " + var + ") -o " + var;


        /*This whole block generates the glue side of the determiner*/

        //binding variable of the quantifier
        LLAtom binder = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR);

        //Parts of the antecedent
        LLAtom argsem = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST,true);
        LLAtom left = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR,false);

        //consequent of the det
        LLAtom right = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR,true);

        //antecedent
        LLFormula ant = new LLFormula(argsem,new LLImplication(),left,false);

        LLFormula det = new LLFormula(ant,new LLImplication(),right,true, binder);

        System.out.println(det.toString());




    }

}

