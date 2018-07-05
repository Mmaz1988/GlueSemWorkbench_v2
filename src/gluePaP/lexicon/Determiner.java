package gluePaP.lexicon;

import gluePaP.glue.LexVariableHandler;
import gluePaP.linearLogic.*;
import gluePaP.semantics.*;

import java.util.HashMap;
import java.util.List;

import static gluePaP.semantics.BinaryTerm.SemOperator.AND;
import static gluePaP.semantics.SemQuantEx.SemQuant.EX;

public class Determiner extends LexicalEntry{
    public LexicalEntry.LexType lexType;


    private static HashMap<String,String> scope;

    //TODO I'd rather have a seperate system in place for this.
    private static void  setScope(String role)
    {
        String var = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomE);
        if (scope == null)
        {
        scope = new HashMap<>();

        }
        scope.put(role,var);

    }

    enum QuantType
    {
        ALL,
        EX
    }

    public Determiner(String identifier, String detType, String role)
    {
        //Part of determining scope relations; kind of hacky
        setScope(role);

        this.lexType = LexType.DET;
        this.identifier = identifier;

        String var = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLvar);

        //Restrictor

        LLAtom subjsem = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST,true);

        LLAtom restr = new LLAtom(identifier, LLTerm.Type.T, LLAtom.LLType.CONST,false);
        //LexVariableHandler.addUsedVariable(LexVariableHandler.variableType.LLatomT,identifier);

        LLFormula restrSem = new LLFormula(subjsem,restr,false);

        //Scope
        LLAtom scopeConst = new LLAtom(scope.get(role), LLTerm.Type.E, LLAtom.LLType.CONST,false);

        //Identifier for the semantics of the whole things
        String detVar = LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLvar);

        // for the resource that is consumed
        LLAtom scopeSem = new LLAtom(detVar,
                        LLTerm.Type.T, LLAtom.LLType.VAR,false);

        LLFormula scope = new LLFormula(scopeConst,scopeSem,false);

        // for the resulting resource
        LLAtom detRes = new LLAtom(detVar,
                LLTerm.Type.T, LLAtom.LLType.VAR,true);


        LLFormula scopeComplete = new LLFormula(scope,detRes,true,detRes);

        LLFormula detSem = new LLFormula(restrSem,scopeComplete,true);

        this.setLlTerm(detSem);


        /*Linear Logic*/

        //binding variable of the quantifier
        LLAtom binder = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR);

        //Parts of the antecedent
       LLAtom argsem = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST,true);
        LLAtom left = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR,false);

        //consequent of the det
        LLAtom right = new LLAtom(var, LLTerm.Type.T, LLAtom.LLType.VAR,true);

        //antecedent
        LLFormula ant = new LLFormula(argsem,left,false);

        //wrapping in consequent
        LLFormula det = new LLFormula(ant,right,true, binder);

       // this.llTerm = det;


        /*Semantics*/

        // p = restr
        SemAtom p = new SemAtom(SemAtom.SemSort.VAR,
                LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarComp),
                new SemType(SemType.AtomicType.E, SemType.AtomicType.T));

        // q = scope
        SemAtom q = new SemAtom(SemAtom.SemSort.VAR,
                LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarComp),
                new SemType(SemType.AtomicType.E, SemType.AtomicType.T));

        if (detType.toLowerCase().equals("a"))
        {

            //binder variable
            SemAtom semBinder = new SemAtom(SemAtom.SemSort.VAR,
                    LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                    SemType.AtomicType.E);

            FuncApp restrFA = new FuncApp(p,semBinder);
            FuncApp scopeFA = new FuncApp(q,semBinder);
            SemFunction ex = new SemFunction(p,new SemFunction(q, new SemQuantEx(SemQuantEx.SemQuant.EX,
                    semBinder, new BinaryTerm(restrFA,BinaryTerm.SemOperator.AND,scopeFA))));

            this.setSem(ex);
            /*
            // Test case for a conjunction with two FuncApps
            FuncApp leftAnd = new FuncApp(varP,varX);
            FuncApp rightAnd = new FuncApp(varQ,varX);
            SemFunction and = new SemFunction(varP,new SemFunction(varQ,
                    new SemQuantEx(EX,varX, new BinaryTerm(leftAnd,AND,rightAnd))));
            SemFunction sleep = new SemFunction(varY,new SemPred("sleep",varY));

             SemRepresentation applied = new FuncApp(and,sleep).betaReduce();


           */
        }
        else if (detType.toLowerCase().equals("every"))
        {


            //binder variable
            SemAtom semBinder = new SemAtom(SemAtom.SemSort.VAR,
                    LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                    SemType.AtomicType.E);


            FuncApp restrFA = new FuncApp(p,semBinder);
            FuncApp scopeFA = new FuncApp(q,semBinder);
            SemFunction uni = new SemFunction(p,new SemFunction(q, new SemQuantEx(SemQuantEx.SemQuant.UNI,
                    semBinder, new BinaryTerm(restrFA, BinaryTerm.SemOperator.IMP,scopeFA))));

            this.setSem(uni);

        }


        /*Semantics*/
    }

    public static HashMap getterScope()
    {
    return Determiner.scope;
    }
    // Setter and Getter
    public static String getScope(String role)
    {
        return scope.get(role);
    }



}

