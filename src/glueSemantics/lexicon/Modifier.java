package glueSemantics.lexicon;

import glueSemantics.glue.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.*;

public class Modifier extends LexicalEntry {
    public LexType lexType;


    public Modifier(String identifier,String lemma)
    {
        this.lexType = LexType.MOD;
        //identifier is provided by dependency in SentenceMeaning

        /*Linear Logic*/

        //generating consumer
        LLAtom var = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST,false);

        //generate semantics
        LLAtom rest = new LLAtom(identifier, LLTerm.Type.T, LLAtom.LLType.CONST,true);
        // TODO polarity and are the atoms the same?
        this.setLlTerm(new LLFormula(new LLFormula(var,rest,false ),new LLFormula(var,rest,true ),true));

        /*Semantics*/
        SemAtom modVar = new SemAtom(SemAtom.SemSort.VAR,
                //binding variable
                LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                SemType.AtomicType.E);

        SemAtom p = new SemAtom(SemAtom.SemSort.VAR,
                LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarComp),
                new SemType(SemType.AtomicType.E, SemType.AtomicType.T));

        SemPred modSem1 = new SemPred(lemma,modVar);
        FuncApp modSem2 = new FuncApp(p,modVar);
        BinaryTerm modBody = new BinaryTerm(modSem1,BinaryTerm.SemOperator.AND,modSem2);

        this.setSem(new SemFunction(p,new SemFunction(modVar,modBody)));



    }
}
