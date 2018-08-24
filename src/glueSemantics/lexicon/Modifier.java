/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.lexicon;

import glueSemantics.semantics.lambda.*;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;

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
