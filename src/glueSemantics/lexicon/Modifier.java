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
        LLAtom var = new LLAtom(identifier, new SemType(SemType.AtomicType.E), LLAtom.LLType.CONST,false);

        //generate semantics
        LLAtom rest = new LLAtom(identifier, new SemType(SemType.AtomicType.T), LLAtom.LLType.CONST,true);
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
