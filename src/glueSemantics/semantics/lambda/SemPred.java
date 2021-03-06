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

package glueSemantics.semantics.lambda;

import glueSemantics.semantics.SemanticRepresentation;
import prover.ProverException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import static glueSemantics.semantics.lambda.SemType.AtomicType.T;

public class SemPred extends SemanticExpression {

    private final String predForm;
    // Does a stack make sense here? We always want to have the same number of args!
    // Maybe a Hashmap is better
    private ArrayList<SemanticRepresentation> argList = new ArrayList<>();


    public SemPred(String predForm, SemanticRepresentation arg0) {
        this.predForm = predForm;
        argList.add(arg0);
        this.setType(T);
    }


    public SemPred(String predForm, SemanticRepresentation arg0, SemanticRepresentation arg1) {
        this.predForm = predForm;
        argList.add(arg0);
        argList.add(arg1);
        this.setType(T);
    }

    public SemPred(String predForm, SemanticRepresentation arg0, SemanticRepresentation arg1, SemanticRepresentation arg2) {
        this.predForm = predForm;
        argList.add(arg0);
        argList.add(arg1);
        argList.add(arg2);
        this.setType(T);
    }

    public SemPred(String predForm, ArrayList<SemanticRepresentation> args) {
        this.predForm = predForm;
        this.argList = args;
        this.setType(T);
    }

    public SemPred(SemPred p) {
        this.predForm = p.predForm;
        this.argList = new ArrayList<>(p.argList);
        this.setType(T);

    }

    @Override
    public SemType getType() {
        return new SemType(T);
    }

    @Override
    public String toString() {
        return predForm + this.printArgs();
    }


    private String printArgs() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i = 0; i < argList.size(); i++) {
            sb.append(argList.get(i).toString());
            if (i+1 < argList.size())
                sb.append(",");
        }
        sb.append(")");
        return sb.toString();
    }

    @Override
    public SemanticRepresentation betaReduce() {

        ArrayList<SemanticRepresentation> newArgs = new ArrayList<>();

        try{
            for (SemanticRepresentation arg : argList)
        {
               newArgs.add(arg.betaReduce());}
        }
        catch(Exception e)
        {
            System.out.println("Failed to beta reduce " + this.toString());
        }

        return new SemPred(predForm,newArgs);
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
        ArrayList<SemanticRepresentation> newArgs = new ArrayList<>(argList);
        for (int i = 0; i < newArgs.size(); i++) {
/*
            if (newArgs.get(i) == var) {
                newArgs.set(i,arg);
            }
 */
            SemanticRepresentation sr = newArgs.get(i).applyTo(var,arg);
            newArgs.set(i,sr);

        }
        return new SemPred(this.predForm,newArgs);
    }

    @Override
    public SemanticExpression clone() {
        return new SemPred(this);
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        Set<SemAtom> out = new HashSet<>();
        for (SemanticRepresentation sr : argList)
        {
            out.addAll(sr.findBoundVariables());
        }
        return out;
    }
}
