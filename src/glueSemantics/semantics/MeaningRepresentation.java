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

package glueSemantics.semantics;

import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemType;

import java.util.HashSet;
import java.util.Set;

/**
 * A MeaningRepresentation is a generic semantic representation without any asociation
 * to a semantic theory or other Java classes that represent entities of a certain semantic
 * theory. This class is used to have simple string representations of formulae on the semantic
 * side of a premise.
 */

//TODO make to string plain if no Settings are available
public class MeaningRepresentation implements SemanticRepresentation{
    private final String formula;
    private SemType type;

    public MeaningRepresentation(String formula) {
        this.formula = formula;
        this.type = new SemType(SemType.AtomicType.T);
    }

    @Override
    public SemanticRepresentation betaReduce() {
        return this;
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg){
        return this;
    }

    @Override
    public SemType getType() {
        return this.type;
    }

    public void setType(SemType type) {this.type = type;}

    @Override
    public SemanticRepresentation clone() {
        return new MeaningRepresentation(this.formula);
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        return new HashSet<>();
    }

    @Override
    public String toString() {
        return formula;
    }
}
