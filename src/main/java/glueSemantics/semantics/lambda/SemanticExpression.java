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


public abstract class SemanticExpression implements SemanticRepresentation {
    private SemType type;


    public SemanticExpression() {
    }

    public SemType getType() {
        return type;
    }

    public void setType(SemType.AtomicType type) {
        this.type = new SemType(type);
    }

    public void setType(SemType type) {
        this.type = type;
    }

    //public abstract SemanticRepresentation betaReduce();

    //public abstract SemanticRepresentation applyTo(SemAtom var, SemanticExpression arg);

    // This is not a regular clone() method, it just calls the copy constructor
    // of the respective class.
    public abstract SemanticExpression clone();


}
