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


import glueSemantics.parser.SemanticParser;
import glueSemantics.semantics.SemanticRepresentation;
import main.Settings;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class SemAtom extends SemanticExpression {
    private String name;
    //private String value;
    //private AtomicType atomicType;
    private final SemSort sort;


    // Constructor for atomic type atom
    public SemAtom(SemSort sort, String name, SemType.AtomicType type) {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }

    // Constructor for higher type atom
    public SemAtom(SemSort sort, String name, SemType type) {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }

    public SemAtom(SemAtom a) {
        this.name = a.name;
        this.sort = a.sort;
        this.setType(a.getType());
    }


    public enum SemSort {
        VAR, CONST
    }


    @Override
    public SemanticExpression betaReduce() {
        return this;
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) {
        if (this == var)
            return arg;
        else
            return this;
    }

    // Moritz: we don't want to actually clone atoms. They need to be the same objects in copied
    // formulas so the bindings remain intact. As atoms aren't modified during runtime anyways this
    // is not a problem.
    @Override
    public SemanticExpression clone() {
        return this;
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        if (sort == SemSort.VAR)
        {
            return Collections.singleton(this);
        }
        return new HashSet<>();
    }

    public String getName() {
        return name;
    }
    public void setName(String name){this.name = name;}

    public SemSort getSort() {
        return sort;
    }
/*
    @Override
    public String toString() {
        if (LLProver2.getSettings().getSemanticOutputStyle() == Settings.PROLOG && getSort() == VAR) {
            return name.toUpperCase();
        } else {
            return name;
        }
    }
 */
    @Override
    public String toString()
    {
        if (SemanticParser.settings.getSemanticOutputStyle() == Settings.PROLOG) {
            if (getSort() == SemSort.VAR) {
                return name.toUpperCase();
            }
            else {
                return name.toLowerCase();
            }
        }
        else
        {
            return name;
        }

    }

    public String toStringTyped() {
        return name + "_" + getType().toString();
    }

}
