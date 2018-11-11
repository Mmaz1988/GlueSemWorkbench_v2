/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;


import glueSemantics.semantics.SemanticRepresentation;
import main.Settings;
import prover.LLProver;

import static glueSemantics.semantics.lambda.SemAtom.SemSort.VAR;

public class SemAtom extends SemanticExpression {
    private final String name;
    //private String value;
    //private AtomicType atomicType;
    private final SemSort sort;


    // Constructor for atomic type atom
    public SemAtom(SemSort sort, String name, SemType.AtomicType type)
    {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }

    // Constructor for higher type atom
    public SemAtom(SemSort sort, String name, SemType type)
    {
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

    public String getName() {
        return name;
    }

    public SemSort getSort() {
        return sort;
    }

    @Override
    public String toString() {
        if (LLProver.getSettings().getSemanticOutputStyle() == Settings.PROLOG && this.getSort() == VAR)
            return name.toUpperCase();
        else
            return name;
    }

    public String toStringTyped() {
        return name + "_" + getType();
    }

}
