/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics;

import glueSemantics.semantics.lambda.SemType;
import prover.ProverException;

/**
 * A MeaningRepresentation is a generic semantic representation without any asociation
 * to a semantic theory or other Java classes that represent entities of a certain semantic
 * theory. This class is used to have simple string representations of formulae on the semantic
 * side of a premise.
 */
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

    @Override
    public SemanticRepresentation clone() {
        return new MeaningRepresentation(this.formula);
    }

    @Override
    public String toString() {
        return formula;
    }
}
