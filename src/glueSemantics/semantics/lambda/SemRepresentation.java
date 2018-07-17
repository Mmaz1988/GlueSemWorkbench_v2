/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;


public abstract class SemRepresentation {
    private SemType type;

    public SemRepresentation() {
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

    public abstract SemRepresentation betaReduce();

    public abstract SemRepresentation applyTo(SemAtom var, SemRepresentation arg);

    // This is not a regular clone() method, it just calls the copy constructor
    // of the respective class.
    public abstract SemRepresentation clone();



}
