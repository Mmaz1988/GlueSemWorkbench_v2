/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;


import static glueSemantics.semantics.lambda.SemType.AtomicType.E;
import static glueSemantics.semantics.lambda.SemType.AtomicType.TEMP;

public class SemType {
    private SemType left;
    private SemType right;
    private AtomicType simple;

    // An enumeration of possible atomic types
    // TEMP is a special type used for variables introduced
    // during the compilation process
    public enum AtomicType {
        E,T,V,I,TEMP
    }

    public SemType getLeft() {
        return left;
    }

    public void setLeft(SemType left) {
        this.left = left;
    }

    public SemType getRight() {
        return right;
    }

    public void setRight(SemType right) {
        this.right = right;
    }

    public SemType(AtomicType simple) {
        this.simple = simple;
        this.left = null;
        this.right = null;
    }

    public SemType(SemType left, SemType right) {
        this.left = left;
        this.right = right;
    }

    public SemType(AtomicType left, AtomicType right) {
        this.left = new SemType(left);
        this.right = new SemType(right);
    }

    @Override
    public String toString() {
        if (left != null)
            return "<" + left.toString() + "," + right.toString() + ">";
        else {
            if (simple == E)
                return "e";
            else
                return "t";
        }
    }

    // Check if both types are matching simple types or if
    // their LHS and RHS match up (recursively).
    // If one of the variables is TEMP also return true
    public boolean equalsType(SemType t) {
        if (this.simple != null) {
            return t.simple != null && (this.simple.equals(t.simple)||this.simple == TEMP||t.simple == TEMP);
        }
        else
            return (this.left.equalsType(t.left) && this.right.equalsType(t.right));
    }
}