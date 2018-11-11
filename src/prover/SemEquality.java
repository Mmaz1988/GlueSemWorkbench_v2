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

package prover;

import glueSemantics.semantics.lambda.SemAtom;

public class SemEquality {

    private final SemAtom variable;
    private final SemAtom constant;

    public SemEquality(SemAtom variable, SemAtom constant)
    {
        this.variable = variable;
        this.constant = constant;
    }


    public SemAtom getVariable() {
        return variable;
    }

    public SemAtom getConstant() {
        return constant;
    }


    @Override
    public String toString() {
        return variable.getName() +variable.getType() + constant.getName() + variable.getType();
    }



    // equals for this object yields true if within the constant and the variable name and type are equal
    @Override
    public boolean equals(Object b)
    {


        if (!(b instanceof SemEquality)) {
            return false;
        }
        if (b == this) {
            return true;
        }

        SemEquality eq = (SemEquality) b;

        return eq.variable.getName().equals(this.variable.getName()) &&
                eq.variable.getType().equals(this.variable.getType()) &&
                eq.constant.getName().equals(this.constant.getName()) &&
                eq.constant.getType().equals(this.constant.getType());

    }


    //used by equals() to determine similarity between elements relevant for equals()
    @Override
    public int hashCode(){
        int result = 17;
        result = 31 * result + this.variable.getName().hashCode();
        result = 31 * result + this.variable.getType().hashCode();
        result = 31 * result + this.constant.getName().hashCode();
        result = 31 * result + this.constant.getType().hashCode();
        return result;
    }

}
