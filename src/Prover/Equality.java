package Prover;

import gluePaP.linearLogic.LLAtom;

public class Equality {

    private final LLAtom variable;
    private final LLAtom constant;

    public Equality(LLAtom variable, LLAtom constant)
    {
        this.variable = variable;
        this.constant = constant;
    }


    public LLAtom getVariable() {
        return variable;
    }

    public LLAtom getConstant() {
        return constant;
    }


    @Override
    public String toString() {
        return variable.getName() + constant.getName();
    }
}
