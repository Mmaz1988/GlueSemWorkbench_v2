package Prover;

import gluePaP.linearLogic.LLAtom;

public class AtomEquality {

    private final LLAtom variable;
    private final LLAtom constant;

    public AtomEquality(LLAtom variable, LLAtom constant)
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

}
