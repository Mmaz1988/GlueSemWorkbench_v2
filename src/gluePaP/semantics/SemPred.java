package gluePaP.semantics;

import Prover.SemEquality;

import java.util.HashMap;

import static gluePaP.semantics.SemType.AtomicType.T;

public class SemPred extends SemRepresentation {

    private String predForm;
    // Does a stack make sense here? We always want to have the same number of args!
    // Maybe a Hashmap is better
    private SemRepresentation[] argList;

    public SemRepresentation getArg(int i) {
        return argList[i];
    }

    public SemPred(String predForm, SemRepresentation arg0) {
        this.predForm = predForm;
        argList = new SemRepresentation[] {arg0};
        this.setType(T);
    }


    public SemPred(String predForm, SemRepresentation arg0, SemRepresentation arg1) {
        this.predForm = predForm;
        this.argList = new SemRepresentation[] {arg0,arg1};
        this.setType(T);
    }

    public SemPred(String predForm, SemRepresentation arg0, SemRepresentation arg1, SemRepresentation arg2) {
        this.predForm = predForm;
        this.argList = new SemRepresentation[] {arg0,arg1,arg2};
        this.setType(T);
    }

    @Override
    public String toString() {
        return predForm + this.printArgs();
    }

    private String printArgs() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i = 0; i < argList.length; i++) {
            sb.append(argList[i]);
            if (i+1 < argList.length)
                sb.append(", ");
        }
        sb.append(")");
        return sb.toString();
    }


    @Override
    public boolean applyTo(SemAtom var, SemRepresentation arg) {
        for (int i = 0; i <= argList.length; i++) {
            if (argList[i] == var) {
                argList[i] = arg;
                return true;
            }
        }
        return false;
    }
}
