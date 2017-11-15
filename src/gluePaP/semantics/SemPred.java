package gluePaP.semantics;

import Prover.SemEquality;

import java.util.HashMap;

import static gluePaP.semantics.SemType.AtomicType.T;

public class SemPred extends SemRepresentation {

    String predForm;
    // Does a stack make sense here? We always want to have the same number of args!
    // Maybe a Hashmap is better
    HashMap<Integer,SemRepresentation> argList;

    public SemRepresentation getArg(int i) {
        return argList.get(i);
    }

    public SemPred(String predForm, SemRepresentation arg0) {
        this.predForm = predForm;
        this.argList = new HashMap<>();
        this.argList.put(1,arg0);
        this.setType(T);
    }


    public SemPred(String predForm, SemRepresentation arg0, SemRepresentation arg1) {
        this.predForm = predForm;
        this.argList = new HashMap<>();
        this.argList.put(0,arg0);
        this.argList.put(1,arg1);
    }

    @Override
    public String toString() {
        return predForm + this.printArgs();
    }

    private String printArgs() {
        String str = "(";
        for (int i = 1; i <= argList.size(); i++) {
            str = str + argList.get(i).toString();
        }
        return str + ")";
    }


    @Override
    public boolean applyTo(SemAtom var, SemRepresentation arg) {
        for (int i = 1; i <= argList.size(); i++) {
            if (argList.get(i) == var) {
                argList.put(i,arg);
                return true;
            }
        }
        return false;
    }
}
