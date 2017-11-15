package gluePaP.semantics;

import java.util.HashMap;
import java.util.Stack;

public class SemPred extends SemRepresentation {

    String predForm;
    // Does a stack make sense here? We always want to have the same number of args!
    // Maybe a Hashmap is better
    HashMap<Integer,SemAtom> argList;
    //Stack<SemAtom> argList;

    @Override
    public SemAtom getArg(int i) {
        return argList.get(i);
    }

    public SemPred(String predForm, SemAtom arg0) {
        this.predForm = predForm;
        this.argList = new HashMap<>();
        this.argList.put(1,arg0);
    }


    public SemPred(String predForm, SemAtom arg0, SemAtom arg1) {
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
}
