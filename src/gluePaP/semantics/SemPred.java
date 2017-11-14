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



}
