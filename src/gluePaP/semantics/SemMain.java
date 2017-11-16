package gluePaP.semantics;

import static gluePaP.semantics.SemAtom.SemSort.CONST;
import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class SemMain {
    public static void main(String[] args){

        System.out.println("Testing SemFunction:");
        SemAtom varP = new SemAtom(VAR,"P",new SemType(E,T));
        SemAtom varX = new SemAtom(VAR,"x",E);
        SemFunction arg1 = new SemFunction(varX,new SemPred("sleep",varX));
        SemFunction func1 = new SemFunction(varP,new SemPred("obviously",varP));
        func1.setArgument(arg1);
        System.out.println(func1.toString());
        System.out.println("Applying constant...");
        SemPred res = (SemPred) func1.betaReduce();
        System.out.println(res.toString());

    }
}
