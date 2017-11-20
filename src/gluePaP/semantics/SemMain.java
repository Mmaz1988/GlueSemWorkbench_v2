package gluePaP.semantics;

import Prover.LLProver;

import static gluePaP.semantics.SemAtom.SemSort.CONST;
import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class SemMain {
    public static void main(String[] args){

        System.out.println("Testing SemFunction:");
        SemAtom varP = new SemAtom(VAR,"P",new SemType(E,T));
        SemAtom varX = new SemAtom(VAR,"x",E);
        SemAtom varY = new SemAtom(VAR,"y",E);
        SemAtom varZ = new SemAtom(VAR,"z",E);
        SemFunction sleep = new SemFunction(varX,new SemPred("sleep",varX));
        SemFunction love = new SemFunction(varX,new SemFunction(varY,
                new SemPred("love",varX,varY)));
        SemFunction give = new SemFunction(varX,new SemFunction(varY,new SemFunction(varZ,
                new SemPred("give",varX,varY,varZ))));
        SemFunction obv = new SemFunction(varP,new SemPred("obviously",varP));
        obv.setArgument(sleep);
        System.out.println(obv.toString());
        System.out.println("Applying constant...");
        SemPred res = (SemPred) obv.betaReduce();
        System.out.println(res.toString());

        LLProver lp = new LLProver(null);
        SemRepresentation converted = lp.convertSemantics(give);
        System.out.println(converted.toString());

    }
}
