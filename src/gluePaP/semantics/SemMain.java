package gluePaP.semantics;

import Prover.LLProver;

import static gluePaP.semantics.BinaryTerm.SemOperator.AND;
import static gluePaP.semantics.SemAtom.SemSort.CONST;
import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class SemMain {
    public static void main(String[] args){

        System.out.println("Testing SemFunction:");
        SemAtom varP = new SemAtom(VAR,"P",new SemType(E,T));
        SemAtom varQ = new SemAtom(VAR,"Q",new SemType(E,T));
        SemAtom varX = new SemAtom(VAR,"x",E);
        SemAtom varY = new SemAtom(VAR,"y",E);
        SemAtom varZ = new SemAtom(VAR,"z",E);

        // Test case for a conjunction with two FuncApps
        FuncApp leftAnd = new FuncApp(varP,varX);
        FuncApp rightAnd = new FuncApp(varQ,varX);
        SemFunction and = new SemFunction(varP,new SemFunction(varQ,
                new SemFunction(varX, new BinaryTerm(leftAnd,AND,rightAnd))));
        SemFunction sleep = new SemFunction(varY,new SemPred("sleep",varY));

        SemRepresentation applied = new FuncApp(and,sleep).betaReduce();

        System.out.println(applied.toString());

    }
}
