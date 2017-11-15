package gluePaP.semantics;

import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemAtom.SemType.E;

public class SemMain {
    public static void main(String[] args){

        System.out.println("Testing SemFunction:");
        SemAtom varX = new SemAtom(VAR,"x",E);
        SemPred pred1 = new SemPred("student",varX);
        SemFunction sem1 = new SemFunction(varX,pred1);
        System.out.println(sem1.toString());

    }
}
