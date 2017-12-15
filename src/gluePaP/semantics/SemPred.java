package gluePaP.semantics;

import Prover.SemEquality;

import java.util.ArrayList;
import java.util.HashMap;

import static gluePaP.semantics.SemType.AtomicType.T;

public class SemPred extends SemRepresentation {

    private final String predForm;
    // Does a stack make sense here? We always want to have the same number of args!
    // Maybe a Hashmap is better
    private ArrayList<SemRepresentation> argList = new ArrayList<>();


    public SemPred(String predForm, SemRepresentation arg0) {
        this.predForm = predForm;
        argList.add(arg0);
        this.setType(T);
    }


    public SemPred(String predForm, SemRepresentation arg0, SemRepresentation arg1) {
        this.predForm = predForm;
        argList.add(arg0);
        argList.add(arg1);
        this.setType(T);
    }

    public SemPred(String predForm, SemRepresentation arg0, SemRepresentation arg1, SemRepresentation arg2) {
        this.predForm = predForm;
        argList.add(arg0);
        argList.add(arg1);
        argList.add(arg2);
        this.setType(T);
    }

    public SemPred(String predForm, ArrayList<SemRepresentation> args) {
        this.predForm = predForm;
        this.argList = args;
    }

    public SemPred(SemPred p) {
        this.predForm = p.predForm;
        this.argList = new ArrayList<>(p.argList);
        this.setType(T);
    }

    @Override
    public SemType getType() {
        return new SemType(T);
    }

    @Override
    public String toString() {
        return predForm + this.printArgs();
    }


    private String printArgs() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i = 0; i < argList.size(); i++) {
            sb.append(argList.get(i));
            if (i+1 < argList.size())
                sb.append(",");
        }
        sb.append(")");
        return sb.toString();
    }

    @Override
    public SemRepresentation betaReduce() {
        return this;
    }

    @Override
    public SemRepresentation applyTo(SemAtom var, SemRepresentation arg) {
        ArrayList<SemRepresentation> newArgs = new ArrayList<>(argList);
        for (int i = 0; i < newArgs.size(); i++) {
            if (newArgs.get(i) == var) {
                newArgs.set(i,arg);
            }
        }
        return new SemPred(this.predForm,newArgs);
    }

    @Override
    public SemRepresentation clone() {
        return new SemPred(this);
    }
}
