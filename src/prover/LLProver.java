package prover;

import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import main.Settings;
import test.Debugging;

import java.util.LinkedList;

public abstract class LLProver {
    public Debugging db;
    private static Settings settings;
    private LinkedList<Premise> solutions = new LinkedList<>();

    public LLProver(){}

    public abstract void  deduce(Sequent seq) throws ProverException, VariableBindingException;


    public LinkedList<Premise> getSolutions() {
        return solutions;
    }

    public void setSolutions(LinkedList<Premise> solutions) {
        this.solutions = solutions;
    }

    public static Settings getSettings() {
        return settings;
    }

    public static void setSettings(Settings settings) {
        LLProver.settings = settings;
    }


}
