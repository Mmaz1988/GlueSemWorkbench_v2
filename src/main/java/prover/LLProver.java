package prover;

import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import main.Settings;
import main.WorkbenchMain;
import utilities.Debugging;

import java.util.LinkedList;
import java.util.logging.Logger;

public abstract class LLProver {
    public Debugging db;
    private static Settings settings;
    private LinkedList<Premise> solutions = new LinkedList<>();


    private final static Logger LOGGER = Logger.getLogger(WorkbenchMain.class.getName());

    public LLProver(){}

    public abstract void  deduce(Sequent seq) throws ProverException, VariableBindingException;

    public abstract Premise combinePremises(Premise functor, Premise argument, StringBuilder proofBuilder) throws VariableBindingException, ProverException;

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

    public static Logger getLOGGER() {
        return LOGGER;
    }

    public abstract StringBuilder getProofBuilder();

    public abstract void setProofBuilder(StringBuilder proofBuilder);

}
