package prover;

import glueSemantics.linearLogic.Premise;
import glueSemantics.linearLogic.Sequent;
import glueSemantics.semantics.MeaningConstructor;
import main.InputOutputProcessor;
import main.Settings;
import main.WorkbenchMain;
import utilities.Debugging;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

public abstract class LLProver {
    public Debugging db;
    private Settings settings;
    private LinkedList<Premise> solutions = new LinkedList<>();


    private final static Logger LOGGER = Logger.getLogger(WorkbenchMain.class.getName());

    public LLProver(){}

    public abstract void  deduce(Sequent seq) throws ProverException, VariableBindingException;

    public abstract Premise combinePremises(Premise functor, Premise argument, StringBuilder proofBuilder) throws VariableBindingException, ProverException;
    public abstract Premise combinePremises(Premise functor, Premise argument) throws VariableBindingException, ProverException;
    public LinkedList<Premise> getSolutions() {
        return solutions;
    }

    public void setSolutions(LinkedList<Premise> solutions) {
        this.solutions = solutions;
    }

    public Settings getSettings() {
        return this.settings;
    }

    public void setSettings(Settings settings) {
        this.settings = settings;
    }

    public static Logger getLOGGER() {
        return LOGGER;
    }

    public abstract StringBuilder getProofBuilder();

    public abstract void setProofBuilder(StringBuilder proofBuilder);

    public List<Premise> searchProof(Integer key, LinkedHashMap<Integer,List<MeaningConstructor>> lexicalEntries) throws VariableBindingException, ProverException {

        try {
        LOGGER.info(String.format("Found %d lexical entries for proof with id S%d",lexicalEntries.get(key).size(),key));

            Sequent testseq = new Sequent(lexicalEntries.get(key));

            deduce(testseq);
            List<Premise> result = new ArrayList<>(getSolutions());


            // LOGGER.info("Found the following deduction(s):\n");
            StringBuilder resultBuilder = new StringBuilder();



            StringBuilder solutionBuilder = new StringBuilder();
            solutionBuilder.append(String.format("Found the following solutions for proof with id S%d:\n",key));

                for (Premise p : result) {
                    solutionBuilder.append(InputOutputProcessor.restoreBackLinearLogicSide(p.toString()));

            }

                /*
                if(this.settings.isExplainFail() && this instanceof LLProver2)
                {
                    settings.setExplanation(failExplainer.explain( ((LLProver2) this).getNonAtomicChart(), ((LLProver2) this).getAtomicChart(),true));
                solutionBuilder.append("None!");
            }
                 */

            LOGGER.info(solutionBuilder.toString());


            //  LOGGER.info(String.format("Found %d solution(s) for derivation with id S%d",solutions.size(),key) );

            /*
            if (settings.isPartial()) {
                for (Premise part : prover.getDatabase())
                {
                    if (part.getPremiseIDs().size() > 1)
                    {
                        partial.add(part.toString());
                    }
                }

                for (Premise part : prover.getModifiers())
                {
                    if (part.getPremiseIDs().size() > 1)
                    {
                        partial.add(part.toString());
                    }
                }
            }

            */


            if (settings.isDebugging()) {
                LOGGER.info(String.format("Generated debugging report for proof with id S%d:\n" + db.toString(),key));
                LOGGER.info(String.format("Finished glue derivation of proof with id S%d.",key));
            }

            if (settings.getProverType() == 1 && settings.isVisualize())
            {
                //visualization.getContentPane().add(((LLProver1) prover).analysis.displayGraph());
                ((LLProver1) this).analysis.displayGraph();
                //    visualization.setVisible(true);
            }

            return result;


        } catch (ProverException e) {
            e.printStackTrace();
        }
    return null;
    }

}
