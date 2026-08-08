package prover;

import Discriminants.McDiscriminant;
import glueSemantics.linearLogic.Premise;

import java.util.LinkedHashMap;

import java.util.HashSet;

public class SolutionObject {
    public String solutionId;
    public Premise solution;
    public HashSet<String> scopeDiscriminants = new HashSet<>();
    public HashSet<McDiscriminant> mcDiscriminants = new HashSet<>();
    public String solutionString;
    public String semantic;
    public Integer sourceIndex;
    public LinkedHashMap<String, Object> graph;
    public String proofId;
    public String solutionKey;
    public String mcSetId;
    public String sentenceId;


    public SolutionObject(Premise solution) {
        this.solution = solution;
        if (solution != null && solution.getSemTerm() != null) {
            this.sourceIndex = solution.getSemTerm().getSourceIndex();
        }
    }

    public SolutionObject(Premise solution, HashSet<String> discriminants) {
        this.solution = solution;
        this.scopeDiscriminants = discriminants;
        if (solution != null && solution.getSemTerm() != null) {
            this.sourceIndex = solution.getSemTerm().getSourceIndex();
        }
    }

}
