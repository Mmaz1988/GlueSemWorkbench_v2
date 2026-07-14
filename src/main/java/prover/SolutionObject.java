package prover;

import Discriminants.McDiscriminant;
import glueSemantics.linearLogic.Premise;

import java.util.LinkedHashMap;

import java.util.HashSet;
import java.util.LinkedHashSet;

public class SolutionObject {
    public String solutionId;
    public Premise solution;
    public HashSet<String> scopeDiscriminants = new HashSet<>();
    public HashSet<McDiscriminant> mcDiscriminants = new HashSet<>();
    public String solutionString;
    public LinkedHashSet<Integer> sourceIndices = new LinkedHashSet<>();
    public LinkedHashMap<String, Object> graph;


    public SolutionObject(Premise solution) {
        this.solution = solution;
        if (solution != null && solution.getSemTerm() != null) {
            this.sourceIndices.addAll(solution.getSemTerm().getSourceIndices());
        }
    }

    public SolutionObject(Premise solution, HashSet<String> discriminants) {
        this.solution = solution;
        this.scopeDiscriminants = discriminants;
        if (solution != null && solution.getSemTerm() != null) {
            this.sourceIndices.addAll(solution.getSemTerm().getSourceIndices());
        }
    }

}
