package webservice.rest.dtos;

import java.util.List;

public class GswbPcdrsOutput {
    public SemanticModel semanticModel = SemanticModel.LFGXDRT;
    public String parentSolutionId;
    public String rootSolutionId;
    public List<GswbSolution> solutions;

    public GswbPcdrsOutput() {
    }

    public GswbPcdrsOutput(String parentSolutionId, List<GswbSolution> solutions) {
        this.parentSolutionId = parentSolutionId;
        this.solutions = solutions;
    }

    public GswbPcdrsOutput(SemanticModel semanticModel, String parentSolutionId,
                           String rootSolutionId, List<GswbSolution> solutions) {
        this(parentSolutionId, solutions);
        this.semanticModel = semanticModel;
        this.rootSolutionId = rootSolutionId;
    }
}
