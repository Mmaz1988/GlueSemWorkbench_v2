package webservice.rest.dtos;

import java.util.List;
import java.util.LinkedHashMap;

/**
 * Semantic deduction response. The solution list is retained for the legacy
 * UI, while semanticAnalyses and synSemMapping expose the analysis model.
 */
public class GswbOutput {
    public List<GswbSolution> solutions;
    public List<GswbSemanticAnalysis> semanticAnalyses;
    public LinkedHashMap<String, List<String>> synSemMapping;
    public String log;
    public Object derivation;
    public List<GswbDiscriminant> discriminants;

    public GswbOutput() {}

    public GswbOutput(List<GswbSolution> solutions, String log, Object derivation,  List<GswbDiscriminant> discriminants)
    {
        this.solutions = solutions;
        this.semanticAnalyses = solutions.stream()
                .map(solution -> solution.semanticAnalysis)
                .filter(java.util.Objects::nonNull)
                .toList();
        this.synSemMapping = new LinkedHashMap<>();
        solutions.forEach(solution -> solution.synSemMapping.forEach((syntaxId, semanticIds) ->
                this.synSemMapping.computeIfAbsent(syntaxId, ignored -> new java.util.ArrayList<>())
                        .addAll(semanticIds)));
        this.synSemMapping.replaceAll((syntaxId, semanticIds) -> semanticIds.stream().distinct().toList());
        this.log = log;
        this.derivation = derivation;
        this.discriminants = discriminants;
    }
}
