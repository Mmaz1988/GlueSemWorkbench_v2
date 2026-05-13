package webservice.rest.dtos;

import java.util.List;

public class GswbOutput {
    public List<GswbSolution> solutions;
    public String log;
    public Object derivation;
    public List<GswbDiscriminant> discriminants;

    public GswbOutput() {}

    public GswbOutput(List<GswbSolution> solutions, String log, Object derivation,  List<GswbDiscriminant> discriminants)
    {
        this.solutions = solutions;
        this.log = log;
        this.derivation = derivation;
        this.discriminants = discriminants;
    }
}
